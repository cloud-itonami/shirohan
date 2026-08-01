(ns shirohan.geom
  "輪郭（closed contour）の幾何。**描画をしない純関数の層**で、`shirohan.plate`
  （版を組む）と `shirohan.svg`（文字列を吐く）とブラウザの canvas が同じ結果を
  食う。

  ## 単位は mm

  版は実世界の寸法で語られる（choke 0.2mm、最小線幅 0.3mm、見当ずれ ±0.5mm）ので
  内部座標も mm。ピクセル化はレンダラの仕事。

  ## 座標系

  原点は左上、x は右、y は下（SVG と同じ）。この向きだと shoelace の符号と
  「時計回り」の見た目が入れ替わるが、**この ns はどこにも「時計回り」と書かない**
  —— 符号だけを見て法線の向きを決めるので、y の向きに依存しない。

  ## 輪郭は自己交差を解かない

  ここが持つ `offset` は**頂点法線オフセット（miter 継ぎ）** で、offset 量が
  形状の特徴幅に対して十分小さいことを前提にする。δ が特徴幅の半分を超えると
  輪郭は自己交差し、面が裏返る。版下の choke は 0.1〜0.5mm で、Tシャツの図案
  に対しては十分小さいのでこれで足りる —— が、**足りない場合を黙って通さない**
  ために `min-feature-width` を用意してあり、`shirohan.plate` はそれで
  `:thin-feature` を報告する。ちゃんとした Straight Skeleton は持っていない。"
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------- math

(defn- sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))
(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- mn [a b] (if (< a b) a b))
(defn- mx [a b] (if (> a b) a b))

(def ^:private eps 1e-9)

;; ---------------------------------------------------------------- contour

;; 輪郭は `{:points [[x y] …] :closed? bool :fill "#rrggbb" :role :art|:knockout}`。
;; `:points` は**先頭を末尾に繰り返さない**（閉じているかは `:closed?` が持つ）。

(defn- dedupe-points
  "連続する同一点を落とす。長さ 0 の辺があると法線が定義できない。"
  [pts]
  (let [out (reduce (fn [acc p]
                      (let [l (peek acc)]
                        (if (and l
                                 (< (abs* (- (double (first p)) (double (first l)))) 1e-7)
                                 (< (abs* (- (double (second p)) (double (second l)))) 1e-7))
                          acc
                          (conj acc [(double (first p)) (double (second p))]))))
                    [] pts)]
    ;; 閉輪郭では末尾＝先頭も重複なので落とす。
    (if (and (> (count out) 1)
             (< (abs* (- (first (peek out)) (first (first out)))) 1e-7)
             (< (abs* (- (second (peek out)) (second (first out)))) 1e-7))
      (pop out)
      out)))

(defn normalize-contour
  "点列を掃除する。潰れて 3 点未満になったら nil（面を持たない）。"
  [c]
  (let [pts (dedupe-points (:points c))]
    (when (>= (count pts) 3)
      (assoc c :points pts))))

(defn signed-area
  "shoelace。符号は輪郭の向き、絶対値は面積（mm²）。"
  [{:keys [points]}]
  (let [n (count points)]
    (if (< n 3)
      0.0
      (* 0.5
         (double
          (reduce + 0.0
                  (map (fn [i]
                         (let [[x1 y1] (nth points i)
                               [x2 y2] (nth points (mod (inc i) n))]
                           (- (* x1 y2) (* x2 y1))))
                       (range n))))))))

(defn area [c] (abs* (signed-area c)))

(defn orientation
  "向きの符号。1 か -1（面積 0 のときは 1）。"
  [c]
  (if (neg? (signed-area c)) -1 1))

(defn bbox
  "輪郭群の外接矩形 `{:x0 :y0 :x1 :y1}`。空なら nil。"
  [contours]
  (let [pts (mapcat :points contours)]
    (when (seq pts)
      (reduce (fn [b [x y]]
                {:x0 (mn (:x0 b) x) :y0 (mn (:y0 b) y)
                 :x1 (mx (:x1 b) x) :y1 (mx (:y1 b) y)})
              (let [[x y] (first pts)] {:x0 x :y0 y :x1 x :y1 y})
              pts))))

(defn bbox-union [a b]
  (cond (nil? a) b
        (nil? b) a
        :else {:x0 (mn (:x0 a) (:x0 b)) :y0 (mn (:y0 a) (:y0 b))
               :x1 (mx (:x1 a) (:x1 b)) :y1 (mx (:y1 a) (:y1 b))}))

;; ---------------------------------------------------------------- offset

(defn- edge-normals
  "各辺 i（points[i] → points[i+1]）の**外向き**単位法線。

  外向きの決め方は輪郭の向きの符号だけで、y 軸の向きに依存しない
  （shoelace が正なら (dy, -dx) が外向き、負ならその反対）。"
  [points sign]
  (let [n (count points)]
    (mapv (fn [i]
            (let [[x1 y1] (nth points i)
                  [x2 y2] (nth points (mod (inc i) n))
                  dx (- x2 x1) dy (- y2 y1)
                  len (sqrt (+ (* dx dx) (* dy dy)))]
              (if (< len eps)
                [0.0 0.0]
                [(* sign (/ dy len)) (* sign (- (/ dx len)))])))
          (range n))))

(defn offset
  "輪郭を δ mm だけオフセットする。**δ > 0 で囲む面が広がり、δ < 0 で縮む。**

  頂点 i は「辺 i-1 と辺 i を外向きに δ 動かした 2 直線の交点」に置く（miter）。
  鋭角では交点が無限に遠ざかるので `miter-limit`（既定 4、SVG と同じ）で
  `|δ| * limit` に丸める —— 丸めないと針のように飛び出した頂点が版に出る。"
  ([c delta] (offset c delta {}))
  ([{:keys [points] :as c} delta {:keys [miter-limit] :or {miter-limit 4.0}}]
   (if (or (< (count points) 3) (< (abs* delta) 1e-12))
     c
     (let [n (count points)
           sign (orientation c)
           normals (edge-normals points sign)
           cap (* (abs* delta) miter-limit)
           moved (mapv
                  (fn [i]
                    (let [[px py] (nth points i)
                          [n1x n1y] (nth normals (mod (+ (dec i) n) n)) ; 入る辺
                          [n2x n2y] (nth normals i)                     ; 出る辺
                          mx0 (+ n1x n2x) my0 (+ n1y n2y)
                          mlen (sqrt (+ (* mx0 mx0) (* my0 my0)))]
                      (if (< mlen eps)
                        ;; 180 度折り返し。miter が定義できないので辺法線で逃がす。
                        [(+ px (* delta n2x)) (+ py (* delta n2y))]
                        (let [ux (/ mx0 mlen) uy (/ my0 mlen)
                              dot (+ (* ux n1x) (* uy n1y))
                              raw (if (< (abs* dot) 1e-6) cap (/ delta dot))
                              len (cond (> raw cap) cap
                                        (< raw (- cap)) (- cap)
                                        :else raw)]
                          [(+ px (* len ux)) (+ py (* len uy))]))))
                  (range n))]
       (assoc c :points moved)))))

;; ---------------------------------------------------------------- 細り検査

(defn- point-seg-dist [[px py] [ax ay] [bx by]]
  (let [dx (- bx ax) dy (- by ay)
        l2 (+ (* dx dx) (* dy dy))]
    (if (< l2 eps)
      (sqrt (+ (* (- px ax) (- px ax)) (* (- py ay) (- py ay))))
      (let [t0 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2)
            t (cond (< t0 0.0) 0.0 (> t0 1.0) 1.0 :else t0)
            qx (+ ax (* t dx)) qy (+ ay (* t dy))]
        (sqrt (+ (* (- px qx) (- px qx)) (* (- py qy) (- py qy))))))))

(def ^:private max-probe-points
  "細り検査は O(n²)。頂点がこれを超える輪郭は等間隔に間引いてから測る
  （測らずに黙って通すよりは、粗くても報告する方を採る）。"
  400)

(defn- probe-points [points]
  (let [n (count points)]
    (if (<= n max-probe-points)
      points
      (let [step (/ (double n) max-probe-points)]
        (mapv #(nth points (min (dec n) (int (* % step)))) (range max-probe-points))))))

(defn min-feature-width
  "輪郭の**最小特徴幅**の近似（mm）。各頂点から、隣接しない辺までの最短距離の最小値。

  「この輪郭を δ 縮めたら消える部分があるか」を δ*2 と比べて判定するための値。
  厳密な medial axis ではない —— 頂点でしか測らないので、辺の途中がいちばん
  細い形（長い辺どうしが平行に近づく場合）は過大評価する。過大評価＝見逃す側
  なので、`shirohan.plate` は choke 値そのものとの比較も併せて出す。"
  [{:keys [points]}]
  (let [pts (probe-points points)
        n (count pts)]
    (if (< n 4)
      ##Inf
      (reduce
       (fn [best i]
         (let [p (nth pts i)]
           (reduce
            (fn [b j]
              ;; 隣接辺（i-1, i）は自分自身なので除く。
              (if (or (= j i) (= j (mod (+ (dec i) n) n)))
                b
                (let [d (point-seg-dist p (nth pts j) (nth pts (mod (inc j) n)))]
                  (if (< d b) d b))))
            best (range n))))
       ##Inf (range n)))))

;; ---------------------------------------------------------------- 内外判定

(defn inside?
  "点が輪郭の内側か（ray casting、even-odd）。"
  [{:keys [points]} [x y]]
  (let [n (count points)]
    (loop [i 0 j (dec n) in? false]
      (if (= i n)
        in?
        (let [[xi yi] (nth points i)
              [xj yj] (nth points j)
              cross? (not= (> yi y) (> yj y))
              hit? (and cross?
                        (< x (+ xi (/ (* (- xj xi) (- y yi)) (+ (- yj yi) eps)))))]
          (recur (inc i) i (if hit? (not in?) in?)))))))

(defn centroid [{:keys [points]}]
  (let [n (count points)]
    [(/ (reduce + 0.0 (map first points)) n)
     (/ (reduce + 0.0 (map second points)) n)]))

;; ---------------------------------------------------------------- 出力補助

(defn fmt
  "SVG に書く数値。小数3桁で丸め、`1.0` は `1` にする（差分が読みやすい）。"
  [x]
  (let [r (/ #?(:clj (Math/round (* (double x) 1000.0))
                :cljs (js/Math.round (* x 1000.0)))
             1000.0)
        s (str r)]
    (if (str/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)))

(defn contour->d
  "輪郭 1 本を SVG の path データにする。閉じていなければ Z を打たない。

  `:curve-d` を持つ輪郭（`shirohan.curve/fit` を通したもの）は**そちらを使う**
  —— ラスタから起こした輪郭は折れ線のままだと拡大時に必ず角張るので、曲線に
  当てはめた結果を持たせてある。"
  [{:keys [points closed? curve-d]}]
  (cond
    (and curve-d (not= closed? false)) curve-d
    (seq points)
    (str "M" (str/join "L" (map (fn [[x y]] (str (fmt x) " " (fmt y))) points))
         (when (not= closed? false) "Z"))))

(defn contours->d
  "輪郭群を 1 本の path データに連結する（サブパスとして並ぶ）。"
  [contours]
  (str/join " " (keep contour->d contours)))
