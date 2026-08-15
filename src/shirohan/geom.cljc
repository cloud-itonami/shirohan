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
  （測らずに黙って通すよりは、粗くても報告する方を採る）。

  **120 に抑えてある。** 400 だと 1 輪郭あたり 16 万回の点-線分距離になり、
  髪の束のように輪郭が 20 本出る図案では検査だけでブラウザが数十秒止まる
  （実測 2026-08-01: 白版を 768px で追ったとき全体で 29 秒。うち大半がここ）。

  この検査は**粗い警告**であって寸法の保証ではない（doc のとおり頂点でしか
  測らない近似）。粗さを上げても見逃す側に倒れるだけで、通してはいけないものを
  通すようにはならない —— 精度より、**実用的な時間で必ず走ること**を採る。"
  120)

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

(defn- knot-dt
  "向心 Catmull-Rom の節点間隔 Δt = |Δp|^0.5。一様 (Δt=1) は辺が長短混じると
  短い辺の先で必ず張り出す（実測 2026-08-14: 人物シルエットの p90 が
  Illustrator 5° に対して 9° 残った）。等間隔なら一様と同じ制御点になる。"
  [a b]
  (let [dx (- (double (first b)) (double (first a)))
        dy (- (double (second b)) (double (second a)))
        d (sqrt (+ (* dx dx) (* dy dy)))]
    (sqrt (max 1e-12 d))))

(defn- tangent
  "頂点 i の Hermite 接線。**角では 0** —— そこだけ直線になる。

  m_i = (p_{i+1} - p_{i-1}) / (t_{i+1} - t_{i-1})。t は向心パラメータ。"
  [points corner-set i]
  (if (corner-set i)
    [0.0 0.0]
    (let [n (count points)
          p0 (nth points (mod (+ i -1 n) n))
          p1 (nth points i)
          p2 (nth points (mod (inc i) n))
          dt (max eps (+ (knot-dt p0 p1) (knot-dt p1 p2)))
          [x0 y0] p0
          [x2 y2] p2]
      [(/ (- x2 x0) dt) (/ (- y2 y0) dt)])))

(defn curve->d
  "点列 + 角の添字 → 3 次ベジェの path データ。

  向心 Catmull-Rom を 3 次ベジェへ: 区間 p1→p2 の制御点は
  `c1 = p1 + m1·Δt/3`、`c2 = p2 - m2·Δt/3`。**点を必ず通る**。ラスタ輪郭の
  出力は `approx-curve->d`（点を通らない）。ここは補間の契約を残す。

  角の検出は `shirohan.curve`。ここは**引くだけ** —— 検出は形状の性質、
  こちらは出力の形式なので、層を分けてある。"
  [points corner-set]
  (let [n (count points)
        cs (or corner-set #{})]
    (when (>= n 3)
      (str "M" (fmt (first (nth points 0))) " " (fmt (second (nth points 0)))
           (apply str
                  (map (fn [i]
                         (let [[x1 y1] (nth points i)
                               [x2 y2] (nth points (mod (inc i) n))
                               dt (knot-dt (nth points i) (nth points (mod (inc i) n)))
                               [m1x m1y] (tangent points cs i)
                               [m2x m2y] (tangent points cs (mod (inc i) n))]
                           (str "C" (fmt (+ x1 (/ (* m1x dt) 3.0))) " "
                                (fmt (+ y1 (/ (* m1y dt) 3.0)))
                                " " (fmt (- x2 (/ (* m2x dt) 3.0))) " "
                                (fmt (- y2 (/ (* m2y dt) 3.0)))
                                " " (fmt x2) " " (fmt y2))))
                       (range n)))
           "Z"))))

(defn- v+ [[ax ay] [bx by]] [(+ ax bx) (+ ay by)])
(defn- v- [[ax ay] [bx by]] [(- ax bx) (- ay by)])
(defn- v* [[ax ay] s] [(* ax s) (* ay s)])
(defn- vdot [[ax ay] [bx by]] (+ (* ax bx) (* ay by)))
(defn- vlen [[ax ay]] (sqrt (+ (* ax ax) (* ay ay))))
(defn- vdist [a b] (vlen (v- a b)))

(defn- vunit [v]
  (let [n (vlen v)]
    (if (< n eps) [0.0 0.0] [(/ (first v) n) (/ (second v) n)])))

(defn- bezier [p0 p1 p2 p3 t]
  (let [u (- 1.0 t)
        u2 (* u u)
        t2 (* t t)]
    (v+ (v+ (v* p0 (* u2 u)) (v* p1 (* 3.0 u2 t)))
        (v+ (v* p2 (* 3.0 u t2)) (v* p3 (* t2 t))))))

(defn- bezier-prime [p0 p1 p2 p3 t]
  (let [u (- 1.0 t)]
    (v+ (v+ (v* (v- p1 p0) (* 3.0 u u))
            (v* (v- p2 p1) (* 6.0 u t)))
        (v* (v- p3 p2) (* 3.0 t t)))))

(defn- chord-ts [pts]
  (let [n (count pts)
        acc (loop [i 1 s 0.0 out [0.0]]
              (if (>= i n)
                out
                (let [s' (+ s (vdist (nth pts (dec i)) (nth pts i)))]
                  (recur (inc i) s' (conj out s')))))
        total (peek acc)]
    (if (< total eps)
      (mapv #(/ (double %) (max 1 (dec n))) (range n))
      (mapv #(/ % total) acc))))

(defn- line-cubic [a b]
  (let [d (v- b a)]
    [a (v+ a (v* d (/ 1.0 3.0))) (v+ a (v* d (/ 2.0 3.0))) b]))

(defn- end-tangents [pts]
  (let [n (count pts)
        t1 (vunit (v- (nth pts 1) (nth pts 0)))
        t2 (vunit (v- (nth pts (- n 2)) (nth pts (dec n))))]
    [(if (and (zero? (first t1)) (zero? (second t1)))
       (vunit (v- (nth pts (dec n)) (nth pts 0)))
       t1)
     (if (and (zero? (first t2)) (zero? (second t2)))
       (vunit (v- (nth pts 0) (nth pts (dec n))))
       t2)]))

(defn- generate-bezier [pts ts t-hat1 t-hat2]
  (let [n (count pts)
        p0 (nth pts 0)
        p3 (nth pts (dec n))
        A (mapv (fn [t]
                  (let [u (- 1.0 t)]
                    [(v* t-hat1 (* 3.0 u u t))
                     (v* t-hat2 (* 3.0 u t t))]))
                ts)
        C00 (reduce + 0.0 (map (fn [a] (vdot (nth a 0) (nth a 0))) A))
        C01 (reduce + 0.0 (map (fn [a] (vdot (nth a 0) (nth a 1))) A))
        C11 (reduce + 0.0 (map (fn [a] (vdot (nth a 1) (nth a 1))) A))
        [x0 x1] (loop [i 0 s0 0.0 s1 0.0]
                  (if (>= i n)
                    [s0 s1]
                    (let [t (nth ts i)
                          u (- 1.0 t)
                          tmp (v- (nth pts i)
                                  (v+ (v* p0 (* u u u)) (v* p3 (* t t t))))
                          a (nth A i)]
                      (recur (inc i)
                             (+ s0 (vdot (nth a 0) tmp))
                             (+ s1 (vdot (nth a 1) tmp))))))
        det (- (* C00 C11) (* C01 C01))
        chord (max eps (vdist p0 p3))
        cap (* 2.0 chord)
        a1 (if (> (abs* det) 1e-12)
             (/ (- (* x0 C11) (* x1 C01)) det)
             (/ chord 3.0))
        a2 (if (> (abs* det) 1e-12)
             (/ (- (* x1 C00) (* x0 C01)) det)
             (/ chord 3.0))
        a1 (if (and (pos? a1) (< a1 cap)) a1 (/ chord 3.0))
        a2 (if (and (pos? a2) (< a2 cap)) a2 (/ chord 3.0))]
    [p0 (v+ p0 (v* t-hat1 a1)) (v+ p3 (v* t-hat2 a2)) p3]))

(defn- max-error [pts ts cubic]
  (let [[p0 p1 p2 p3] cubic]
    (loop [i 1 best 0.0 best-i 1]
      (if (>= i (dec (count pts)))
        [best best-i]
        (let [d (vdist (nth pts i) (bezier p0 p1 p2 p3 (nth ts i)))]
          (if (> d best)
            (recur (inc i) d i)
            (recur (inc i) best best-i)))))))

(defn- reparameterize [pts ts cubic]
  (let [[p0 p1 p2 p3] cubic]
    (mapv (fn [t p]
            (let [q (bezier p0 p1 p2 p3 t)
                  qp (bezier-prime p0 p1 p2 p3 t)
                  num (vdot (v- q p) qp)
                  den (vdot qp qp)]
              (if (< den 1e-12)
                t
                (max 0.0 (min 1.0 (- t (/ num den)))))))
          ts pts)))

(defn- vneg [[ax ay]] [(- ax) (- ay)])

(defn- closed-fwd-tangent [pts i]
  (let [n (count pts)
        a (nth pts (mod (+ i -1 n) n))
        b (nth pts (mod (inc i) n))
        t (vunit (v- b a))]
    (if (and (zero? (first t)) (zero? (second t)))
      (vunit (v- (nth pts (mod (inc i) n)) (nth pts i)))
      t)))

(defn- newton-fit [pts th1 th2]
  (let [ts0 (chord-ts pts)
        cubic0 (generate-bezier pts ts0 th1 th2)]
    (loop [k 0 ts ts0 cubic cubic0]
      (if (>= k 3)
        [cubic ts]
        (let [ts' (reparameterize pts ts cubic)
              cubic' (generate-bezier pts ts' th1 th2)]
          (recur (inc k) ts' cubic'))))))

(defn- fit-span
  "折れ線 span（両端を含む）を許容 `tol` の 3 次ベジェ列へ。点は通らない。
  分割点では前後の接線を共有して G1 にする（角は呼び出し側が span を切る）。"
  ([pts tol] (fit-span pts tol 0 nil nil))
  ([pts tol depth t1 t2]
   (let [n (count pts)
         pv (vec pts)]
     (cond
       (< n 2) []
       (= n 2) [(line-cubic (nth pv 0) (nth pv 1))]
       :else
       (let [et (end-tangents pv)
             th1 (if (and t1 (> (vlen t1) eps)) (vunit t1) (nth et 0))
             th2 (if (and t2 (> (vlen t2) eps)) (vunit t2) (nth et 1))
             [cubic ts] (newton-fit pv th1 th2)
             [err _idx] (max-error pv ts cubic)]
         (if (or (<= err tol) (> depth 14))
           [cubic]
           (let [split (quot n 2)
                 fwd (let [t (vunit (v- (nth pv (inc split)) (nth pv (dec split))))]
                       (if (and (zero? (first t)) (zero? (second t)))
                         (vunit (v- (nth pv (inc split)) (nth pv split)))
                         t))
                 left (subvec pv 0 (inc split))
                 right (subvec pv split n)]
             (if (or (< (count left) 2) (< (count right) 2))
               [cubic]
               (into (fit-span left tol (inc depth) th1 (vneg fwd))
                     (fit-span right tol (inc depth) fwd th2))))))))))

(defn- span-pts [points i j]
  (let [pv (vec points)
        n (count pv)]
    (if (< i j)
      (subvec pv i (inc j))
      (into (subvec pv i n) (subvec pv 0 (inc j))))))

(defn- farthest-idx [pts i]
  (let [p (nth pts i)
        n (count pts)
        j (apply max-key (fn [k] (vdist p (nth pts k))) (range n))]
    (if (= j i) (mod (+ i (quot n 2)) n) j)))

(defn- vang
  "2 ベクトルのなす角（度）。"
  [u v]
  (let [nu (vlen u) nv (vlen v)]
    (if (or (< nu eps) (< nv eps))
      0.0
      (let [c (/ (vdot u v) (* nu nv))
            c (max -1.0 (min 1.0 c))]
        #?(:clj (* 57.29577951308232 (Math/acos c))
           :cljs (* 57.29577951308232 (js/Math.acos c)))))))

(defn- straight-cubic?
  "ハンドルが弦上にある＝折れ線の 1 辺を立方で書いたもの。"
  [[p0 p1 p2 p3]]
  (and (< (point-seg-dist p1 p0 p3) 0.05)
       (< (point-seg-dist p2 p0 p3) 0.05)))

(defn- mergeable-straights?
  [a b tol]
  (let [p0 (nth a 0)
        pv (nth a 3)
        p3 (nth b 3)]
    (and (< (vdist pv (nth b 0)) 1e-6)
         (straight-cubic? a)
         (straight-cubic? b)
         (<= (point-seg-dist pv p0 p3) tol)
         (< (vang (v- pv p0) (v- p3 pv)) 50.0))))

(defn- merge-straight-cubics
  "隣接する直線立方を、接合点の矢高が tol 以下かつ接合角 < 50° のとき弦に繋ぐ。
  角（90°）は残す。長い弧は straight ではないので触れない。"
  [cubics tol]
  (reduce (fn [acc b]
            (if (and (seq acc) (mergeable-straights? (peek acc) b tol))
              (conj (pop acc) (line-cubic (nth (peek acc) 0) (nth b 3)))
              (conj acc b)))
          [] cubics))

(defn- g1-smooth-joins
  "n=2 直線立方の列は接合が折れ線のまま残る（flatten の p90 が接合角）。
  角未満（< 50°）の直線立方どうしだけ、接合の両ハンドルを二等分線へ揃えて G1 にする。
  判定は元の立方で行い、1 本の両端を別の接合が触っても潰さない。"
  [cubics]
  (let [cv (vec cubics)
        n (count cv)]
    (if (< n 2)
      cv
      (reduce
       (fn [acc i]
         (let [a (nth cv i)
               j (mod (inc i) n)
               b (nth cv j)
               p0 (nth a 0)
               pv (nth a 3)
               p3 (nth b 3)
               u (v- pv p0)
               v (v- p3 pv)
               ja (vang u v)]
           (if (and (straight-cubic? a)
                    (straight-cubic? b)
                    (< (vdist pv (nth b 0)) 1e-6)
                    (> (vlen u) eps)
                    (> (vlen v) eps)
                    (> ja 8.0)
                    (< ja 50.0))
             (let [T (vunit (v+ (vunit u) (vunit v)))
                   ha (min 0.4 (/ (vlen u) 3.0))
                   hb (min 0.4 (/ (vlen v) 3.0))
                   a' (assoc (vec (nth acc i)) 2 (v- pv (v* T ha)))
                   b' (assoc (vec (nth acc j)) 1 (v+ pv (v* T hb)))]
               (-> acc (assoc i a') (assoc j b')))
             acc)))
       cv (range n)))))

(defn- lerp
  ([a b] (v* (v+ a b) 0.5))
  ([a b t] (v+ a (v* (v- b a) t))))

(defn- split-cubic
  "de Casteljau で t=1/2。曲線は変わらない。短い立方にすると flatten の 12 点/立方が
  Illustrator と同じ密度になる（Gold は 519 本、こちらは split 前 186 本）。"
  [[p0 p1 p2 p3]]
  (let [a (lerp p0 p1)
        b (lerp p1 p2)
        d (lerp p2 p3)
        e (lerp a b)
        f (lerp b d)
        g (lerp e f)]
    [[p0 a e g] [g f d p3]]))

(defn- split-long-cubics
  "直線立方（角の辺）は割らない。G1 した弧だけ弦 1mm 超を 1 回割る。"
  [cubics]
  (mapcat (fn [c]
            (if (and (not (straight-cubic? c))
                     (> (vdist (nth c 0) (nth c 3)) 1.0))
              (split-cubic c)
              [c]))
          cubics))

(defn- approx-tol [points]
  "対角の 0.12%。mm に拡縮した人物（対角 ~210mm）で ≈0.25mm ≈ 1px。
  床を 0.75 にすると ~3px 張り出して IoU が 0.96 まで落ちた（実測 2026-08-15）。"
  (let [xs (map first points)
        ys (map second points)
        dx (- (apply max xs) (apply min xs))
        dy (- (apply max ys) (apply min ys))
        diag (sqrt (+ (* dx dx) (* dy dy)))]
    (max 0.2 (* 0.0012 diag))))

(defn approx-curve->d
  "点列 + 角 → 3 次ベジェ。**中間点は通らない**（許容は対角の 0.1%）。

  向心 Catmull-Rom は点を必ず通るので、iso の 15–45° zigzag を flatten すると
  p90 が 9° に残る（実測 2026-08-15）。Illustrator は同じ折れをハンドルで避ける。
  階段頂点への Schneider ではない —— DP 済みの iso 折れ線に、角と角の間だけ
  最小二乗の立方を載せる。角では区間が切れ、G0 の尖りは残る。"
  ([points corner-set] (approx-curve->d points corner-set {}))
  ([points corner-set {:keys [tol]}]
   (let [n (count points)
         cs (or corner-set #{})
         tol (or tol (approx-tol points))]
     (when (>= n 3)
       (let [idxs (vec (sort (filter (fn [i] (and (number? i) (>= i 0) (< i n))) cs)))
             ;; 始点は常に points[0]（拡縮後の M が先頭点と一致する契約）。
             ;; 角が無い閉曲線は直径で 2 分割する —— 始点=終点の 1 span だと
             ;; 弦が 0 になり最小二乗が潰れる。
             cubics (split-long-cubics
                     (g1-smooth-joins
                     (merge-straight-cubics
                     (if (seq idxs)
                      (let [corner-at? (set idxs)
                            idxs (if (zero? (first idxs)) idxs (into [0] idxs))
                            nxt (conj (subvec idxs 1) (first idxs))]
                        (mapcat (fn [a b]
                                  (let [t1 (when-not (corner-at? a) (closed-fwd-tangent points a))
                                        t2 (when-not (corner-at? b) (vneg (closed-fwd-tangent points b)))]
                                    (fit-span (vec (span-pts points a b)) tol 0 t1 t2)))
                                idxs nxt))
                      (let [j (farthest-idx points 0)
                            t0 (closed-fwd-tangent points 0)
                            tj (closed-fwd-tangent points j)]
                        (into (fit-span (vec (span-pts points 0 j)) tol 0 t0 (vneg tj))
                              (fit-span (vec (span-pts points j 0)) tol 0 tj (vneg t0)))))
                     tol)))]
         (when (seq cubics)
           (let [[x0 y0] (nth (first cubics) 0)]
             (str "M" (fmt x0) " " (fmt y0)
                  (apply str
                         (map (fn [[_p0 p1 p2 p3]]
                                (str "C" (fmt (first p1)) " " (fmt (second p1))
                                     " " (fmt (first p2)) " " (fmt (second p2))
                                     " " (fmt (first p3)) " " (fmt (second p3))))
                              cubics))
                  "Z"))))))))

(defn contour->d
  "輪郭 1 本を SVG の path データにする。閉じていなければ Z を打たない。

  `:corners` **キーがある**輪郭はベジェで出す（中身が空でも）。iso 折れ線を
  向心 Catmull-Rom で通ると flatten p90 が 9° に残るので、角と角の間は
  `approx-curve->d`（許容 ≈ 対角 0.1%）で当てる。角が 0 個なのは「尖らせる
  場所が無い」であって「曲線にしない」ではない。空集合を `(seq corners)` で
  折ると、そこだけ `L` の階段に戻る（実測 2026-08-14）。

  SVG 由来の輪郭は `:corners` を持たないので、従来どおり折れ線のまま出す。

  **焼いた path 文字列は持たせない。** 角の添字は拡縮・平行移動・オフセットの
  どれでも変わらないが、座標は変わる —— 文字列を持たせると、その後の変換の
  たびに黙って古い座標のまま残る（実測 2026-08-01: ラスタ経路で mm へ拡縮した
  あとも画素座標の path が出ていた）。**引くのは最後**。"
  [{:keys [points closed? corners] :as c}]
  (cond
    (and (contains? c :corners) (not= closed? false) (>= (count points) 3))
    (approx-curve->d points (or corners #{}))

    (seq points)
    (str "M" (str/join "L" (map (fn [[x y]] (str (fmt x) " " (fmt y))) points))
         (when (not= closed? false) "Z"))))

(defn contours->d
  "輪郭群を 1 本の path データに連結する（サブパスとして並ぶ）。"
  [contours]
  (str/join " " (keep contour->d contours)))
