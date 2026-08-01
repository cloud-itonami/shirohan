(ns shirohan.cut
  "カットライン（断裁線）—— アクリルスタンド・ステッカーの外形を切る線。

  ## 何を作るのか

  図案の**インクが載る面から `:cut-margin-mm` 外側**の閉じた輪郭。いただいた
  Illustrator の 3 レイヤー構成（カットライン / cmyk / 白版）の 1 枚目に当たる。

  ```
  ┌───────────────┐  ← カットライン（外側 3mm）
  │  ┌─────────┐  │
  │  │ 白版    │  │  ← 白版（内側 0.1mm）
  │  │  ┌───┐  │  │
  │  │  │図案│  │  │
  ```

  ## 単純なオフセットでは作れない

  外側 3mm は白版の 0.1mm と桁が違う。頂点法線オフセット（`shirohan.geom/offset`）
  は δ が特徴幅に対して小さいことを前提にしているので、3mm 外へ出すと

  - **離れた部品が繋がらない**（髪と手が 4mm 離れていたら別々の線のまま）
  - 角が針のように飛び出す
  - 自己交差して裏返る

  カットラインに要るのは逆で、**近い部品は 1 本の外形にまとまり、角は丸く、
  小さすぎる穴は塞がる**（切り抜けないので）。それは「インク面を半径 r の円で
  膨らませた領域の境界」そのもの ——  形態学の膨張（dilation）。

  ## だからラスタで解く

  インク面を作業解像度のマスクに焼き、**チャンファー距離変換**で各画素の
  インクまでの距離を出し、`距離 <= r` を閾値にして境界を追う。この方法なら

  - 部品の合流・角の丸み・穴の消滅が**全部同じ 1 つの演算から出る**
  - 多角形のブール演算器が要らない（この repo が持たないと決めているもの）

  代償は精度が作業解像度で決まること。既定 `:cut-px-per-mm 2`（0.5mm 刻み）は
  3mm のオフセットに対して十分で、ブラウザの SCI でも実用的な速さで終わる。
  精度が要るなら上げられるが、**画素数の2乗で重くなる**ことは doc に書いておく。"
  (:require [shirohan.geom :as geom]
            [shirohan.raster :as raster]))

(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))

;; ---------------------------------------------------------------- ラスタ化

(defn- edges-of [contours]
  (into []
        (mapcat (fn [{:keys [points]}]
                  (let [n (count points)]
                    (keep (fn [i]
                            (let [[x1 y1] (nth points i)
                                  [x2 y2] (nth points (mod (inc i) n))]
                              (when (not= y1 y2) [x1 y1 x2 y2])))
                          (range n)))))
        contours))

(defn- rasterize
  "輪郭群 → 0/1 のマスク（nonzero 巻き数則、走査線 1 本ずつ）。

  `shirohan.psd/coverage` と同じ走査線塗り。psd から呼ばないのは、カットラインが
  PSD 書き出しに依存する理由が無いため（依存の向きを増やさない）。"
  [contours w h scale]
  (let [es (edges-of contours)]
    (persistent!
     (reduce
      (fn [acc py]
        (let [wy (/ (+ py 0.5) scale)
              xs (->> es
                      (keep (fn [[x1 y1 x2 y2]]
                              (when (or (and (<= y1 wy) (< wy y2))
                                        (and (<= y2 wy) (< wy y1)))
                                [(+ x1 (* (- x2 x1) (/ (- wy y1) (- y2 y1))))
                                 (if (< y1 y2) 1 -1)])))
                      (sort-by first))
              spans (loop [v xs wind 0 start nil out []]
                      (if-let [[x d] (first v)]
                        (let [w2 (+ wind d)]
                          (cond (and (zero? wind) (not (zero? w2))) (recur (next v) w2 x out)
                                (and (not (zero? wind)) (zero? w2)) (recur (next v) w2 nil
                                                                           (conj out [start x]))
                                :else (recur (next v) w2 start out)))
                        out))]
          (reduce (fn [a px]
                    (let [wx (/ (+ px 0.5) scale)]
                      (conj! a (if (some (fn [[a0 b0]] (and (<= a0 wx) (< wx b0))) spans) 1 0))))
                  acc (range w))))
      (transient []) (range h)))))

;; ---------------------------------------------------------------- 距離変換

(def ^:private big 1000000)

(defn distance-map
  "各画素からインク面までの距離（画素単位）。2 パスのチャンファー変換。

  斜め方向を √2 ではなく **1.41** で近似する古典的な 3×4 チャンファー ——
  誤差は数 % で、カットラインの丸みには十分。厳密なユークリッド距離変換
  （Felzenszwalb）を持ち込む理由が無い。"
  [mask w h]
  (let [d0 (transient (mapv #(if (pos? %) 0.0 (double big)) mask))
        ;; 前方（左上→右下）
        fwd (loop [i 0 d d0]
              (if (>= i (* w h))
                d
                (let [x (mod i w) y (quot i w)
                      cand (cond-> [(nth d i)]
                             (> x 0) (conj (+ 1.0 (nth d (dec i))))
                             (> y 0) (conj (+ 1.0 (nth d (- i w))))
                             (and (> x 0) (> y 0)) (conj (+ 1.41 (nth d (- i w 1))))
                             (and (< x (dec w)) (> y 0)) (conj (+ 1.41 (nth d (- i w -1)))))]
                  (recur (inc i) (assoc! d i (apply min cand))))))
        ;; 後方（右下→左上）
        bwd (loop [i (dec (* w h)) d fwd]
              (if (neg? i)
                d
                (let [x (mod i w) y (quot i w)
                      cand (cond-> [(nth d i)]
                             (< x (dec w)) (conj (+ 1.0 (nth d (inc i))))
                             (< y (dec h)) (conj (+ 1.0 (nth d (+ i w))))
                             (and (< x (dec w)) (< y (dec h))) (conj (+ 1.41 (nth d (+ i w 1))))
                             (and (> x 0) (< y (dec h))) (conj (+ 1.41 (nth d (+ i w -1)))))]
                  (recur (dec i) (assoc! d i (apply min cand))))))]
    (persistent! bwd)))

;; ---------------------------------------------------------------- 入口

(def default-opts
  {:cut-margin-mm 3.0    ; インク面から外側へ出す量。アクリル・ステッカーの定番
   :cut-px-per-mm 2.0    ; 作業解像度。上げると画素数の2乗で重くなる
   :simplify-mm 0.3})    ; 追った境界を均す量

(defn cut-line
  "インクが載る面 → カットラインの輪郭群。

  `contours` は白版の元（`shirohan.plate` の underbase-art）。返り値は
  `{:contours [...] :margin-mm m}`。輪郭が 1 本も無ければ `:contours []`。

  **穴の扱い**: 直径が `2×margin` に満たない穴は膨張で塞がる。これは仕様 ——
  3mm のオフセットで残る穴は刃が入らないので、残す方が嘘になる。"
  ([contours size] (cut-line contours size {}))
  ([contours {:keys [width-mm height-mm]} opts]
   (let [{:keys [cut-margin-mm cut-px-per-mm simplify-mm]} (merge default-opts opts)]
     (if (empty? contours)
       {:contours [] :margin-mm cut-margin-mm}
       (let [scale cut-px-per-mm
             ;; マージンぶん外へ出るので、キャンバスを両側に広げておく
             pad (int (+ 2 (* cut-margin-mm scale)))
             w (+ (int (* width-mm scale)) (* 2 pad))
             h (+ (int (* height-mm scale)) (* 2 pad))
             shifted (mapv (fn [c] (update c :points
                                           #(mapv (fn [[x y]]
                                                    [(+ x (/ pad scale)) (+ y (/ pad scale))]) %)))
                           contours)
             mask (rasterize shifted w h scale)
             dm (distance-map mask w h)
             r (* cut-margin-mm scale)
             inside? (fn [i] (<= (nth dm i) r))
             idx (mapv #(if (inside? %) 0 -1) (range (* w h)))
             traced (keep (fn [pts]
                            (let [simple (raster/douglas-peucker
                                          (mapv (fn [[x y]] [(- (/ x scale) (/ pad scale))
                                                             (- (/ y scale) (/ pad scale))]) pts)
                                          simplify-mm)]
                              (geom/normalize-contour {:points simple :closed? true})))
                          (raster/chain-edges (raster/boundary-edges idx w h #(= 0 %))))]
         {:contours (vec traced) :margin-mm cut-margin-mm})))))
