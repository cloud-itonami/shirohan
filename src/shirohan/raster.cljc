(ns shirohan.raster
  "ラスタ画像（PNG/JPEG を展開した RGBA 画素）を**版に使える輪郭**に変換する。

  「画像を上げるだけで白版ができる」の実体はここ。LLM は使わない —— 幾何は
  決定論でなければ、承認した版と刷った版が同じであることを示せない
  （判断＝色数・どれを白抜きにするか等だけを `shirohan.advice` が LLM に出す）。

  ## 手順

  ```
  RGBA画素 ─▶ ①メディアンカット量子化 ─▶ ②最近傍で色番号に割付
           ─▶ ③色ごとの2値マスク ─▶ ④クラック追跡で輪郭を出す
           ─▶ ⑤Douglas–Peucker で簡略化 ─▶ ⑥小さすぎる島を捨てる ─▶ 輪郭
  ```

  ④の**クラック追跡**は、内側画素と外側画素の境目（画素の辺）をそのまま辿る方法。
  マーチングスクエアと違って交点の補間が要らず、出てくるのは必ず閉じた矩形折れ線に
  なる。向きは「内側が常に同じ側に来る」ように辺の向きを決めてあるので、外周と穴は
  自動的に逆向きになり、`shirohan.plate` の入れ子判定がそのまま効く。

  ## 解像度は上げない（性能ではなく、正直さの問題）

  ブラウザでは SCI（scittle）がこの `.cljc` を**インタプリタで**実行する。
  1000×1000 の画素を Clojure のループで回すと実用にならないので、**呼び出し側が
  長辺 `:max-side`（既定 192px）に縮小してから渡す**。

  これは「速いから縮める」のではない。この道具が扱うのは**ロゴ・イラスト系の
  少数色の図案**で、その用途では 192px から起こした輪郭を Douglas–Peucker で
  均した精度が版に十分見合う。写真から版を起こすのは本来ハーフトーン（網点）の
  仕事で、輪郭追跡でやるべきではない —— なので**写真は「向いていない」と報告する**
  （`:photographic-source`）。黙って粗い版を出さない。"
  (:require [shirohan.geom :as geom]))

(defn- sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))
(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- rnd [x] #?(:clj (Math/round (double x)) :cljs (js/Math.round x)))

(defn- hex2 [n]
  (let [i (max 0 (min 255 (int n)))
        s #?(:clj (Integer/toHexString i) :cljs (.toString i 16))]
    (if (= 1 (count s)) (str "0" s) s)))

(defn rgb->hex [[r g b]] (str "#" (hex2 r) (hex2 g) (hex2 b)))

;; ---------------------------------------------------------------- 画素の取り出し

(defn byte-reader
  "`data` の i 番目を返す関数を作る。

  `canvas.getImageData().data` は **`Uint8ClampedArray`** で、`nth` を呼ぶと
  `nth not supported on this type Uint8ClampedArray` で落ちる（実測 2026-08-01、
  ブラウザで画像を読ませて初めて出た）。JVM の vector と typed array の両方を
  受けるために、**境界で 1 回だけ**アクセサを決める。

  判定に `satisfies? IIndexed` を使わないこと —— **scittle(SCI) は cljs の
  プロトコル記号を持たない**ので `Could not resolve symbol: IIndexed` で
  ページ全体が止まる（同日実測。JVM のテストも nbb の煙試験も通ったのに、
  ブラウザで初めて出た）。`sequential?` なら SCI にもある。"
  [data]
  #?(:clj (fn [i] (nth data i))
     :cljs (if (sequential? data)
             (fn [i] (nth data i))
             (fn [i] (aget data i)))))

(defn- px
  "RGBA の i 番目の画素。`rd` は `byte-reader` が作ったアクセサ。"
  [rd i]
  (let [o (* i 4)]
    [(rd o) (rd (+ o 1)) (rd (+ o 2)) (rd (+ o 3))]))

(defn samples
  "量子化に使う標本。全画素を舐めず、**等間隔で最大 `n` 点**だけ取る
  （量子化は分布さえ取れれば足りる。全画素を見るのは④の割付で1回だけ）。"
  [{:keys [width height data]} n alpha-min]
  (let [rd (byte-reader data)
        total (* width height)
        step (max 1 (int (/ total (max 1 n))))]
    (into []
          (comp (map #(px rd %))
                (filter #(>= (nth % 3) alpha-min))
                (map #(subvec % 0 3)))
          (range 0 total step))))

;; ---------------------------------------------------------------- ①メディアンカット

(defn- box-of [pixels]
  (let [ch (fn [i] (map #(nth % i) pixels))
        rng (fn [i] (let [c (ch i)] (- (apply max c) (apply min c))))]
    {:pixels pixels
     :ranges [(rng 0) (rng 1) (rng 2)]}))

(defn- split-box
  "いちばん広がっている軸の**平均**で切る。

  素のメディアンカットは中央値で切るが、それは画素数が偏った図案で壊れる ——
  青 288 画素の中に白 36 画素の穴がある絵では、中央値が青の塊の**中**に落ちて、
  青を2つに割っただけのパレットになる（白が消える。実測 2026-08-01）。ロゴや
  イラストは「面積の大きい色と小さい色」が同居するのが普通なので、双峰分布を
  分けられる平均分割を採る。平均で片側が空になる（全部同じ値）ときだけ中央値に
  落とす。"
  [{:keys [pixels ranges]}]
  (let [axis (first (apply max-key second (map-indexed vector ranges)))
        sorted (vec (sort-by #(nth % axis) pixels))
        n (count sorted)
        mean (/ (reduce + 0.0 (map #(nth % axis) sorted)) n)
        at-mean (count (take-while #(< (nth % axis) mean) sorted))
        cut (if (and (pos? at-mean) (< at-mean n)) at-mean (quot n 2))]
    (when (and (pos? cut) (< cut n))
      [(box-of (subvec sorted 0 cut)) (box-of (subvec sorted cut))])))

(defn- representative
  "箱を代表する 1 色。**平均ではなく最頻色**を採る。

  平均だと、輪郭のアンチエイリアス（縁に無数に出る中間色）が代表色を引っ張る。
  実測 2026-08-01: ブラウザの canvas で縮小した赤+黄のロゴが、平均だと
  `#e85039`（赤と黄の混色）1 色に潰れた —— 同じ図案を混色なしで作った
  JVM の試験では `#e11d48` / `#facc15` と正しく出ていたので、**混色が入る
  実データでしか出ないバグ**だった。

  フラットな塗りの図案では、箱の中の圧倒的多数が「その色ちょうど」なので、
  最頻色は**元の色をそのまま**返す。縁の中間色は票が割れて勝てない。
  同数のときは辞書順で決める（決定論を保つため）。"
  [pixels]
  (let [f (frequencies pixels)
        best (apply max (vals f))]
    (first (sort (keep (fn [[c n]] (when (= n best) c)) f)))))

(defn median-cut
  "標本 → 最大 k 色のパレット。**決定論的**（乱数を使う k-means と違い、同じ画像
  からは必ず同じパレットが出る）。

  返すのは `distinct` を掛けたあとの色 —— 図案の色数が k より少ないとき、素の
  median-cut は分けられない箱を分け続けて**同じ色を複数返す**。同じ色の版を
  2 枚作っても意味が無いので落とす（結果として「4 色でと言ったのに 3 版しか
  出ない」ことがあるが、それは図案が 3 色だったという事実）。"
  [pixels k]
  (if (empty? pixels)
    []
    (loop [boxes [(box-of pixels)]]
      (if (>= (count boxes) k)
        (vec (distinct (map #(representative (:pixels %)) boxes)))
        ;; 次に切る箱は **画素数 × 広がり** で選ぶ。
        ;;
        ;; 広がりだけで選ぶと、アンチエイリアスの縁が作る「色は散らばっているが
        ;; 画素は少ない箱」が、実在する2色を抱えた濃い箱より先に割られる。実測
        ;; 2026-08-01: 赤の円 + 黄の帯 + 白の穴（縁に混色 42 色）で、赤+黄の箱
        ;; （2416 標本・広がり 175）より 赤〜白の混色の箱（396 標本・広がり 165）が
        ;; 優先され、赤+黄が最後まで割られずに **黄（全体の 26%）が代表色に負けて
        ;; 消えた**。混色を含まない合成画像では再現せず、実データで初めて出た。
        (let [idx (first (apply max-key
                                #(let [b (second %)]
                                   (* (count (:pixels b)) (apply max (:ranges b))))
                                (map-indexed vector boxes)))
              parts (split-box (nth boxes idx))]
          (if (nil? parts)
            (vec (distinct (map #(representative (:pixels %)) boxes)))
            (recur (into (into (subvec boxes 0 idx) parts)
                         (subvec boxes (inc idx))))))))))

(defn- nearest [palette [r g b]]
  (loop [i 0 best 0 bd #?(:clj Double/MAX_VALUE :cljs js/Number.MAX_VALUE)]
    (if (>= i (count palette))
      best
      (let [[pr pg pb] (nth palette i)
            d (+ (* (- r pr) (- r pr)) (* (- g pg) (- g pg)) (* (- b pb) (- b pb)))]
        (if (< d bd) (recur (inc i) i d) (recur (inc i) best bd))))))

;; ---------------------------------------------------------------- ④クラック追跡

(defn- boundary-edges
  "色番号 `want` の2値マスクの境界辺を、**向き付き**で集める。

  内側画素の、外側と接する辺だけを**向き付きで**集め、終点＝始点で鎖にする。
  向きの決め方（画像座標は y が下）:

  ```
  上が外  (x+1,y)   → (x,y)
  左が外  (x,y)     → (x,y+1)
  下が外  (x,y+1)   → (x+1,y+1)
  右が外  (x+1,y+1) → (x+1,y)
  ```

  こう決めると単独画素は shoelace が負になり、その内側にできる穴は正になる ——
  外周と穴が自動的に逆向きになる。"
  [idx width height member?]
  (let [inside? (fn [x y]
                  (and (>= x 0) (>= y 0) (< x width) (< y height)
                       (member? (nth idx (+ x (* y width))))))
        edges (persistent!
               (reduce
                (fn [acc i]
                  (let [x (mod i width) y (quot i width)]
                    (if-not (member? (nth idx i))
                      acc
                      (cond-> acc
                        (not (inside? x (dec y))) (assoc! [(inc x) y] [x y])
                        (not (inside? (dec x) y)) (assoc! [x y] [x (inc y)])
                        (not (inside? x (inc y))) (assoc! [x (inc y)] [(inc x) (inc y)])
                        (not (inside? (inc x) y)) (assoc! [(inc x) (inc y)] [(inc x) y])))))
                (transient {}) (range (* width height))))]
    ;; 同じ始点から出る辺は最大1本（上の4方向は始点が全部違う）ので map で足りる。
    edges))

(defn- chain-edges
  "向き付き辺の集合 `{始点 終点}` を、閉じた鎖の列にする。

  4点未満の鎖は捨てる —— 面を持たないので版に載らない。"
  [edges]
  (loop [remaining edges out []]
    (if (empty? remaining)
      out
      (let [start (first (keys remaining))]
        (let [[pts left]
              (loop [p start pts [] m remaining]
                (if-let [nxt (get m p)]
                  (recur nxt (conj pts p) (dissoc m p))
                  [pts m]))]
          (recur left (if (>= (count pts) 4) (conj out pts) out)))))))

;; ---------------------------------------------------------------- ⑤簡略化

(defn- perp-dist [[px* py] [ax ay] [bx by]]
  (let [dx (- bx ax) dy (- by ay)
        n (sqrt (+ (* dx dx) (* dy dy)))]
    (if (< n 1e-12)
      (sqrt (+ (* (- px* ax) (- px* ax)) (* (- py ay) (- py ay))))
      (/ (abs* (- (* dx (- ay py)) (* (- ax px*) dy))) n))))

(defn douglas-peucker
  "折れ線の簡略化。閉じた輪郭は「いちばん遠い2点」で2本に割ってから掛ける
  （端点を固定する素の DP は閉曲線に直接は使えない）。"
  [pts tol]
  (letfn [(dp [v]
            (if (<= (count v) 2)
              v
              (let [a (first v) b (peek v)
                    [idx d] (reduce (fn [[bi bd] i]
                                      (let [d (perp-dist (nth v i) a b)]
                                        (if (> d bd) [i d] [bi bd])))
                                    [0 -1.0] (range 1 (dec (count v))))]
                (if (<= d tol)
                  [a b]
                  (let [l (dp (subvec v 0 (inc idx)))
                        r (dp (subvec v idx))]
                    (into (vec (butlast l)) r))))))]
    (if (< (count pts) 4)
      pts
      (let [a (first pts)
            ;; 割る位置は「先頭からいちばん遠い点」。閉曲線を2本の開曲線にしてから
            ;; DP をかけるための分割点で、ここが近いと片側が潰れる。
            far (reduce (fn [bi i]
                          (let [d1 (let [[x y] (nth pts i) [ax ay] a]
                                     (+ (* (- x ax) (- x ax)) (* (- y ay) (- y ay))))
                                d2 (let [[x y] (nth pts bi) [ax ay] a]
                                     (+ (* (- x ax) (- x ax)) (* (- y ay) (- y ay))))]
                            (if (> d1 d2) i bi)))
                        1 (range 1 (count pts)))
            head (dp (subvec pts 0 (inc far)))
            tail (dp (conj (subvec pts far) a))]
        (vec (butlast (into (vec (butlast head)) tail)))))))

;; ---------------------------------------------------------------- 入口

(def default-opts
  {:colors 4            ; 版の数（白版を除く）。少ないほど刷りやすく安い
   :alpha-min 128       ; これ未満の α は「地」＝インクを載せない
   :simplify-px 0.8     ; Douglas–Peucker の許容誤差（縮小後の画素）
   :min-area-px 12      ; これより小さい島は捨てる（スキャンのゴミ・輪郭のギザ）
   :max-side 192})      ; 呼び出し側が縮小しておくべき長辺

(defn- photographic?
  "写真かどうかの粗い判定。標本の相異なる色が標本数の 4 割を超えていたら、
  それは階調が連続している＝網点でやるべき絵。**閾値の根拠は経験則**なので、
  止めずに所見として出すだけにする。"
  [smp]
  (and (> (count smp) 200)
       (> (/ (count (set smp)) (double (count smp))) 0.4)))

(defn trace
  "RGBA 画素 → `shirohan.artwork/load-svg` と同じ形の
  `{:contours … :findings … :bbox … :scale …}`。

  `image` は `{:width :height :data}`。`data` は RGBA が 4 バイトずつ並んだ列
  （Uint8ClampedArray でも vector でもよい）。**呼び出し側で `:max-side` まで
  縮小してから渡すこと** —— この ns は縮小しない（縮小はレンダラの仕事で、
  ブラウザなら canvas が最も綺麗にやる）。"
  ([image] (trace image {}))
  ([{:keys [width height data] :as image} opts]
   (let [{:keys [colors alpha-min simplify-px min-area-px max-side]}
         (merge default-opts opts)
         smp (samples image 8000 alpha-min)]
     (if (empty? smp)
       {:contours [] :bbox nil :scale 1.0
        :findings [{:kind :no-art :note "不透明な画素が1つも無い（全面が透過）"}]}
       (let [palette (median-cut smp colors)
             rd (byte-reader data)
             idx (mapv (fn [i]
                         (let [[r g b a] (px rd i)]
                           (if (< a alpha-min) -1 (nearest palette [r g b]))))
                       (range (* width height)))
             ->contours (fn [member? extra]
                          (keep (fn [pts]
                                  (let [simple (douglas-peucker
                                                (mapv #(mapv double %) pts) simplify-px)]
                                    (when-let [c (geom/normalize-contour
                                                  {:points simple :closed? true})]
                                      (when (>= (geom/area c) min-area-px)
                                        (merge c extra)))))
                                (chain-edges (boundary-edges idx width height member?))))
             traced (mapcat
                     (fn [ci]
                       ;; 色ごとに `:shape` を分ける —— **穴かどうかは同じ色のマスクの
                       ;; 中でしか判定できない**（別の色が上に乗っているだけの領域は
                       ;; 穴ではない）。
                       (->contours #(= ci %) {:fill (rgb->hex (nth palette ci))
                                              :shape [:color ci]}))
                     (range (count palette)))
             ;; **白版のもと**: インクが載る面（透明でない画素）のシルエット。
             ;;
             ;; 色ごとの輪郭を足し合わせて作ってはいけない —— 赤の中の白い円は
             ;; 「赤に空いた穴」だが、**そこには白インクが載る**ので白版では穴で
             ;; はない。白版は『白インクを塗る部分の指示』なので、色の切れ目では
             ;; なく**地との境目**だけを見る（実務家の指摘、2026-08-01。ラスタ経路
             ;; で赤の中の白い円が白版から抜けていた）。
             silhouette (vec (->contours #(>= % 0) {:shape :silhouette
                                                    :role :silhouette}))
             findings (cond-> []
                        (photographic? smp)
                        (conj {:kind :photographic-source
                               :note "階調が連続しています。写真から版を起こすのは本来ハーフトーン（網点）の仕事で、輪郭追跡では階調が失われます"})
                        (> (max width height) max-side)
                        (conj {:kind :oversized-raster
                               :note (str "長辺 " (max width height) "px。ブラウザ内の処理は "
                                          max-side "px までを想定しています（呼び出し前に縮小してください）")}))]
         {:contours (vec traced)
          :silhouette silhouette
          :findings findings
          :palette (mapv rgb->hex palette)
          :bbox (geom/bbox traced)
          :scale 1.0})))))
