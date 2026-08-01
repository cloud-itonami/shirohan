(ns shirohan.psd
  "版一式を **PSD**（Photoshop 形式）として書き出す。**1 版 = 1 レイヤー**。

  刷る側が PSD を欲しがるのは、版ごとにレイヤーが分かれていて、ボディ色の上で
  重なりを目で確かめられるから。だから出すのは「絵を1枚」ではなく
  **ボディ色の背景 + 刷る順のレイヤー**。

  ## 何を書いて、何を書かないか

  - チャンネルは **非圧縮（compression 0）**。PSD は RLE(PackBits) も許すが、
    圧縮は「ファイルが小さくなる」以外の価値が無く、エンコーダを1つ増やすと
    バグの置き場所が1つ増える。`kasane.codec/packbits` は**デコーダ**なので
    そのままでは使えない。
  - レイヤーは RGB + アルファの 4 チャンネル。マスク・調整レイヤー・レイヤー
    効果・カラープロファイルは書かない（版に要らない）。
  - **ラスタである。** ベクタのまま渡したいなら `shirohan.pdf`（`.ai`）を使う。

  ## 検証

  自前で書いたバイト列を、独立実装である `kasane`（`resources/kasane/grammar/psd.edn`
  ＋ `kasane.decode`）で読み直せることをテストで確認している。書き手と読み手が
  同じコードだと「自分の間違いに気付けない」ので、読み手は他人のものを使う。

  ## 大きさ

  `:px-per-mm` 既定 6（≒150dpi）。刷り位置の確認には十分で、A3 相当でも
  4M 画素に収まる。上げると画素数の 2 乗で重くなるので `:max-pixels` で頭打ちに
  し、超えたら**黙って縮めず所見を返す**。"
  (:require [clojure.string :as str]
            [shirohan.geom :as geom]))

;; ---------------------------------------------------------------- バイト書き

(defn- u8 [n] [(bit-and (int n) 0xff)])
(defn- u16 [n] [(bit-and (bit-shift-right (int n) 8) 0xff) (bit-and (int n) 0xff)])
(defn- u32 [n] (let [v (long n)]
                 [(bit-and (bit-shift-right v 24) 0xff)
                  (bit-and (bit-shift-right v 16) 0xff)
                  (bit-and (bit-shift-right v 8) 0xff)
                  (bit-and v 0xff)]))
(defn- ascii [s] (mapv #(bit-and (int #?(:clj (int %) :cljs (.charCodeAt s %))) 0xff)
                       #?(:clj (seq s) :cljs (range (count s)))))

(defn- pascal4
  "PSD のレイヤー名。先頭に長さ 1 バイト、全体が 4 の倍数になるまで 0 詰め。"
  [s]
  (let [b (ascii (subs s 0 (min 255 (count s))))
        raw (into (u8 (count b)) b)
        pad (mod (- 4 (mod (count raw) 4)) 4)]
    (into raw (repeat pad 0))))

;; ---------------------------------------------------------------- 走査線塗り

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

(defn coverage
  "輪郭群を 0/1 のビットマップにする（nonzero 巻き数則、走査線 1 本ずつ）。

  画素の中心（+0.5）で判定する。アンチエイリアスはしない —— 版は 2 値で、
  中間調は網点でしか出せないから、ここで灰色を作ると刷れないものが描ける。"
  [contours width height ox oy scale]
  (let [es (edges-of contours)]
    (persistent!
     (reduce
      (fn [acc py]
        (let [wy (+ (/ (+ py 0.5) scale) oy)
              xs (->> es
                      (keep (fn [[x1 y1 x2 y2]]
                              (when (or (and (<= y1 wy) (< wy y2))
                                        (and (<= y2 wy) (< wy y1)))
                                [(+ x1 (* (- x2 x1) (/ (- wy y1) (- y2 y1))))
                                 (if (< y1 y2) 1 -1)])))
                      (sort-by first))]
          (if (empty? xs)
            (reduce (fn [a _] (conj! a 0)) acc (range width))
            ;; 巻き数が 0 でない区間を塗る
            (let [spans (loop [v xs w 0 start nil out []]
                          (if-let [[x d] (first v)]
                            (let [w2 (+ w d)]
                              (cond (and (zero? w) (not (zero? w2))) (recur (next v) w2 x out)
                                    (and (not (zero? w)) (zero? w2)) (recur (next v) w2 nil
                                                                            (conj out [start x]))
                                    :else (recur (next v) w2 start out)))
                            out))]
              (reduce (fn [a px]
                        (let [wx (+ (/ (+ px 0.5) scale) ox)]
                          (conj! a (if (some (fn [[a0 b0]] (and (<= a0 wx) (< wx b0))) spans)
                                     1 0))))
                      acc (range width))))))
      (transient []) (range height)))))

;; ---------------------------------------------------------------- レイヤー

(defn- plate-layer
  "版 1 枚 → `{:name :rgb [r g b] :alpha <0/1 の列>}`。白抜きは alpha から引く。"
  [p width height ox oy scale]
  (let [a (coverage (:art p) width height ox oy scale)
        k (when (seq (:knockout p)) (coverage (:knockout p) width height ox oy scale))
        alpha (if k
                (vec (map (fn [v h] (if (pos? h) 0 v)) a k))
                a)]
    {:name (:label p) :hex (:color p) :alpha alpha}))

(defn- hex->rgb [hex]
  (let [h (str/replace (str hex) "#" "")
        p (fn [i] #?(:clj (Integer/parseInt (subs h i (+ i 2)) 16)
                     :cljs (js/parseInt (subs h i (+ i 2)) 16)))]
    (if (= 6 (count h)) [(p 0) (p 2) (p 4)] [0 0 0])))

(defn- layer-record [{:keys [name]} width height chan-len]
  (into []
        (concat
         (u32 0) (u32 0) (u32 height) (u32 width)   ; top left bottom right
         (u16 4)
         (mapcat (fn [id] (into (u16 (bit-and id 0xffff)) (u32 chan-len)))
                 [-1 0 1 2])                        ; alpha, R, G, B
         (ascii "8BIM") (ascii "norm")
         (u8 255) (u8 0) (u8 0) (u8 0)
         (let [nm (pascal4 name)]
           (concat (u32 (+ 8 (count nm))) (u32 0) (u32 0) nm)))))

(defn- channel-bytes
  "1 チャンネル分。先頭 2 バイトが圧縮方式（0 = 非圧縮）。"
  [vals]
  (into (u16 0) vals))

;; ---------------------------------------------------------------- 入口

(def default-opts {:px-per-mm 6.0 :max-pixels 4000000})

(defn write
  "版一式 → PSD のバイト列（0-255 の整数ベクタ）と所見。

  返り値は `{:bytes [...] :width w :height h :findings [...]}`。
  画素数が `:max-pixels` を超える場合は **書き出さず**所見だけを返す
  （黙って縮めると、受け取った側は縮んだことに気付けない）。"
  ([job] (write job {}))
  ([{:keys [size spec plates]} opts]
   (let [{:keys [px-per-mm max-pixels]} (merge default-opts opts)
         w (max 1 (int (* (:width-mm size) px-per-mm)))
         h (max 1 (int (* (:height-mm size) px-per-mm)))]
     (if (> (* w h) max-pixels)
       {:bytes nil :width w :height h
        :findings [{:kind :psd-too-large
                    :note (str w "×" h " 画素は上限 " max-pixels
                               " を超えます。:px-per-mm を下げてください")}]}
       (let [ordered (sort-by :order plates)
             layers (mapv #(plate-layer % w h 0.0 0.0 px-per-mm) ordered)
             npx (* w h)
             chan-len (+ 2 npx)
             ;; --- 合成（ボディ色の上に刷る順で重ねる） ---
             [br bg bb] (hex->rgb (:garment-color spec))
             composite (reduce
                        (fn [acc {:keys [hex alpha]}]
                          (let [[r g b] (hex->rgb hex)]
                            (mapv (fn [px a] (if (pos? a) [r g b] px)) acc alpha)))
                        (vec (repeat npx [br bg bb]))
                        layers)
             plane (fn [ci] (mapv #(nth % ci) composite))
             layer-records (mapv #(layer-record % w h chan-len) layers)
             layer-channels (into []
                                  (mapcat
                                   (fn [{:keys [hex alpha]}]
                                     (let [[r g b] (hex->rgb hex)]
                                       (concat
                                        (channel-bytes (mapv #(* 255 %) alpha))
                                        (channel-bytes (mapv (fn [_] r) alpha))
                                        (channel-bytes (mapv (fn [_] g) alpha))
                                        (channel-bytes (mapv (fn [_] b) alpha)))))
                                   layers))
             records-bytes (into [] (apply concat layer-records))
             layer-info-body (into (into (u16 (count layers)) records-bytes) layer-channels)
             ;; layer info は偶数長に揃える（仕様）
             layer-info (if (odd? (count layer-info-body))
                          (conj layer-info-body 0)
                          layer-info-body)
             layer-info-block (into (u32 (count layer-info)) layer-info)
             lmi (into layer-info-block (u32 0))            ; global layer mask info
             header (into [] (concat (ascii "8BPS") (u16 1) (repeat 6 0)
                                     (u16 3) (u32 h) (u32 w) (u16 8) (u16 3)))
             image-data (into (u16 0) (concat (plane 0) (plane 1) (plane 2)))]
         {:width w :height h :findings []
          :bytes (into [] (concat header
                                  (u32 0)                   ; color mode data
                                  (u32 0)                   ; image resources
                                  (u32 (count lmi)) lmi
                                  image-data))})))))
