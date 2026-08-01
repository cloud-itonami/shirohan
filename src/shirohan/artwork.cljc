(ns shirohan.artwork
  "SVG 文書から**塗りのある図形だけ**を取り出して、mm 座標の輪郭にする。

  ## 何を読むか

  `path` `rect` `circle` `ellipse` `polygon` `polyline` の 6 つ。`g` の入れ子を
  たどって `transform` と `fill` を継承する。`transform` は `translate` `scale`
  `rotate` `matrix` `skewX` `skewY` を合成する。

  ## 何を読まないか（黙って落とさず `:findings` で報告する）

  - **`text`** —— フォントのアウトライン化が要る。書体データを解析するのは
    この repo が持つべき機構ではない（`cloud-itonami/inkan` が同じ理由で
    `<text>` を `<text>` のまま出しているのと同じ判断）。版下に文字を載せるなら
    **入稿前にアウトライン化する**のが刷る側の常識でもある。
  - **`image`** —— ラスタ。ベクタ版は作れない。
  - **`use`** —— 参照解決を持たない。展開してから入稿する。
  - **グラデーション・パターン塗り** —— 版は単色。`url(#…)` の塗りは版に分解できない。
  - **`stroke` だけの図形（`fill=\"none\"`）** —— 線を面に変換（stroke のアウトライン化）
    していないので版に載せられない。入稿前にパス化する。

  「読めなかった」を握り潰さないのがこの ns の要点。版から図案が消えた理由が
  分からないまま刷るのがいちばん高くつく。

  ## 正規表現に inline flag を使わない

  `(?s)` `(?i)` は **JavaScript の RegExp に無い**。この repo の .cljc は
  ブラウザで scittle が読む（`shirohan.plate` まで丸ごと）ので、`(?s)` を書いた
  瞬間にページが構文エラーで死ぬ。`.` の代わりに `[\\s\\S]`、大文字小文字は
  選択肢を並べて書く。

  ## 白抜き（knockout）の指定

  既定では **塗りが白（`#ffffff`）の図形を白抜きとして扱う**。濃色ボディに刷る
  版下では、白は「生地を見せる穴」か「白インク」のどちらかで、図案データだけ
  からは決まらない。既定を白抜きにするのは、間違えたときに**インクを載せすぎる
  より版が抜ける方が刷る前に気づける**から。`:knockout-fill` で変えられるし、
  `id`/`class` に `knockout` を含む要素は塗りに関係なく白抜きになる。"
  (:require [clojure.string :as str]
            [shirohan.path :as path]
            [shirohan.geom :as geom]))

(defn- nan? [v] #?(:clj (Double/isNaN (double v)) :cljs (js/isNaN v)))

(defn- num* [s d]
  (if (str/blank? (str s))
    d
    (let [v #?(:clj (try (Double/parseDouble (str/trim (str s)))
                         (catch Exception _ d))
               :cljs (js/parseFloat (str s)))]
      (if (nan? v) d v))))

(defn- num-opt [s] (when-not (str/blank? (str s))
                     (let [v (num* s ##NaN)] (when-not (nan? v) v))))

(defn- sin [x] #?(:clj (Math/sin (double x)) :cljs (js/Math.sin x)))
(defn- cos [x] #?(:clj (Math/cos (double x)) :cljs (js/Math.cos x)))
(defn- tan [x] #?(:clj (Math/tan (double x)) :cljs (js/Math.tan x)))
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))

;; ---------------------------------------------------------------- 行列

(def identity-ctm [1.0 0.0 0.0 1.0 0.0 0.0])

(defn- mul
  "m1 ∘ m2（m2 を先に適用する）。[a b c d e f] は x' = ax+cy+e, y' = bx+dy+f。"
  [[a1 b1 c1 d1 e1 f1] [a2 b2 c2 d2 e2 f2]]
  [(+ (* a1 a2) (* c1 b2))
   (+ (* b1 a2) (* d1 b2))
   (+ (* a1 c2) (* c1 d2))
   (+ (* b1 c2) (* d1 d2))
   (+ (* a1 e2) (* c1 f2) e1)
   (+ (* b1 e2) (* d1 f2) f1)])

(defn- apply-ctm [[a b c d e f] [x y]]
  [(+ (* a x) (* c y) e) (+ (* b x) (* d y) f)])

(def ^:private transform-re
  #"(matrix|translate|scale|rotate|skewX|skewY|skewx|skewy)\s*\(([^)]*)\)")

(defn parse-transform
  "`transform` 属性を 1 個の行列にする。並んだ変換は**左から順に適用**（SVG 仕様）。"
  [s]
  (if (str/blank? (str s))
    identity-ctm
    (reduce
     (fn [acc [_ op args]]
       (let [v (mapv #(num* % 0.0)
                     (remove str/blank? (str/split (str/trim args) #"[\s,]+")))
             g #(nth v % 0.0)
             m (case (str/lower-case op)
                 "matrix" [(g 0) (g 1) (g 2) (g 3) (g 4) (g 5)]
                 "translate" [1.0 0.0 0.0 1.0 (g 0) (if (> (count v) 1) (g 1) 0.0)]
                 "scale" (let [sx (if (seq v) (g 0) 1.0)
                               sy (if (> (count v) 1) (g 1) sx)]
                           [sx 0.0 0.0 sy 0.0 0.0])
                 "rotate" (let [a (/ (* (g 0) pi) 180.0)
                                ca (cos a) sa (sin a)
                                r [ca sa (- sa) ca 0.0 0.0]]
                            (if (> (count v) 2)
                              (mul (mul [1.0 0.0 0.0 1.0 (g 1) (g 2)] r)
                                   [1.0 0.0 0.0 1.0 (- (g 1)) (- (g 2))])
                              r))
                 "skewx" [1.0 0.0 (tan (/ (* (g 0) pi) 180.0)) 1.0 0.0 0.0]
                 "skewy" [1.0 (tan (/ (* (g 0) pi) 180.0)) 0.0 1.0 0.0 0.0]
                 identity-ctm)]
         (mul acc m)))
     identity-ctm
     (re-seq transform-re s))))

;; ---------------------------------------------------------------- XML 走査

(def ^:private tag-re
  #"<\s*(/?)\s*([A-Za-z_][\w:.-]*)((?:\"[^\"]*\"|'[^']*'|[^>\"'])*?)(/?)\s*>")

(def ^:private attr-re #"([A-Za-z_:][-\w:.]*)\s*=\s*(?:\"([^\"]*)\"|'([^']*)')")

(def ^:private elided-re
  ;; 中身が XML タグでない（CSS / JS / 文字列）ので、タグ走査の前に丸ごと落とす。
  #"<\s*(style|script|text|title|desc|metadata)\b[^>]*>[\s\S]*?<\s*/\s*\1\s*>")

(defn- strip-noise [s]
  (-> (str s)
      (str/replace #"<!--[\s\S]*?-->" "")
      (str/replace #"<!\[CDATA\[[\s\S]*?\]\]>" "")
      (str/replace elided-re (fn [m] (str "<" (nth m 1) "-elided/>")))))

(defn- attrs-of [s]
  (into {} (map (fn [[_ k v1 v2]] [(str/lower-case k) (or v1 v2)])
                (re-seq attr-re (str s)))))

(def ^:private drawable #{"path" "rect" "circle" "ellipse" "polygon" "polyline"})

(def ^:private skip-subtree
  "中身を**描かない**要素。`defs` の中のパスを図案として拾ってしまうと、
  使われていない図形が版に載る。"
  #{"defs" "clippath" "mask" "marker" "pattern" "symbol"
    "lineargradient" "radialgradient" "filter"})

(def ^:private reported
  {"text-elided" :text "text" :text "image" :image "use" :use})

(def ^:private void-tags
  #{"path" "rect" "circle" "ellipse" "polygon" "polyline" "line" "image" "use"
    "stop" "br" "text-elided" "style-elided" "script-elided" "title-elided"
    "desc-elided" "metadata-elided"})

(defn scan
  "SVG 文字列 → `{:elements [{:tag :attrs :ctm :fill} …] :unsupported #{…} :view-box …}`。

  DOM を作らずタグを舐めるだけなので、閉じ忘れのある壊れた SVG でも止まらない。
  `defs` 等はサブツリーごと飛ばす（`skip` は入れ子の深さを数える）。"
  [svg]
  (let [src (strip-noise svg)]
    (loop [ms (seq (re-seq tag-re src))
           stack [{:ctm identity-ctm :fill nil}]
           skip 0
           out []
           unsup #{}
           vb nil]
      (if-not ms
        {:elements out :unsupported unsup :view-box vb}
        (let [[_ close? raw-tag raw self-slash] (first ms)
              tag (str/lower-case raw-tag)
              closing? (seq close?)
              self? (or (= self-slash "/") (contains? void-tags tag))]
          (cond
            ;; --- サブツリーを飛ばしている最中 ---
            (pos? skip)
            (recur (next ms) stack
                   (cond closing? (dec skip)
                         self? skip
                         :else (inc skip))
                   out unsup vb)

            closing?
            (recur (next ms) (if (> (count stack) 1) (pop stack) stack) 0 out unsup vb)

            (contains? skip-subtree tag)
            (recur (next ms) stack (if self? 0 1) out unsup vb)

            :else
            (let [a (attrs-of raw)
                  top (peek stack)
                  ctm (mul (:ctm top) (parse-transform (get a "transform")))
                  fill (or (get a "fill")
                           (second (re-find #"fill\s*:\s*([^;]+)" (str (get a "style"))))
                           (:fill top))]
              (cond
                (contains? reported tag)
                (recur (next ms) stack 0 out (conj unsup (get reported tag)) vb)

                (contains? drawable tag)
                (recur (next ms) stack 0
                       (conj out {:tag tag :attrs a :ctm ctm
                                  :fill (some-> fill str/trim)})
                       unsup vb)

                :else
                (recur (next ms)
                       (if self? stack (conj stack {:ctm ctm :fill fill}))
                       0 out unsup
                       (if (= tag "svg") (or vb (get a "viewbox")) vb))))))))))

;; ---------------------------------------------------------------- 図形 → d

(defn- rect-d [a]
  (let [x (num* (get a "x") 0) y (num* (get a "y") 0)
        w (num* (get a "width") 0) h (num* (get a "height") 0)
        rx0 (num-opt (get a "rx")) ry0 (num-opt (get a "ry"))
        rx (min (/ w 2) (or rx0 ry0 0))
        ry (min (/ h 2) (or ry0 rx0 0))]
    (when (and (pos? w) (pos? h))
      (if (or (<= rx 0) (<= ry 0))
        (str "M" x " " y "H" (+ x w) "V" (+ y h) "H" x "Z")
        (str "M" (+ x rx) " " y
             "H" (- (+ x w) rx) "A" rx " " ry " 0 0 1 " (+ x w) " " (+ y ry)
             "V" (- (+ y h) ry) "A" rx " " ry " 0 0 1 " (- (+ x w) rx) " " (+ y h)
             "H" (+ x rx) "A" rx " " ry " 0 0 1 " x " " (- (+ y h) ry)
             "V" (+ y ry) "A" rx " " ry " 0 0 1 " (+ x rx) " " y "Z")))))

(defn- ellipse-d [cx cy rx ry]
  (when (and (pos? rx) (pos? ry))
    (str "M" (- cx rx) " " cy
         "A" rx " " ry " 0 1 0 " (+ cx rx) " " cy
         "A" rx " " ry " 0 1 0 " (- cx rx) " " cy "Z")))

(defn- points-d [s closed?]
  (let [v (mapv #(num* % 0.0)
                (remove str/blank? (str/split (str/trim (str s)) #"[\s,]+")))]
    (when (>= (count v) 6)
      (str "M" (str/join "L" (map (fn [i] (str (nth v (* 2 i)) " " (nth v (inc (* 2 i)))))
                                  (range (quot (count v) 2))))
           (when closed? "Z")))))

(defn element->d [{:keys [tag attrs]}]
  (case tag
    "path" (get attrs "d")
    "rect" (rect-d attrs)
    "circle" (let [r (num* (get attrs "r") 0)]
               (ellipse-d (num* (get attrs "cx") 0) (num* (get attrs "cy") 0) r r))
    "ellipse" (ellipse-d (num* (get attrs "cx") 0) (num* (get attrs "cy") 0)
                         (num* (get attrs "rx") 0) (num* (get attrs "ry") 0))
    "polygon" (points-d (get attrs "points") true)
    "polyline" (points-d (get attrs "points") false)
    nil))

;; ---------------------------------------------------------------- 色

(def ^:private named-colors
  {"black" "#000000" "white" "#ffffff" "red" "#ff0000" "lime" "#00ff00"
   "blue" "#0000ff" "yellow" "#ffff00" "cyan" "#00ffff" "aqua" "#00ffff"
   "magenta" "#ff00ff" "fuchsia" "#ff00ff" "silver" "#c0c0c0" "gray" "#808080"
   "grey" "#808080" "maroon" "#800000" "olive" "#808000" "green" "#008000"
   "purple" "#800080" "teal" "#008080" "navy" "#000080" "orange" "#ffa500"})

(defn- hex2 [n]
  (let [i (max 0 (min 255 (int (+ (double n) 0.5))))
        s #?(:clj (Integer/toHexString i) :cljs (.toString i 16))]
    (if (= 1 (count s)) (str "0" s) s)))

(defn normalize-fill
  "塗りを `#rrggbb` に正規化する。版に分解できないものは keyword で返す
  （`:none` `:unresolvable`）—— 「とりあえず黒」で誤魔化さない。"
  [fill]
  (let [f (str/lower-case (str/trim (str (or fill "#000000"))))]
    (cond
      (str/blank? f) "#000000"
      (= f "none") :none
      (= f "transparent") :none
      (str/starts-with? f "url(") :unresolvable
      (str/starts-with? f "currentcolor") :unresolvable
      (contains? named-colors f) (get named-colors f)
      (re-matches #"#[0-9a-f]{3}" f)
      (let [r (subs f 1 2) g (subs f 2 3) b (subs f 3 4)]
        (str "#" r r g g b b))
      (re-matches #"#[0-9a-f]{6}" f) f
      (re-matches #"#[0-9a-f]{8}" f) (subs f 0 7)
      (re-matches #"rgba?\([^)]*\)" f)
      (let [v (vec (take 3 (map #(num* % 0.0)
                                (remove str/blank?
                                        (str/split (str/replace f #"rgba?\(|\)" "")
                                                   #"[\s,]+")))))]
        (if (= 3 (count v))
          (str "#" (hex2 (nth v 0)) (hex2 (nth v 1)) (hex2 (nth v 2)))
          :unresolvable))
      :else :unresolvable)))

;; ---------------------------------------------------------------- load

(defn- transform-contour [ctm c]
  (update c :points #(mapv (partial apply-ctm ctm) %)))

(defn- element->contours
  [{:keys [tag attrs] :as el} {:keys [tolerance-mm ko-fill]}]
  (let [d (element->d el)
        norm (normalize-fill (:fill el))
        ko? (or (and ko-fill (= norm ko-fill))
                (boolean (re-find #"knockout"
                                  (str/lower-case (str (get attrs "id") " "
                                                       (get attrs "class"))))))]
    (cond
      (str/blank? (str d)) {:contours [] :findings []}

      (= norm :none)
      {:contours []
       :findings [{:kind :stroke-only :element tag
                   :note "fill=\"none\" の図形は版に載せられない（線をパス化して入稿する）"}]}

      (= norm :unresolvable)
      {:contours []
       :findings [{:kind :unresolvable-fill :element tag
                   :note "グラデーション・パターン・currentColor は単色の版に分解できない"}]}

      :else
      (reduce (fn [acc c]
                (if (false? (:closed? c))
                  (update acc :findings conj
                          {:kind :open-contour :element tag
                           :note "閉じていないサブパスは面にならない"})
                  (if-let [nc (geom/normalize-contour c)]
                    (update acc :contours conj
                            (assoc nc :fill norm :role (if ko? :knockout :art)))
                    acc)))
              {:contours [] :findings []}
              (map (partial transform-contour (:ctm el))
                   (path/parse d {:tolerance-mm tolerance-mm}))))))

(defn load-svg
  "SVG 文字列 → `{:contours [...] :findings [...] :bbox {...} :scale …}`。

  輪郭は **mm 座標**で、図案の外接矩形の左上が原点、幅が `:print-width-mm`
  （既定 280mm ＝ Tシャツ前身頃の実用上限あたり）になるよう等方拡縮する。
  版下の寸法は「元データの単位」ではなく「刷る寸法」で決まるので、ここで mm に
  そろえてしまう。

  opts:
  - `:print-width-mm`  刷り上がりの幅（既定 280.0）
  - `:tolerance-mm`    曲線を折るときの弦の許容誤差（既定 0.05）
  - `:knockout-fill`   白抜きとして扱う塗り（既定 `\"#ffffff\"`、`nil` で無効）"
  ([svg] (load-svg svg {}))
  ([svg {:keys [print-width-mm tolerance-mm knockout-fill]
         :or {print-width-mm 280.0 tolerance-mm 0.05 knockout-fill "#ffffff"}}]
   (let [{:keys [elements unsupported]} (scan svg)
         opts {:tolerance-mm tolerance-mm
               :ko-fill (some-> knockout-fill str/lower-case str/trim)}
         raw (reduce (fn [acc el]
                       (let [{:keys [contours findings]} (element->contours el opts)]
                         (-> acc
                             (update :contours into contours)
                             (update :findings into findings))))
                     {:contours [] :findings []}
                     elements)
         findings (into (:findings raw)
                        (keep (fn [k]
                                (case k
                                  :text {:kind :text-not-outlined
                                         :note "<text> はアウトライン化していないため版に載せられない（入稿前にパス化する）"}
                                  :image {:kind :raster-image
                                          :note "<image> はラスタ。ベクタ版は作れない"}
                                  :use {:kind :use-reference
                                        :note "<use> の参照解決を持たない（展開してから入稿する）"}
                                  nil))
                              unsupported))
         bb (geom/bbox (:contours raw))]
     (if (nil? bb)
       {:contours []
        :findings (conj (vec findings)
                        {:kind :no-art :note "塗りのある閉じた図形が 1 つも無い"})
        :bbox nil :scale 1.0}
       (let [w (max 1e-9 (- (:x1 bb) (:x0 bb)))
             s (/ print-width-mm w)
             mv (fn [[x y]] [(* s (- x (:x0 bb))) (* s (- y (:y0 bb)))])
             cs (mapv #(update % :points (fn [ps] (mapv mv ps))) (:contours raw))]
         {:contours cs
          :findings (vec findings)
          :bbox (geom/bbox cs)
          :scale s})))))
