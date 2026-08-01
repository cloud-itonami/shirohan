(ns shirohan.plate
  "図案を**版（plate）**に分解する。ここが白版の本体。

  ## 白版とは

  濃色のボディに色を刷ると、生地の色が透けて figure が沈む。だから色版の下に
  **白インクの下地＝白版**を先に刷る。白版が図案とぴったり同じ大きさだと、
  刷り位置が 0.1mm でもずれた瞬間に白が figure の外へはみ出して縁が白く光る
  （白フチ／ハロー）。そこで白版は図案より **choke（縮み代）** ぶん内側に詰める。

  ```
  図案の輪郭 ─┐
              ├─ choke 0.2mm 内側 ──▶ 白版
  白抜きの穴 ─┴─ choke 0.2mm 外側 ──▶ 白版（穴が広がる＝白が縮む）
  ```

  **穴は逆向きに動く**のが要点。「白の面を全周 choke だけ痩せさせる」ことが
  目的なので、外周は内へ、穴は外へ動かす。`shirohan.geom/offset` は
  「δ>0 で囲む面が広がる」符号なので、`:art` に `-choke`、`:knockout` に
  `+choke` を渡すだけでこれになる。

  ## 白抜き（knockout）をブール演算で解かない

  穴あきの版は SVG の `mask` で表す（`shirohan.svg`）。多角形のブール演算器を
  持たないのは、**持たなくても正しい結果が出る**から —— `mask` はラスタライザが
  解くので、輪郭がいくつ重なっても、穴が穴の中にあっても結果は正しい。
  自前のブール演算は交点計算の数値誤差でしか壊れないので、持たない方が強い。

  代わりに輪郭は `:art` / `:knockout` のタグ付きでそのまま持ち回る。RIP や
  カッティングプロッタに渡すときは、受け側が自分のブール演算を持っている。

  ## 版の順番

  `:order` は刷る順。0 が白版で、以降のスポット版は**明るい順**に並べる。
  重ねるほど濃くなる刷りでは、明るい色を先に置いて濃い色を後から重ねるのが
  一般的で、逆にすると濃色の上に明るい色が乗って濁る。"
  (:require [clojure.string :as str]
            [shirohan.geom :as geom]))

(def default-spec
  {:choke-mm 0.2          ; 白版の縮み代。0.15〜0.3mm が実用域
   :min-line-mm 0.3       ; スクリーンで刷れる最小線幅の目安
   :margin-mm 12.0        ; 版の余白（見当合わせマークが入る）
   :print-width-mm 280.0  ; 刷り上がりの幅
   :garment-color "#1f1f1f" ; プレビューのボディ色（版ではない）
   :white-underbase? true
   :registration? true
   :knockout-fill "#ffffff"
   :tolerance-mm 0.05})

;; ---------------------------------------------------------------- 色

(defn- hex->rgb [h]
  (let [h (str/replace (str h) "#" "")
        p (fn [i] #?(:clj (Integer/parseInt (subs h i (+ i 2)) 16)
                     :cljs (js/parseInt (subs h i (+ i 2)) 16)))]
    (if (= 6 (count h)) [(p 0) (p 2) (p 4)] [0 0 0])))

(defn luminance
  "相対輝度 0..1（Rec.709）。版の並び順にだけ使う。"
  [hex]
  (let [[r g b] (hex->rgb hex)]
    (/ (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 255.0)))

(defn- label-for [hex]
  (str "スポット版 " (str/upper-case (str hex))))

;; ---------------------------------------------------------------- QC

(defn- contour-findings [c {:keys [choke-mm min-line-mm]} label]
  (let [w (geom/min-feature-width c)
        out []]
    (cond-> out
      (< w (* 2.0 choke-mm))
      (conj {:kind :choke-erases-feature
             :plate label
             :measured-mm (geom/fmt w)
             :note (str "最小特徴幅 " (geom/fmt w) "mm が choke の 2 倍（"
                        (geom/fmt (* 2.0 choke-mm)) "mm）を下回る。"
                        "この部分は白版から消える —— choke を下げるか図案を太らせる")})

      (< w min-line-mm)
      (conj {:kind :below-min-line
             :plate label
             :measured-mm (geom/fmt w)
             :note (str "最小特徴幅 " (geom/fmt w) "mm が刷れる最小線幅 "
                        (geom/fmt min-line-mm) "mm を下回る。刷ると潰れるか飛ぶ")}))))

(defn- knockout-findings [knockouts arts]
  (keep (fn [k]
          (let [c (geom/centroid k)]
            (when-not (some #(geom/inside? % c) arts)
              {:kind :knockout-outside-art
               :note "白抜きの図形が、どの図案の内側にもない。版に何の効果も持たない（塗りが白の図形を白抜きと解釈した結果かもしれない）"})))
        knockouts))

;; ---------------------------------------------------------------- 分解

(defn- shift [dx dy c]
  (update c :points #(mapv (fn [[x y]] [(+ x dx) (+ y dy)]) %)))

;; ---------------------------------------------------------------- 入れ子の深さ

(defn nesting-depth
  "`c` を囲んでいる同一集合の輪郭の数。

  choke の向きはこれで決まる。「O」の字は外周と内周の 2 本の輪郭でできていて、
  **内周が囲んでいるのは穴**なので、白版では外周を内へ、内周を外へ動かす。
  輪郭 1 本を見ただけではどちらか分からないので、囲まれている数の偶奇で決める
  （偶数＝インクの外周、奇数＝穴の縁）。SVG の nonzero 塗りが実際に描く形と、
  素直な図案では一致する。

  判定点には**重心ではなく先頭の頂点**を使う。凹形状では重心が輪郭の外に出る
  ことがあり、そうなると自分自身の内外判定が壊れる。頂点なら必ず輪郭上にある。"
  [c others]
  (let [p (first (:points c))]
    (count (filter #(and (not (identical? % c)) (geom/inside? % p)) others))))

(defn- choke-delta
  "この輪郭に与えるオフセット量。インクの外周は内へ（負）、穴の縁は外へ（正）。"
  [c others choke-mm]
  (if (odd? (nesting-depth c others)) choke-mm (- choke-mm)))

(defn- plate-area [{:keys [art knockout]}]
  (max 0.0 (- (reduce + 0.0 (map geom/area art))
              (reduce + 0.0 (map geom/area knockout)))))

(defn separate
  "`shirohan.artwork/load-svg` の結果 + spec → 版の一式。

  返り値:

  ```clojure
  {:plates [{:id :white :label \"白版\" :color \"#ffffff\" :order 0
             :art [輪郭…] :knockout [輪郭…] :area-mm2 …} …]
   :findings [{:kind … :note …} …]
   :size {:width-mm … :height-mm …}
   :spec {…}}
  ```

  版の座標は**版の左上が原点**（図案は `:margin-mm` だけ内側に置かれる）。
  すべての版が同じ原点・同じ寸法を共有するので、重ねれば見当が合う。"
  ([artwork] (separate artwork {}))
  ([{:keys [contours findings bbox]} spec]
   (let [{:keys [choke-mm margin-mm white-underbase?] :as spec} (merge default-spec spec)
         m margin-mm
         cs (mapv (partial shift m m) contours)
         arts (filterv #(= :art (:role %)) cs)
         kos (filterv #(= :knockout (:role %)) cs)
         w (+ (* 2 m) (if bbox (- (:x1 bbox) (:x0 bbox)) 0.0))
         h (+ (* 2 m) (if bbox (- (:y1 bbox) (:y0 bbox)) 0.0))

         ;; --- 白版: インクの外周を choke だけ内へ、穴の縁を choke だけ外へ ---
         white (when (and white-underbase? (seq arts))
                 {:id :white :label "白版（下地）" :color "#ffffff" :order 0
                  :underbase? true
                  :art (mapv #(geom/offset % (choke-delta % arts choke-mm)) arts)
                  ;; 白抜きは宣言によって穴なので、深さを見ずに必ず外へ広げる。
                  :knockout (mapv #(geom/offset % choke-mm) kos)})

         ;; --- スポット版: 塗りの色ごとに 1 版。choke はかけない ---
         spots (->> (group-by :fill arts)
                    (map (fn [[fill group]]
                           {:id (keyword (str "spot" (str/replace (str fill) "#" "")))
                            :label (label-for fill)
                            :color fill
                            :underbase? false
                            :art (vec group)
                            :knockout kos}))
                    (sort-by (comp - luminance :color))
                    vec)
         spots (vec (map-indexed #(assoc %2 :order (inc %1)) spots))

         plates (vec (keep identity (cons white spots)))
         plates (mapv #(assoc % :area-mm2 (plate-area %)) plates)

         qc (into (vec findings)
                  (concat
                   ;; 細り検査は **choke をかける前の**図案に対してかける。offset 後の
                   ;; 輪郭で測ると、すでに自己交差して裏返った形の幅を測ることになり、
                   ;; 「消えた」という肝心の事実がむしろ見えなくなる。白版を作るときだけ
                   ;; 意味のある検査なので、白版を出さない設定では回さない。
                   (when white
                     (mapcat #(contour-findings % spec "白版（下地）") arts))
                   (knockout-findings kos arts)
                   (when (empty? arts)
                     [{:kind :no-art :note "版に載せられる図案が無い"}])))]
     {:plates plates
      :findings (vec (distinct qc))
      :size {:width-mm w :height-mm h}
      :spec spec})))
