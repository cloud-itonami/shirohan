(ns shirohan.plate
  "図案を**版（plate）**に分解する。ここが白版の本体。

  ## 白版とは —— **白インクを塗る部分の指示**

  白インクの上に色インクを刷らないと色が出ない。だから

  ```
  ①白版（1色ベタ塗り）を刷る  →  ②その上に色インクを刷る
  ```

  という順になる。**白版は図案の色分けではなく、図案が乗る面のシルエットを
  1 色でベタに塗ったもの。** 図案の中の白い部分も「白インクで刷る部分」なので
  白版に**含まれる**。穴になるのは、インクを一切載せない地（透明）だけ。

  ```
        図案                         白版
   ┌──────────────┐          ┌──────────────┐
   │  ▓▓▓ 赤 ▓▓▓  │          │  ████████████ │   ← 全部ベタ
   │  ░░ 白 ░░░░  │   ──▶    │  ████████████ │      （白い部分も塗る）
   │              │          │              │   ← 透明な地だけ抜ける
   └──────────────┘          └──────────────┘
  ```

  実務家の指摘（2026-08-01）:「白版は白インクを塗る部分の指示です。白インクの
  上に色インクを印刷しないと色が出ないので、1色ベタ塗り部分＝白インク を印刷
  してから色インクを印刷します」。**当初この repo は白（`#ffffff`）を既定で
  白抜き（穴）として扱っており、意味が逆だった。**

  ## choke（縮み代）—— 0.1mm

  白インクと色インクの刷りは**若干ずれる**。白版が色版とぴったり同じ大きさだと、
  ずれた瞬間に白が図案の外へはみ出して縁が白く光る（白フチ／ハロー）。そこで
  白版は図案より **0.1mm 小さく**作る。

  ```
  図案の輪郭 ─┐
              ├─ choke 0.1mm 内側 ──▶ 白版
  白抜きの穴 ─┴─ choke 0.1mm 外側 ──▶ 白版（穴が広がる＝白が縮む）
  ```

  **穴は逆向きに動く**のが要点。「白の面を全周 choke だけ痩せさせる」ことが
  目的なので、外周は内へ、穴は外へ動かす。`shirohan.geom/offset` は
  「δ>0 で囲む面が広がる」符号なので、`:art` に `-choke`、`:knockout` に
  `+choke` を渡すだけでこれになる。

  ## 白抜き（knockout）は別物

  「生地の色をそのまま見せる穴」を作りたいときだけ使う、**明示的な**指定
  （`:knockout-fill` か、`id`/`class` に `knockout`）。既定では使わない ——
  既定で白を穴にすると、白いところが刷られずに生地が出てしまう。

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
  {;; 白インクと色インクの刷りが**若干ずれる**ぶんだけ、白版を小さく作る。
   ;; 現場の実務値は 0.1mm（製版担当者の指摘、2026-08-01）。
   :choke-mm 0.1
   :min-line-mm 0.3       ; スクリーンで刷れる最小線幅の目安
   :margin-mm 12.0        ; 版の余白（見当合わせマークが入る）
   :print-width-mm 280.0  ; 刷り上がりの幅
   :garment-color "#1f1f1f" ; プレビューのボディ色（版ではない）
   :white-underbase? true
   :registration? true
   ;; **既定では白抜きにしない。** 白版は「白インクを塗る部分の指示」なので、
   ;; 図案の白い部分も白インクで刷る = 白版に**含まれる**。穴になるのは
   ;; 透明（アルファ 0）の地だけ。白抜き（生地を見せる穴）は別の意図で、
   ;; 使いたいときだけ明示する。
   :knockout-fill nil
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
  ことがあり、そうなると自分自身の内外判定が壊れる。頂点なら必ず輪郭上にある。

  **同じ `:shape`（同じ要素／同じ `d`）の中だけで数える。** 別の要素が上に
  乗っているだけの図形は穴ではない —— 区別しないと、青い四角の上に置いた白い
  四角が『青に空いた穴』になり、白版がそこだけ抜ける（実測 2026-08-01）。"
  [c others]
  (let [p (first (:points c))
        same-shape (filter #(= (:shape %) (:shape c)) others)]
    (count (filter #(and (not (identical? % c)) (geom/inside? % p)) same-shape))))

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
         ;; **白版のもと**。あればこれを使う（ラスタ経路が「インクが載る面」を
         ;; 直接トレースして渡してくる）。無ければ色版の和で代用する。
         ;;
         ;; 色版の和で代用するのは近似でしかない —— 赤の中の白い円は「赤に
         ;; 空いた穴」だが、そこには白インクが載るので白版では穴ではない。
         ;; SVG 経路では `:shape` 単位の入れ子判定でこれを避けているが、
         ;; ラスタ経路は色ごとのマスクしか持たないので、地との境目を直接
         ;; トレースしたものを渡してもらう必要がある。
         silhouette (filterv #(= :silhouette (:role %)) cs)
         underbase-art (if (seq silhouette) silhouette arts)
         w (+ (* 2 m) (if bbox (- (:x1 bbox) (:x0 bbox)) 0.0))
         h (+ (* 2 m) (if bbox (- (:y1 bbox) (:y0 bbox)) 0.0))

         ;; --- 白版: インクの外周を choke だけ内へ、穴の縁を choke だけ外へ ---
         white (when (and white-underbase? (seq underbase-art))
                 {:id :white :label "白版（下地）" :color "#ffffff" :order 0
                  :underbase? true
                  :art (mapv #(geom/offset % (choke-delta % underbase-art choke-mm))
                             underbase-art)
                  ;; 白抜きは宣言によって穴なので、深さを見ずに必ず外へ広げる。
                  :knockout (mapv #(geom/offset % choke-mm) kos)})

         ;; --- スポット版: 塗りの色ごとに 1 版。choke はかけない ---
         ;;
         ;; **白版を出すとき、白のスポット版は作らない。** 白版の上にもう一度
         ;; 白を刷っても意味が無い（既にそこはベタで白い）。図案の白い部分は
         ;; 白版そのものが受け持つ、というのが「白版＝白インクを塗る部分の指示」
         ;; の帰結。白版を出さない設定（淡色ボディ）では、白も普通の 1 色として
         ;; 刷るので残す。
         spots (->> (group-by :fill arts)
                    (remove (fn [[fill _]]
                              (and white (= (str/lower-case (str fill)) "#ffffff"))))
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
                     (mapcat #(contour-findings % spec "白版（下地）") underbase-art))
                   (knockout-findings kos arts)
                   (when (empty? arts)
                     [{:kind :no-art :note "版に載せられる図案が無い"}])))]
     {:plates plates
      :findings (vec (distinct qc))
      :size {:width-mm w :height-mm h}
      :spec spec})))
