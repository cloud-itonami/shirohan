(ns shirohan.svg
  "`shirohan.plate/separate` の結果を SVG 文字列にする。

  ## 版 1 枚は「フィルム」として出す

  製版に渡す版下は、インクの色ではなく**黒 100% / 白 0%** の 2 値。スクリーンの
  感光乳剤は光の有無しか見ないので、色を付けても意味がなく、むしろ濃度が下がって
  露光が甘くなる。だから `:as :film`（既定）は黒地に白抜き、ではなく**白地に黒**で
  出す。インクの色で見たいとき（画面で確認するとき）だけ `:as :ink`。

  ## 白抜きは mask で表す

  多角形のブール演算をしない（`shirohan.plate` の doc 参照）。穴は
  `<mask>` の中に黒で描き、ラスタライザに解かせる。輪郭が何本重なっても、
  穴の中に穴があっても正しい。

  ## 見当合わせマーク（トンボ）

  すべての版が**同じ原点・同じ寸法**を持ち、四隅に同じマークが入る。刷る側は
  これを重ねて見当を出す。マークは版の内容ではないので、`:registration? false`
  で落とせる。"
  (:require [clojure.string :as str]
            [shirohan.geom :as geom]))

(def ^:private fmt geom/fmt)

(defn- esc [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

;; ---------------------------------------------------------------- 見当合わせ

(defn registration-marks
  "四隅の見当合わせマーク。半径 2mm の円 + 十字（腕 5mm）。

  余白 `margin-mm` の中央に置く —— 図案に重ならず、かつ断裁の外に出ない位置。"
  [{:keys [width-mm height-mm]} margin-mm color]
  (let [c (/ margin-mm 2.0)
        r 2.0
        arm 5.0
        at (fn [x y]
             (str "<g stroke=\"" color "\" stroke-width=\"0.2\" fill=\"none\">"
                  "<circle cx=\"" (fmt x) "\" cy=\"" (fmt y) "\" r=\"" (fmt r) "\"/>"
                  "<line x1=\"" (fmt (- x arm)) "\" y1=\"" (fmt y)
                  "\" x2=\"" (fmt (+ x arm)) "\" y2=\"" (fmt y) "\"/>"
                  "<line x1=\"" (fmt x) "\" y1=\"" (fmt (- y arm))
                  "\" x2=\"" (fmt x) "\" y2=\"" (fmt (+ y arm)) "\"/>"
                  "</g>"))]
    (str (at c c)
         (at (- width-mm c) c)
         (at c (- height-mm c))
         (at (- width-mm c) (- height-mm c)))))

;; ---------------------------------------------------------------- 版 1 枚

(defn- mask-def [id {:keys [knockout]} {:keys [width-mm height-mm]}]
  (when (seq knockout)
    (str "<defs><mask id=\"" id "\" maskUnits=\"userSpaceOnUse\""
         " x=\"0\" y=\"0\" width=\"" (fmt width-mm) "\" height=\"" (fmt height-mm) "\">"
         "<rect x=\"0\" y=\"0\" width=\"" (fmt width-mm) "\" height=\"" (fmt height-mm)
         "\" fill=\"#ffffff\"/>"
         "<path d=\"" (geom/contours->d knockout) "\" fill=\"#000000\"/>"
         "</mask></defs>")))

(defn- plate-body
  "版の中身（背景も見当も含まない）。`ink` は塗る色。"
  [plate size ink mask-id]
  (let [d (geom/contours->d (:art plate))]
    (when-not (str/blank? d)
      (str "<path d=\"" d "\" fill=\"" ink "\""
           (when mask-id (str " mask=\"url(#" mask-id ")\""))
           "/>"))))

(defn plate-svg
  "版 1 枚を SVG 文書にする。

  opts:
  - `:as`         `:film`（既定・白地に黒）か `:ink`（インクの色で表示）
  - `:px-per-mm`  `width`/`height` 属性の換算（既定 4 ≒ 100dpi）。`viewBox` は常に mm
  - `:registration?` 見当合わせマークを描くか（既定は spec に従う）"
  ([result plate] (plate-svg result plate {}))
  ([{:keys [size spec]} plate {:keys [as px-per-mm registration?]
                               :or {as :film px-per-mm 4}}]
   (let [{:keys [width-mm height-mm]} size
         film? (= as :film)
         ink (if film? "#000000" (:color plate))
         bg (if film? "#ffffff" (:garment-color spec))
         mark-color (if film? "#000000" "#8a8a8a")
         reg? (if (some? registration?) registration? (:registration? spec))
         mask-id (str "ko-" (name (:id plate)))]
     (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
          " width=\"" (fmt (* width-mm px-per-mm)) "\""
          " height=\"" (fmt (* height-mm px-per-mm)) "\""
          " viewBox=\"0 0 " (fmt width-mm) " " (fmt height-mm) "\""
          " role=\"img\" aria-label=\"" (esc (:label plate)) "\">"
          "<title>" (esc (:label plate)) "</title>"
          "<rect x=\"0\" y=\"0\" width=\"" (fmt width-mm) "\" height=\"" (fmt height-mm)
          "\" fill=\"" bg "\"/>"
          (mask-def mask-id plate size)
          (plate-body plate size ink (when (seq (:knockout plate)) mask-id))
          (when reg? (registration-marks size (:margin-mm spec) mark-color))
          "</svg>"))))

;; ---------------------------------------------------------------- 刷り上がり

(defn preview-svg
  "全版をボディ色の上に**刷る順**で重ねた仕上がり予想。

  版そのものではないので製版には渡さない —— 白版がどこまで詰まっているか
  （＝白フチが出ないか）を目で確かめるための絵。"
  ([result] (preview-svg result {}))
  ([{:keys [size spec plates] :as result} {:keys [px-per-mm registration?]
                                           :or {px-per-mm 4}}]
   (let [{:keys [width-mm height-mm]} size
         reg? (if (some? registration?) registration? false)
         ordered (sort-by :order plates)]
     (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
          " width=\"" (fmt (* width-mm px-per-mm)) "\""
          " height=\"" (fmt (* height-mm px-per-mm)) "\""
          " viewBox=\"0 0 " (fmt width-mm) " " (fmt height-mm) "\""
          " role=\"img\" aria-label=\"刷り上がりの予想\">"
          "<title>刷り上がりの予想</title>"
          "<rect x=\"0\" y=\"0\" width=\"" (fmt width-mm) "\" height=\"" (fmt height-mm)
          "\" fill=\"" (:garment-color spec) "\"/>"
          (str/join
           (map-indexed
            (fn [i p]
              (let [mid (str "pv-ko-" i)]
                (str (mask-def mid p size)
                     (plate-body p size (:color p) (when (seq (:knockout p)) mid)))))
            ordered))
          (when reg? (registration-marks size (:margin-mm spec) "#8a8a8a"))
          "</svg>"))))

(defn underbase-check-svg
  "白版が図案からどれだけ内側に詰まっているかを見る図。

  ボディ色の上に、**図案を薄く**・**白版を白で**重ねる。白が図案からはみ出して
  いれば白フチが出る（choke が足りない）、白が痩せすぎていれば下地が効かない。
  数字（choke 0.2mm）を見て判断するのは難しいが、この絵なら一目で分かる。"
  ([result] (underbase-check-svg result {}))
  ([{:keys [size spec plates]} {:keys [px-per-mm] :or {px-per-mm 4}}]
   (let [{:keys [width-mm height-mm]} size
         white (first (filter :underbase? plates))
         spots (filter (complement :underbase?) plates)]
     (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
          " width=\"" (fmt (* width-mm px-per-mm)) "\""
          " height=\"" (fmt (* height-mm px-per-mm)) "\""
          " viewBox=\"0 0 " (fmt width-mm) " " (fmt height-mm) "\""
          " role=\"img\" aria-label=\"白版と図案の重なり\">"
          "<title>白版と図案の重なり（choke の確認）</title>"
          "<rect x=\"0\" y=\"0\" width=\"" (fmt width-mm) "\" height=\"" (fmt height-mm)
          "\" fill=\"" (:garment-color spec) "\"/>"
          ;; 図案（輪郭だけ・マゼンタ）—— 白版がこの線より内側にあるべき
          (when white
            (str (mask-def "ub-ko" white size)
                 (plate-body white size "#ffffff"
                             (when (seq (:knockout white)) "ub-ko"))))
          (str/join
           (map (fn [p]
                  (str "<path d=\"" (geom/contours->d (:art p))
                       "\" fill=\"none\" stroke=\"#ff2d95\" stroke-width=\"0.25\"/>"))
                spots))
          "</svg>"))))

;; ---------------------------------------------------------------- 一括

(defn plate-files
  "全版のファイル名 → SVG のマップ。RIP へ渡す一式。

  ファイル名は `01-white.svg` のように**刷る順が先頭に来る** —— 並べたときに
  順番が崩れないことが、現場で版を取り違えないことに直結する。"
  ([result] (plate-files result {}))
  ([{:keys [plates] :as result} opts]
   (into {}
         (map (fn [p]
                [(str (if (< (:order p) 10) (str "0" (:order p)) (:order p))
                      "-" (name (:id p)) ".svg")
                 (plate-svg result p (merge {:as :film} opts))])
              (sort-by :order plates)))))
