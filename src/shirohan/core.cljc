(ns shirohan.core
  "入口。SVG 文字列 + spec → 版一式。

  ```clojure
  (require '[shirohan.core :as shirohan])

  (def job (shirohan/plan svg-string {:choke-mm 0.2 :print-width-mm 260}))

  (:findings job)                    ;; 刷る前に潰すべき問題
  (map :label (:plates job))         ;; (\"白版（下地）\" \"スポット版 #FF2D95\" …)
  (shirohan/films job)               ;; {\"01-white.svg\" \"<svg…>\" …}
  ```

  `plan` は**純関数**。同じ SVG と同じ spec からは必ず同じ版が出るので、
  版下を差分でレビューできるし、承認した版と刷った版が同じであることを
  ハッシュで示せる。"
  (:require [clojure.string :as str]
            [shirohan.artwork :as artwork]
            [shirohan.geom :as geom]
            [shirohan.mockup :as mockup]
            [shirohan.pdf :as pdf]
            [shirohan.plate :as plate]
            [shirohan.psd :as psd]
            [shirohan.raster :as raster]
            [shirohan.svg :as svg]))

(def default-spec plate/default-spec)

(defn plan
  "SVG 文字列 → `shirohan.plate/separate` の結果（`:artwork` を添えて返す）。"
  ([svg-string] (plan svg-string {}))
  ([svg-string spec]
   (let [spec (merge default-spec spec)
         art (artwork/load-svg svg-string
                               {:print-width-mm (:print-width-mm spec)
                                :tolerance-mm (:tolerance-mm spec)
                                :knockout-fill (:knockout-fill spec)})]
     (assoc (plate/separate art spec) :artwork art))))

(defn- fit-to-print-width
  "輪郭群を左上原点・指定幅 mm に等方拡縮する。`artwork/load-svg` が SVG に対して
  やっているのと同じことを、ラスタ由来の輪郭に対してやる。"
  [contours print-width-mm]
  (let [bb (geom/bbox contours)]
    (if (nil? bb)
      {:contours [] :bbox nil}
      (let [w (max 1e-9 (- (:x1 bb) (:x0 bb)))
            s (/ print-width-mm w)
            mv (fn [[x y]] [(* s (- x (:x0 bb))) (* s (- y (:y0 bb)))])
            cs (mapv #(update % :points (fn [ps] (mapv mv ps))) contours)]
        {:contours cs :bbox (geom/bbox cs)}))))

(defn plan-image
  "**画素から直接**版一式を作る（画像を上げるだけで白版まで出す経路）。

  `image` は `{:width :height :data}`（RGBA が 4 バイトずつ）。

  ## 解像度は白版に振る

  `spec` に `:silhouette-image` を渡すと、**白版のもとだけをそちらの高解像度で
  追う**（`shirohan.raster/trace-silhouette`）。色版は `image` の解像度のまま。

  白版が成果物で、色版は版ずれの確認用なので、限られた計算をどちらに使うかは
  はっきりしている。シルエット専用経路は量子化も色ごとの追跡もしないので、
  同じ時間で 2〜3 倍の辺長を扱える（実測 2026-08-01、SCI:
  透明 PNG は 320px 210ms / 768px 1.1s、白背景は 320px 543ms / 768px 3.1s）。

  **座標系を揃えてから合流させる**のが要点 —— 高解像度の点をそのまま混ぜると、
  白版と色版が別の尺度になって見当が合わない。"
  ([image] (plan-image image {}))
  ([image spec]
   (let [spec (merge default-spec {:colors 4} spec)
         hi (:silhouette-image spec)
         ;; **色版は既定で作らない。** 成果物は白版で、色版は版ずれの確認用。
         ;; 色の量子化と色ごとの輪郭追跡はこの経路でいちばん重い処理なので、
         ;; 要らない人に払わせない（実測 2026-08-01、ブラウザ: 色版ありで 20 秒、
         ;; 白版だけなら 2〜4 秒）。
         separate? (get spec :separate-colors? false)
         traced (if separate?
                  (raster/trace image {:colors (:colors spec)
                                       :alpha-min (get spec :alpha-min 128)})
                  {:contours [] :silhouette [] :findings [] :palette []})
         ko (some-> (:knockout-fill spec) str/lower-case)
         tagged (mapv #(assoc % :role (if (and ko (= ko (:fill %))) :knockout :art))
                      (:contours traced))
         ;; **白版のもとは常に専用経路で追う。** 色版を作るかどうかと独立
         ;; （色版は任意、白版は成果物）。`:silhouette-image` があればそちらを
         ;; 使い、無ければ `image` をそのまま。
         src (or hi image)
         hi-traced (raster/trace-silhouette src {:alpha-min (get spec :alpha-min 128)})
         ;; 高解像度側の点を `image` の座標系へ落としてから混ぜる —— 尺度が
         ;; 混ざると白版と色版の見当が合わない。
         k (/ (double (:width image)) (double (:width src)))
         silhouette (if (= 1.0 k)
                      (:contours hi-traced)
                      (mapv #(update % :points
                                     (fn [ps] (mapv (fn [[x y]] [(* k x) (* k y)]) ps)))
                            (:contours hi-traced)))
         all (into tagged silhouette)
         {:keys [contours bbox]} (fit-to-print-width all (:print-width-mm spec))
         art {:contours contours :bbox bbox :scale 1.0
              :findings (into (vec (:findings traced)) (:findings hi-traced))}]
     (assoc (plate/separate art spec)
            :artwork art
            :palette (:palette traced)))))

(defn films
  "版一式を「ファイル名 → SVG」で返す（製版に渡す形）。"
  ([job] (svg/plate-files job))
  ([job opts] (svg/plate-files job opts)))

(defn cut-contours
  "カットライン（断裁線）の輪郭。無ければ空。"
  [job]
  (get-in job [:cut :contours] []))

(defn preview [job] (svg/preview-svg job))

(defn underbase-check [job] (svg/underbase-check-svg job))

(defn mockup
  "Tシャツに刷った着用イメージ（位置と大きさの確認用）。"
  ([job] (mockup/mockup-svg job))
  ([job opts] (mockup/mockup-svg job opts)))

(defn ai-pdf
  "PDF バイト列。`.ai` として保存すれば Illustrator が開ける（PDF ベース）。"
  [job]
  (pdf/job->pdf job))

(defn photoshop
  "PSD（1 版 = 1 レイヤー）。`{:bytes :width :height :findings}`。"
  ([job] (psd/write job))
  ([job opts] (psd/write job opts)))

(defn blocking?
  "刷る前に必ず人が見るべき所見か。

  白版が消える・図案が無い・版に分解できない塗りがある、は**刷れない**。
  細り（`:below-min-line`）は刷れるが潰れる可能性があるという注意なので、
  ここには含めない —— 止めるべきものだけを止める。"
  [{:keys [kind]}]
  (contains? #{:choke-erases-feature :no-art :unresolvable-fill
               :text-not-outlined :raster-image :use-reference}
             kind))

(defn summary
  "承認・監査に載せる要約。版の一覧、面積、止めるべき所見の数。"
  [{:keys [plates findings size spec]}]
  {:plate-count (count plates)
   :plates (mapv #(select-keys % [:id :label :color :order :area-mm2]) plates)
   :size size
   :choke-mm (:choke-mm spec)
   :print-width-mm (:print-width-mm spec)
   :findings (count findings)
   :blocking (count (filter blocking? findings))})
