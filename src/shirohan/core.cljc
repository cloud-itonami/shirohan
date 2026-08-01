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

  `image` は `{:width :height :data}`（RGBA が 4 バイトずつ）。呼び出し側で
  `shirohan.raster/default-opts` の `:max-side` まで縮小してから渡すこと。

  spec は `plan` と同じものに加えて `:colors`（版の数）を取る。白抜きの判定は
  SVG 経路と同じで、パレットに出た白（`:knockout-fill`）が穴になる。"
  ([image] (plan-image image {}))
  ([image spec]
   (let [spec (merge default-spec {:colors 4} spec)
         traced (raster/trace image {:colors (:colors spec)
                                     :alpha-min (get spec :alpha-min 128)})
         ko (some-> (:knockout-fill spec) str/lower-case)
         tagged (mapv #(assoc % :role (if (and ko (= ko (:fill %))) :knockout :art))
                      (:contours traced))
         ;; シルエット（インクが載る面）も一緒に拡縮する —— **同じ変換**を
         ;; かけないと白版と色版の見当が合わなくなるので、1 回の fit に混ぜる。
         all (into tagged (:silhouette traced))
         {:keys [contours bbox]} (fit-to-print-width all (:print-width-mm spec))
         art {:contours contours :bbox bbox :scale 1.0
              :findings (:findings traced)}]
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
