;; ブラウザ側（scittle = ブラウザ内 ClojureScript、ビルド無し）。
;;
;; **版の組み立てはここに書かない。** `shirohan.core` をそのまま読み込んで使う ——
;; JS に書き直すと組版が2実装になり、画面のプレビューと保存した版下が必ず食い違う。
;; ここがやるのは入力の収集・縮小・SVG の差し込み・ダウンロード・助言の往復だけ。
;;
;; NodeList の走査に `array-seq` を使わないこと —— **scittle(SCI) に無い**
;; （inkan で実測: `Could not resolve symbol: array-seq` でページ全体の初期化が
;; 止まった）。NodeList 自身の `forEach` を使う。
(ns shirohan.page-app
  (:require [shirohan.advice :as advice]
            [shirohan.core :as shirohan]
            [shirohan.mockup :as mockup]
            [shirohan.raster :as raster]
            [shirohan.svg :as svg]
            [clojure.string :as str]))

(defn- el [id] (.getElementById js/document id))
(defn- val-of [id] (some-> (el id) .-value))
(defn- status! [msg] (set! (.-textContent (el "status")) msg))

;; 直近の入力。SVG 文字列か、縮小済みの画素かのどちらか。
(def ^:private source (atom {:kind :svg}))
(def ^:private last-job (atom nil))

;; ---------------------------------------------------------------- 入力

(defn- current-spec []
  (let [ko (val-of "f-knockout")]
    {:choke-mm (js/parseFloat (val-of "f-choke"))
     :print-width-mm (js/parseFloat (val-of "f-width"))
     :garment-color (val-of "f-garment")
     :colors (js/parseInt (val-of "f-colors"))
     ;; "none" は「白抜きにしない」の番兵。`""` を使うと `dds/select` が上流どおり
     ;; placeholder（disabled selected）に変えてしまうので使えない。
     :knockout-fill (when-not (or (str/blank? ko) (= "none" ko)) ko)
     :white-underbase? (= "1" (val-of "f-underbase"))}))

(defn- placement-id [] (keyword (or (val-of "f-placement") "chest-center")))

;; ---------------------------------------------------------------- 所見

(def ^:private finding-labels
  "所見の見出し。`shirohan.core` の `:kind` と 1:1 で、**ここで文言を作らない**
  （`:note` はカーネルが持っている）。ここが持つのは重大度の色分けだけ。"
  {:choke-erases-feature "白版から消える"
   :below-min-line "刷ると潰れる可能性"
   :knockout-outside-art "効果のない白抜き"
   :no-art "図案が無い"
   :text-not-outlined "文字がアウトライン化されていない"
   :raster-image "ラスタ画像"
   :use-reference "<use> の参照"
   :unresolvable-fill "版に分解できない塗り"
   :stroke-only "線だけの図形"
   :open-contour "閉じていないパス"
   :photographic-source "写真から起こしている"
   :oversized-raster "画像が大きすぎる"
   :psd-too-large "PSD が大きすぎる"})

(defn- finding-node
  "上流 DADS の notification-banner と**同じ markup** を組む。

  重大度は modifier class ではなく `data-type` 属性で表す（`jp-go-dds.core` の
  実装がそうなっており、class を付けても CSS は当たらない）。アイコンの `<svg>`
  だけは省く —— それを描くにはブラウザに `jp-go-dds.core` を丸ごと送る必要があり、
  装飾のためにページ重量を倍にする取引は割に合わない。見出しテキストは残るので
  意味は失われない。"
  [f]
  (let [blocking? (shirohan/blocking? f)
        banner (.createElement js/document "div")]
    (set! (.-className banner) "dads-notification-banner")
    (.setAttribute banner "data-style" "standard")
    (.setAttribute banner "data-type" (if blocking? "warning" "info-1"))
    (set! (.-innerHTML banner)
          (str "<h3 class='dads-notification-banner__heading'>"
               "<span class='dads-notification-banner__heading-text'>"
               (if blocking? "止める: " "注意: ")
               (get finding-labels (:kind f) (name (:kind f)))
               "</span></h3>"
               "<div class='dads-notification-banner__body'><p>"
               (:note f)
               (when (:measured-mm f) (str "（実測 " (:measured-mm f) "mm）"))
               "</p></div>"))
    banner))

(defn- render-findings! [job]
  (let [box (el "finding-list")
        fs (:findings job)]
    (set! (.-innerHTML box) "")
    (if (empty? fs)
      (set! (.-innerHTML box)
            "<p class='sh-ok'>所見はありません。このまま製版に回せます。</p>")
      (doseq [f fs] (.appendChild box (finding-node f))))))

;; ---------------------------------------------------------------- 版

(defn- plate-node [job p]
  (let [card (.createElement js/document "div")]
    (set! (.-className card) "sh-plate")
    (set! (.-innerHTML card)
          (str (svg/plate-svg job p {:as :film :px-per-mm 2})
               "<p class='sh-plate-name'>"
               "<span class='sh-swatch' style='background:" (:color p) "'></span>"
               (:order p) ". " (:label p) "</p>"
               "<p class='sh-note'>面積 "
               (.toFixed (/ (:area-mm2 p) 100.0) 1) " cm²</p>"))
    card))

(defn- render-palette!
  "画像から拾った色。**版の一覧とは別に出す** —— 「4色でと言ったのに3版しか
  出ない」ときの理由（図案が3色だった／混色を1色に寄せた）がここに出る。"
  [job]
  (let [box (el "palette")
        pal (:palette job)]
    (set! (.-innerHTML box)
          (if (empty? pal)
            ""
            (str "<span class='sh-note'>拾った色:</span>"
                 (apply str
                        (map (fn [c]
                               (str "<span class='sh-chip'>"
                                    "<span class='sh-swatch' style='background:" c "'></span>"
                                    c "</span>"))
                             pal)))))))

(defn- render-plates! [job]
  (let [box (el "plate-list")]
    (set! (.-innerHTML box) "")
    (doseq [p (sort-by :order (:plates job))]
      (.appendChild box (plate-node job p)))))

;; ---------------------------------------------------------------- 描画

(defn- build-job []
  (let [spec (current-spec)
        s @source]
    (if (= :image (:kind s))
      (shirohan/plan-image (:image s) spec)
      (shirohan/plan (val-of "f-svg") spec))))

(defn- render! []
  (try
    (let [job (build-job)]
      (reset! last-job job)
      (set! (.-innerHTML (el "pv-print")) (shirohan/preview job))
      (set! (.-innerHTML (el "pv-underbase")) (shirohan/underbase-check job))
      (set! (.-innerHTML (el "pv-mockup"))
            (shirohan/mockup job {:placement-id (placement-id)}))
      (set! (.-textContent (el "mockup-note")) (mockup/print-size-note job {}))
      (render-palette! job)
      (render-plates! job)
      (render-findings! job)
      (let [s (shirohan/summary job)]
        (status! (str (:plate-count s) " 版 / "
                      (.toFixed (:width-mm (:size s)) 0) "×"
                      (.toFixed (:height-mm (:size s)) 0) "mm / choke "
                      (:choke-mm s) "mm"
                      (if (pos? (:blocking s))
                        (str " —— 止めるべき所見が " (:blocking s) " 件あります")
                        " —— 止めるべき所見はありません")))))
    (catch :default e
      (status! (str "図案を読めませんでした: " (.-message e))))))

;; ---------------------------------------------------------------- 画像の取り込み

(defn- downscale
  "画像を長辺 `max-side` まで縮めて RGBA を取り出す。

  **縮めるのは canvas の仕事**（面積平均で綺麗に縮む）。`shirohan.raster` は
  縮小を持たない —— ブラウザ内の SCI で 100 万画素のループは実用にならないし、
  そもそもこの道具が扱うのは少数色のロゴ・イラストで、192px から起こした輪郭で
  版に足りる（`shirohan.raster` の ns doc）。"
  [img max-side]
  (let [w0 (.-naturalWidth img) h0 (.-naturalHeight img)
        k (min 1.0 (/ max-side (max w0 h0)))
        w (max 1 (js/Math.round (* w0 k)))
        h (max 1 (js/Math.round (* h0 k)))
        c (.createElement js/document "canvas")]
    (set! (.-width c) w)
    (set! (.-height c) h)
    (let [ctx (.getContext c "2d")]
      (set! (.-imageSmoothingEnabled ctx) true)
      (.drawImage ctx img 0 0 w h)
      {:width w :height h :data (.-data (.getImageData ctx 0 0 w h))})))

(defn- load-image! [data-url]
  (let [img (js/Image.)]
    (set! (.-onload img)
          (fn [_]
            (status! "画像から輪郭を起こしています…")
            ;; 描画を1フレーム進めてから重い処理に入る（そうしないと
            ;; 「起こしています」が表示されないまま固まって見える）。
            (js/setTimeout
             (fn []
               (reset! source {:kind :image
                               :image (downscale img (:max-side raster/default-opts))
                               :thumb data-url})
               (render!))
             30)))
    (set! (.-onerror img) (fn [_] (status! "画像を読めませんでした。")))
    (set! (.-src img) data-url)))

(defn- read-file! [file]
  (let [svg? (or (= "image/svg+xml" (.-type file))
                 (str/ends-with? (str/lower-case (.-name file)) ".svg"))
        r (js/FileReader.)]
    (set! (.-onload r)
          (fn [_]
            (if svg?
              (do (reset! source {:kind :svg})
                  (set! (.-value (el "f-svg")) (.-result r))
                  (render!))
              (load-image! (.-result r)))))
    (set! (.-onerror r) (fn [_] (status! "ファイルを読めませんでした。")))
    (if svg? (.readAsText r file) (.readAsDataURL r file))))

;; ---------------------------------------------------------------- 保存

(defn- download! [href filename]
  (let [a (.createElement js/document "a")]
    (set! (.-href a) href)
    (set! (.-download a) filename)
    (.appendChild (.-body js/document) a)
    (.click a)
    (.removeChild (.-body js/document) a)))

(defn- download-blob! [blob filename]
  (let [url (.createObjectURL js/URL blob)]
    (download! url filename)
    (js/setTimeout #(.revokeObjectURL js/URL url) 4000)))

(defn- download-text! [text filename mime]
  (download-blob! (js/Blob. #js [text] #js {:type mime}) filename))

(defn- download-bytes!
  "0-255 の整数ベクタを Uint8Array にしてから落とす。"
  [bytes filename mime]
  (let [arr (js/Uint8Array. (count bytes))]
    (dotimes [i (count bytes)] (aset arr i (nth bytes i)))
    (download-blob! (js/Blob. #js [arr] #js {:type mime}) filename)))

(defn- with-job [f]
  (if-let [job @last-job] (f job) (status! "先に図案を読み込んでください。")))

(defn- save-films! []
  (with-job
    (fn [job]
      ;; zip は作らない（依存を増やさずに済む形を選ぶ）。版は数枚なので
      ;; 1 枚ずつ落とす方が、現場での取り違えも起きにくい。
      (let [files (shirohan/films job)]
        (doseq [[name svg] (sort-by key files)]
          (download-text! svg name "image/svg+xml;charset=utf-8"))
        (status! (str (count files) " 枚の版下を保存しました（白地に黒・刷る順の連番）"))))))

(defn- save-ai! []
  (with-job
    (fn [job]
      (download-bytes! (shirohan/ai-pdf job) "shirohan.ai" "application/pdf")
      (status! "AI（PDF ベース）を保存しました。1 ページ目が刷り上がり予想、以降が刷る順の版です"))))

(defn- save-psd! []
  (with-job
    (fn [job]
      (status! "PSD を組んでいます…")
      (js/setTimeout
       (fn []
         ;; ブラウザ（SCI インタプリタ）で回すので解像度は控えめに。
         ;; 高解像度が要るなら CLI（nbb）で `shirohan.core/photoshop` を直接呼ぶ。
         (let [{:keys [bytes findings width height]} (shirohan/photoshop job {:px-per-mm 2.0})]
           (if (nil? bytes)
             (status! (str "PSD を書き出せませんでした: " (:note (first findings))))
             (do (download-bytes! bytes "shirohan.psd" "image/vnd.adobe.photoshop")
                 (status! (str "PSD を保存しました（" width "×" height "px、1 版 = 1 レイヤー）"))))))
       30))))

(defn- save-mockup! []
  (with-job
    (fn [job]
      (download-text! (shirohan/mockup job {:placement-id (placement-id)})
                      "shirohan-mockup.svg" "image/svg+xml;charset=utf-8")
      (status! "着用イメージを保存しました（版下ではありません）"))))

;; ---------------------------------------------------------------- おまかせ

(defn- set-select! [id v]
  (when-let [n (el id)] (set! (.-value n) (str v))))

(defn- apply-advice! [decided palette]
  (let [{:keys [spec rationale rejected]} (advice/apply-advice decided palette)]
    (when (:colors spec) (set-select! "f-colors" (:colors spec)))
    (when (:garment-color spec) (set-select! "f-garment" (:garment-color spec)))
    (when (:choke-mm spec) (set-select! "f-choke" (:choke-mm spec)))
    (when (:placement-id spec) (set-select! "f-placement" (name (:placement-id spec))))
    (when (contains? spec :knockout-fill)
      (set-select! "f-knockout" (or (:knockout-fill spec) "none")))
    (render!)
    (status! (str "おまかせを適用しました"
                  (when rationale (str " —— " rationale))
                  (when (seq rejected)
                    (str "（採用しなかったもの: "
                         (str/join "、" (map #(str (name (:field %)) "＝" (:reason %)) rejected))
                         "）"))))))

(defn- advise! []
  (with-job
    (fn [job]
      (let [palette (or (:palette job)
                        (vec (distinct (map :color (remove :underbase? (:plates job))))))]
        (status! "murakumo に相談しています…")
        (-> (js/fetch "/api/shirohan/advise"
                      #js {:method "POST"
                           :headers #js {"Content-Type" "application/json"}
                           :body (js/JSON.stringify
                                  #js {:palette (clj->js palette)
                                       :note (or (val-of "f-note") "")})})
            (.then (fn [r] (.then (.json r) (fn [b] [(.-status r) b]))))
            (.then (fn [[st b]]
                     (let [m (js->clj b)]
                       (cond
                         (= 503 st) (status! (str "おまかせは今使えません（"
                                                  (get m "note" "未設定") "）。"
                                                  "手動の設定でそのまま版は作れます。"))
                         (not= 200 st) (status! (str "おまかせに失敗しました: "
                                                     (get m "reason" st)))
                         :else (apply-advice! (get m "advice") palette)))))
            (.catch (fn [e] (status! (str "おまかせに失敗しました: " (.-message e))))))))))

;; ---------------------------------------------------------------- 配線

(defn- on-act! [act f]
  (.forEach (.querySelectorAll js/document (str "[data-act='" act "']"))
            (fn [b] (.addEventListener b "click" (fn [_] (f))))))

(defn- wire! []
  (doseq [id ["f-choke" "f-width" "f-garment" "f-knockout" "f-underbase"
              "f-colors" "f-placement"]]
    (when-let [node (el id)]
      (.addEventListener node "change" (fn [_] (render!)))))
  ;; SVG のソースは打つたびに走らせない —— 大きな図案では折れ線化が重く、
  ;; 1 文字ごとの再計算で入力が詰まる。貼り終わり（change）と明示の再計算で回す。
  (when-let [ta (el "f-svg")]
    (.addEventListener ta "change" (fn [_] (reset! source {:kind :svg}) (render!))))
  (when-let [f (el "f-file")]
    (.addEventListener f "change"
                       (fn [e] (when-let [file (aget (.. e -target -files) 0)]
                                 (read-file! file)))))
  (on-act! "rerender" render!)
  (on-act! "advise" advise!)
  (on-act! "save-films" save-films!)
  (on-act! "save-ai" save-ai!)
  (on-act! "save-psd" save-psd!)
  (on-act! "save-mockup" save-mockup!))

(wire!)
(render!)
