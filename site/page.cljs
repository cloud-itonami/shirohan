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
            [shirohan.color :as color]
            [shirohan.geom :as geom]
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

(def ^:private view-mode (atom :white))

(defn- current-spec []
  (let [ko (val-of "f-knockout")
        cut (js/parseFloat (or (val-of "f-cut") "3"))]
    {:choke-mm (js/parseFloat (val-of "f-choke"))
     :print-width-mm (js/parseFloat (val-of "f-width"))
     :garment-color (val-of "f-garment")
     :colors (js/parseInt (val-of "f-colors"))
     ;; "none" は「白抜きにしない」の番兵。`""` を使うと `dds/select` が上流どおり
     ;; placeholder（disabled selected）に変えてしまうので使えない。
     :knockout-fill (when-not (or (str/blank? ko) (= "none" ko)) ko)
     :white-underbase? (= "1" (val-of "f-underbase"))
     :cut-line? (pos? cut)
     :cut-margin-mm cut
     :separate-colors? (= "1" (val-of "f-sep"))}))

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
   :psd-too-large "PSD が大きすぎる"
   :opaque-background-assumed "地を画像の縁から判定しました"
   :enclosed-background-region "囲まれた同色領域があります"})

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
               "<p class='sh-cmyk'>" (color/cmyk->label (:cmyk p)) "</p>"
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

;; SCI は前方参照を解決しないので、後ろで定義するものはここで宣言しておく。
(declare downscale silhouette-side run-seg!)

(defn- build-job []
  (let [spec (current-spec)
        s @source]
    (if (= :image (:kind s))
      ;; **白版だけは `f-res` の解像度で追い直す。** 元の `img` を持っておいて
      ;; ここで縮小し直すので、解像度を変えたら読み込み直さずに効く（以前は
      ;; `:silhouette-image` をどこにも入れておらず、この選択が無効だった）。
      ;; AI 切り抜きを使っているときは、そのマスクが白版のもとになる。
      (shirohan/plan-image (:image s)
                           (assoc spec :silhouette-image
                                  (or (:seg-image s)
                                      (when-let [i (:img s)]
                                        (downscale i (silhouette-side))))))
      (shirohan/plan (val-of "f-svg") spec))))

(defn- cut-preview
  "カットラインの確認図。ボディ色の上に、版と**断裁線（シアン）**を重ねる。

  線の色は Illustrator の慣習に合わせてシアン系にする —— 版下の黒と混ざらず、
  かつ『これは刷らない線だ』と一目で分かる。"
  [job]
  (let [size (:size job)
        d (geom/contours->d (shirohan/cut-contours job))]
    (str (subs (shirohan/preview job) 0 (- (count (shirohan/preview job)) 6))
         (when-not (str/blank? d)
           (str "<path d=\"" d "\" fill=\"none\" stroke=\"#00a3e0\""
                " stroke-width=\"" (max 0.3 (/ (:width-mm size) 400.0)) "\"/>"))
         "</svg>")))

(defn- white-plate [job]
  (first (filter :underbase? (:plates job))))

(defn- white-film
  "**この道具の成果物。** 図案があった場所を黒くベタ塗りした版下（白地に黒）。

  感光乳剤は光の有無しか見ないので、インクの色を付けても意味がない —— だから
  『白版』は白く塗らず黒で出す。刷るときにここへ白インクが載る。"
  [job]
  (if-let [w (white-plate job)]
    (svg/plate-svg job w {:as :film :px-per-mm 3})
    "<p>白版がありません（細かい設定で「白版を作る」を選んでください）</p>"))

(defn- render-stage! [job]
  (set! (.-innerHTML (el "stage"))
        (case @view-mode
          :print (shirohan/preview job)
          :underbase (shirohan/underbase-check job)
          :cut (cut-preview job)
          :mockup (shirohan/mockup job {:placement-id (placement-id)})
          (white-film job))))

(defn- render! []
  (try
    (let [job (build-job)]
      (reset! last-job job)
      (render-stage! job)
      (render-palette! job)
      (render-plates! job)
      (render-findings! job)
      (let [s (shirohan/summary job)]
        (status! (str (mockup/print-size-note job {}) " / "
                      (:plate-count s) " 版 / "
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

(defn- silhouette-side []
  (js/parseInt (or (val-of "f-res") "768")))

(defn- load-image! [data-url]
  (let [img (js/Image.)]
    (set! (.-onload img)
          (fn [_]
            (status! (str "画像から輪郭を起こしています…（白版は "
                          (silhouette-side) "px で追うので数秒かかります）"))
            ;; 描画を1フレーム進めてから重い処理に入る（そうしないと
            ;; 「起こしています」が表示されないまま固まって見える）。
            (js/setTimeout
             (fn []
               (reset! source {:kind :image
                               :img img
                               :url data-url
                               :image (downscale img (:max-side raster/default-opts))
                               :thumb data-url})
               (render!)
               (when (not= "none" (or (val-of "f-seg") "none")) (run-seg!)))
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

(defn- save-white! []
  (with-job
    (fn [job]
      (if-let [w (white-plate job)]
        (do (download-text! (svg/plate-svg job w {:as :film})
                            "shirohan-white.svg" "image/svg+xml;charset=utf-8")
            (status! (str "白版を保存しました（白地に黒、"
                          (.toFixed (/ (:area-mm2 w) 100.0) 1) " cm²）")))
        (status! "白版がありません（細かい設定で「白版を作る」を選んでください）")))))

(defn- save-cut! []
  (with-job
    (fn [job]
      (let [cs (shirohan/cut-contours job)
            size (:size job)]
        (if (empty? cs)
          (status! "カットラインが無効です（細かい設定で「作らない」以外を選んでください）")
          (do (download-text!
               (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
                    " width=\"" (:width-mm size) "mm\" height=\"" (:height-mm size) "mm\""
                    " viewBox=\"0 0 " (:width-mm size) " " (:height-mm size) "\">"
                    "<path d=\"" (geom/contours->d cs) "\" fill=\"none\""
                    " stroke=\"#00a3e0\" stroke-width=\"0.25\"/></svg>")
               "cutline.svg" "image/svg+xml;charset=utf-8")
              (status! (str "カットラインを保存しました（外側 "
                            (get-in job [:cut :margin-mm]) "mm、輪郭 "
                            (count cs) " 本）"))))))))

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
        (status! "murakumo に相談しています… 推論モデルなので 40 秒〜2 分かかります（この間も手動の設定でそのまま版は作れます）")
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

(defn- run-seg!
  "AI 切り抜きを走らせ、結果のマスクを白版のもとにする。

  透明度の閾値と縁からの塗りつぶしは、白背景・写真・アンチエイリアスに弱い。
  そこだけ学習済みモデルに任せ、**輪郭追跡から先の版下の処理は一切変えない** ——
  マスクの作り方を差し替えるだけの継ぎ目にしてある。モデルはブラウザの中で
  動くので、画像は送信されない。"
  []
  (let [m (or (val-of "f-seg") "none")
        s @source]
    (cond
      (not= :image (:kind s)) nil
      (= "none" m) (do (swap! source dissoc :seg-image :seg-ms) (render!))
      (nil? (.-shirohanSeg js/window)) (status! "切り抜きモデルを読み込めませんでした。")
      :else
      (-> (js/window.shirohanSeg (:url s) m (silhouette-side) status!)
          (.then (fn [r]
                   (swap! source assoc
                          :seg-image {:width (.-w r) :height (.-h r) :data (.-data r)}
                          :seg-ms (.-ms r))
                   (render!)
                   (status! (str "切り抜き完了（" m "・" (.-ms r) "ms）。"
                                 "モデルを切り替えると同じ図案で見比べられます。"))))
          (.catch (fn [e] (status! (str "切り抜きに失敗しました: " (.-message e)))))))))

(defn- on-act! [act f]
  (.forEach (.querySelectorAll js/document (str "[data-act='" act "']"))
            (fn [b] (.addEventListener b "click" (fn [_] (f))))))

(defn- wire! []
  (doseq [id ["f-choke" "f-width" "f-garment" "f-knockout" "f-underbase"
              "f-colors" "f-placement" "f-cut" "f-res" "f-sep"]]
    (when-let [node (el id)]
      (.addEventListener node "change" (fn [_] (render!)))))
  ;; 切り抜きは重いので、選び直したときだけ走らせる。
  (when-let [node (el "f-seg")]
    (.addEventListener node "change" (fn [_] (run-seg!))))
  ;; SVG のソースは打つたびに走らせない —— 大きな図案では折れ線化が重く、
  ;; 1 文字ごとの再計算で入力が詰まる。貼り終わり（change）と明示の再計算で回す。
  (when-let [ta (el "f-svg")]
    (.addEventListener ta "change" (fn [_] (reset! source {:kind :svg}) (render!))))
  (when-let [f (el "f-file")]
    (.addEventListener f "change"
                       (fn [e] (when-let [file (aget (.. e -target -files) 0)]
                                 (read-file! file)))))
  ;; 表示の切り替え（セグメント）
  (.forEach (.querySelectorAll js/document "[data-view]")
            (fn [b]
              (.addEventListener
               b "click"
               (fn [_]
                 (reset! view-mode (keyword (.getAttribute b "data-view")))
                 (.forEach (.querySelectorAll js/document "[data-view]")
                           (fn [o] (.setAttribute o "aria-selected"
                                                  (if (= o b) "true" "false"))))
                 (when-let [job @last-job] (render-stage! job))))))
  (on-act! "rerender" render!)
  (on-act! "advise" advise!)
  (on-act! "save-white" save-white!)
  (on-act! "save-films" save-films!)
  (on-act! "save-ai" save-ai!)
  (on-act! "save-psd" save-psd!)
  (on-act! "save-cut" save-cut!)
  (on-act! "save-mockup" save-mockup!))

(wire!)
(render!)
