(ns shirohan.page
  "公開サイト `itonami.cloud/cloud-itonami/shirohan/` のマークアップ。

  `src/` ではなく `site/` に置いてあるのは、**ライブラリ本体を依存ゼロに保つ**ため
  —— `shirohan.{geom,path,artwork,plate,svg,core}` は `clojure.string` しか引かない
  ので、design system を引くこの ns を同じ `:paths` に入れたくない。deps.edn の
  `:site` alias でだけ classpath に載る。

  ## なぜ kotoba-ui ではなく DADS（デジタル庁デザインシステム）なのか

  モノレポ標準の UI スタックは kotoba-ui（skill `kotoba-uiux` / ADR-2607122200）で、
  DADS はその明示的な opt-out 先。**ここで opt-out する理由は「白版が行政文脈だから」
  ではない**（版下は商業印刷そのもので、行政とは関係がない）。理由は公開面の構造:

  `itonami.cloud/{org}/{repo}` に載るサイトには、プラットフォーム側が共通クローム
  （ヘッダー・パンくず・フッター）を**注入する**（`cloud-itonami.site.service-page`、
  ADR-2607301300）。そのパンくずは上流 DADS の `dads-breadcrumb` CSS を要求し、
  渡さないと例外で生成が止まる —— つまり**この公開面自体が DADS 面**。ここに
  kotoba-ui のページを置くと、1 画面に 2 つの design system のトークン体系が同居し、
  注入されたパンくずは DADS トークン（`--color-neutral-*`）を失って素の `ol` に近づく。
  先行 2 サイト（inkan / rirekisho）も DADS。**面の一貫性を design system の
  既定より優先する**、という判断。

  結果として変わること（inkan の判断と同じ）:

  - **light mode 固定になる。** 上流デジタル庁に dark palette が無いので、DADS を
    選ぶことは dark を捨てることと同義。dark が要るなら kotoba-ui に戻すのが正しい
    分岐であって、dark を自作しない。
  - class 語彙が `dads-*`（上流忠実）+ `dds-ext-*`（layout 補助）になる。
  - **外部リクエストは scittle だけ**。webfont も画像も引かない。"
  (:require [clojure.string :as str]
            [jp-go-dds.core :as dds]
            [jp-go-dds.page :as dds-page]
            [shirohan.mockup :as mockup]))

(def ^:private sample-svg
  "初期表示に使う図案。**外部から取ってこない** —— ページを開いた瞬間に
  何が起きる道具なのか分かる必要があり、そのために通信を足したくない。

  白抜き（白い星）と2色を含むので、白版・knockout・版分解の全部が一度に見える。"
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 240 160'>"
       "<rect x='0' y='0' width='240' height='160' rx='18' fill='#1d4ed8'/>"
       "<circle cx='68' cy='80' r='44' fill='#ff2d95'/>"
       "<path fill='#ffffff' d='M68 44 L79 70 L107 72 L86 90 L92 118"
       " L68 103 L44 118 L50 90 L29 72 L57 70 Z'/>"
       "<path fill='#ffd400' d='M126 46 h86 v22 h-86 Z M126 78 h64 v22 h-64 Z"
       " M126 110 h86 v22 h-86 Z'/>"
       "</svg>"))

(def ^:private garment-colors
  "ボディ色の候補。**版ではなく確認用の背景**なので、実際のボディ色に厳密である
  必要はない —— 白版が効いているかどうかが見える濃さであればよい。"
  [["#1f1f1f" "ブラック"]
   ["#1d3a5c" "ネイビー"]
   ["#5c1f2e" "バーガンディ"]
   ["#2f4f34" "オリーブ"]
   ["#8a8a8a" "グレー"]
   ["#f2f2f2" "ホワイト"]])

(def ^:private chokes
  [["0" "0.0mm（かけない）"]
   ["0.1" "0.1mm（細い図案・高精度機）"]
   ["0.15" "0.15mm"]
   ["0.2" "0.2mm（既定）"]
   ["0.3" "0.3mm（手刷り・見当が甘い環境）"]
   ["0.5" "0.5mm（厚地・大判）"]])

(def ^:private widths
  [["120" "120mm（ワンポイント）"]
   ["180" "180mm（胸中央）"]
   ["220" "220mm"]
   ["260" "260mm（前身頃・標準）"]
   ["280" "280mm（前身頃・上限）"]])

;; ---------------------------------------------------------------- 入力

(defn- controls []
  (dds/card
   ;; `dds/card` は class opts を取らないので、CSS hook は内側の div で持つ
   ;; （上流 class を app CSS で上書きしないための包み）。
   [:div {:class "sh-form"}
    (dds/stack
     (dds/form-field
      {:label "図案（SVG / PNG / JPEG）" :for "f-file" :support-id "f-file-support"
       :support "画像を選ぶだけで、色を拾って輪郭を起こし、白版まで作ります。ファイルは送信されません。"}
      [:input {:type "file" :id "f-file" :name "file"
               :accept ".svg,image/svg+xml,image/png,image/jpeg,image/webp"
               :class "sh-file" :aria-describedby "f-file-support"}])

     (dds/form-field
      {:label "版の数（画像から起こすとき）" :for "f-colors" :support-id "f-colors-support"
       :support "少ないほど安く刷れます。白抜きに使う白もこの数に含まれます。"}
      (dds/select {:id "f-colors" :name "colors" :value "4"
                   :aria-describedby "f-colors-support"}
                  [["2" "2色"] ["3" "3色"] ["4" "4色"] ["5" "5色"] ["6" "6色"]]))

     (dds/form-field
      {:label "SVG のソース" :for "f-svg" :support-id "f-svg-support"
       :support "ここに直接貼っても構いません。文字はアウトライン化してから入稿してください。"}
      ;; `dds/textarea` は opts しか取らず**初期内容を渡せない**（HTML の textarea は
      ;; value 属性ではなく子要素で中身を持つ）。上流と同じ markup を手で組む。
      [:span {:class "dads-textarea"}
       [:textarea {:id "f-svg" :name "svg" :rows "6"
                   :class "dads-textarea__textarea sh-source"
                   :spellcheck "false" :aria-describedby "f-svg-support"}
        sample-svg]])

     ;; DADS の `select` は **`[opts options]`** の順（`[options opts]` ではない）。
     ;; 逆に渡すと opts が options として map され、`<option value=":id">f-choke</option>`
     ;; のような選択肢が黙って出る。`:value` を必ず渡す —— 渡さないと HTML 上は
     ;; **先頭の option が選ばれる**ので、見た目は正常なまま既定値がずれる。
     (dds/form-field
      {:label "choke（縮み代）" :for "f-choke" :support-id "f-choke-support"
       :support "白版を図案より内側に詰める量。見当が甘い環境ほど大きく取ります。"}
      (dds/select {:id "f-choke" :name "choke" :value "0.2"
                   :aria-describedby "f-choke-support"}
                  chokes))

     (dds/form-field
      {:label "刷り幅" :for "f-width"}
      (dds/select {:id "f-width" :name "width" :value "260"} widths))

     (dds/form-field
      {:label "ボディ色" :for "f-garment" :support-id "f-garment-support"
       :support "確認用の背景です。版そのものには含まれません。"}
      (dds/select {:id "f-garment" :name "garment" :value "#1f1f1f"
                   :aria-describedby "f-garment-support"}
                  garment-colors))

     (dds/form-field
      {:label "白抜きとして扱う塗り" :for "f-knockout" :support-id "f-ko-support"
       :support "既定は白。濃色ボディでは「生地を見せる穴」であることが多いためです。"}
      (dds/select {:id "f-knockout" :name "knockout" :value "#ffffff"
                   :aria-describedby "f-ko-support"}
                  ;; 「無効」に `""` を使わないこと —— `dds/select` は値が空の
                  ;; option を**上流どおり placeholder として `disabled selected`**
                  ;; にするので、無効側が最初から選ばれた上に選べなくなる（実測）。
                  [["#ffffff" "白（#ffffff）を白抜きにする"]
                   ["none" "白抜きにしない（白もインクとして刷る）"]]))

     (dds/form-field
      {:label "白版" :for "f-underbase"}
      (dds/select {:id "f-underbase" :name "underbase" :value "1"}
                  [["1" "白版を作る"] ["0" "作らない（淡色ボディ）"]]))

     (dds/form-field
      {:label "刷り位置" :for "f-placement"}
      (dds/select {:id "f-placement" :name "placement" :value "chest-center"}
                  (mapv (fn [{:keys [id label]}] [(name id) label]) mockup/placements)))

     (dds/form-field
      {:label "おまかせに伝えること（任意）" :for "f-note" :support-id "f-note-support"
       :support "「落ち着いた色で」「予算優先で版を減らして」など。空でも構いません。判断は推論モデルが行うため、押してから 40 秒〜2 分かかります。"}
      (dds/input-text {:id "f-note" :name "note" :aria-describedby "f-note-support"}))

     (dds/row
      (dds/button "おまかせで決める" {:type :outline :attrs {:data-act "advise"}})
      (dds/button "もう一度組む" {:type :text :attrs {:data-act "rerender"}})))]))

;; ---------------------------------------------------------------- 出力

(defn- previews []
  (dds/card
   (dds/stack
    [:div {:class "sh-previews"}
     [:figure {:class "sh-fig sh-fig-wide"}
      [:figcaption "Tシャツに刷ったところ（位置と大きさの確認）"]
      [:div {:id "pv-mockup" :class "sh-canvas sh-canvas-tall" :role "img"
             :aria-label "Tシャツに刷った着用イメージ"}]
      [:p {:class "sh-note" :id "mockup-note"}]]
     [:figure {:class "sh-fig"}
      [:figcaption "刷り上がりの予想（版だけ）"]
      [:div {:id "pv-print" :class "sh-canvas" :role "img"
             :aria-label "刷り上がりの予想"}]]
     [:figure {:class "sh-fig"}
      [:figcaption "白版と図案の重なり（choke の確認）"]
      [:div {:id "pv-underbase" :class "sh-canvas" :role "img"
             :aria-label "白版と図案の重なり"}]]]
    [:p {:class "dads-form-control-label__support-text" :id "status"}
     "図案を読み込んでいます…"])))

(defn- plates []
  (dds/card
   (dds/stack
    (dds/heading 2 "版" {:size "24"})
    [:p {:class "sh-note"}
     "刷る順に並んでいます。版下は" [:strong "白地に黒"]
     "（スクリーンの感光乳剤は光の有無しか見ないため、インクの色を付けても意味がありません）。"]
    [:div {:id "palette" :class "sh-palette"}]
    [:div {:id "plate-list" :class "sh-plates"}]
    ;; ボタンの hook は `:attrs` の `data-act` で取る（ブラウザ側 page.cljs が
    ;; `[data-act='…']` で拾う）。DADS button は `:attrs` を passthrough する。
    (dds/row
     (dds/button "版下 SVG" {:type :solid-fill :attrs {:data-act "save-films"}})
     (dds/button "AI（PDF ベース）" {:type :outline :attrs {:data-act "save-ai"}})
     (dds/button "PSD（1版1レイヤー）" {:type :outline :attrs {:data-act "save-psd"}})
     (dds/button "着用イメージ SVG" {:type :text :attrs {:data-act "save-mockup"}})))))

(defn- findings []
  (dds/card
   (dds/stack
    (dds/heading 2 "刷る前に見るところ" {:size "24"})
    [:div {:id "finding-list"}])))

;; ---------------------------------------------------------------- 説明

(defn- limits []
  (dds/stack
   (dds/notification-banner
    {:type :warning :heading "文字はアウトライン化してから入稿してください"}
    [:p [:code "<text>"] " はフォントのアウトライン化が要るため、この道具は版に載せません"
     "（黙って落とさず所見として報告します）。" [:code "<image>"] "（ラスタ）・"
     [:code "<use>"] "・グラデーション/パターン塗り・" [:code "fill=\"none\""]
     " の線だけの図形も同様です。"])
   (dds/notification-banner
    {:type :info-1 :heading "choke は「白フチが出ない」ための保険です"}
    [:p "白版が図案とぴったり同じだと、刷り位置が 0.1mm ずれた瞬間に白が図案の外へ"
     "はみ出して縁が白く光ります。逆に詰めすぎると下地が効かず、生地の色が透けます。"
     "上の「白版と図案の重なり」で、白が" [:strong "図案の輪郭より内側にぎりぎり収まっている"]
     "状態を目で確かめてください。"])
   (dds/notification-banner
    {:type :info-1 :heading "多角形のブール演算はしていません"}
    [:p "白抜きは SVG の " [:code "<mask>"] " で表し、ラスタライザに解かせています。"
     "choke は頂点法線オフセット（miter 継ぎ）で、Straight Skeleton ではありません。"
     "縮み代が図案の特徴幅の半分を超えると輪郭は破綻するので、その場合は"
     [:strong "「白版から消える」と報告して止めます。"]])))

(defn- how []
  (dds/card
   [:p "版の組み立ては純 " [:code ".cljc"] " の "
    [:a {:class "dads-link" :href "https://github.com/cloud-itonami/shirohan"}
     "cloud-itonami/shirohan"]
    "。同じ図案と同じ設定からは必ず同じ版が出る純関数なので、版下を差分でレビューでき、"
    "承認した版と刷った版が同じであることをハッシュで示せます。"]
   [:p {:class "sh-note"}
    "ブラウザの中でも" [:strong "同じ .cljc をそのまま"] "実行しています"
    "（scittle）—— 組版を JavaScript に書き直すと実装が2つになり、画面と版下が"
    "食い違うためです。ファイルはどこにも送信されません。"]
   [:p {:class "sh-note"}
    "受発注・校正・刷りの工程管理は "
    [:a {:class "dads-link" :href "/marketplace/"} "業種別の実装"]
    " の ISIC 1313（繊維の仕上げ ＝ printing on textiles and clothing）が持ちます。"
    "このページはその前段の" [:strong "製版"] "だけを担当します。"]))

;; ---------------------------------------------------------------- CSS

(def app-css
  "アプリ固有の CSS。**DADS token だけを参照し raw hex は書かない。**
  色は全部ドメインの値（ボディ色・インク色）なので DOM 側（JS が生成する
  style 属性）に出る —— CSS には 1 つも書かない。"
  (str/join
   "\n"
   [;; DADS の input / select は既定幅を持つ。1列フォームでは幅がばらついて
    ;; 読みづらいので、この画面では列幅いっぱいに揃える。
    ".sh-form .dads-input-text,.sh-form .dads-input-text__input,"
    ".sh-form .dads-select,.sh-form .dads-select__control,"
    ".sh-form .dads-select__select{inline-size:100%}"
    ".sh-file,.sh-source{inline-size:100%;font:inherit;"
    "border:1px solid var(--color-neutral-solid-gray-300);border-radius:8px;"
    "background:var(--color-neutral-white);padding:.5rem}"
    ".sh-source{font-family:ui-monospace,monospace;resize:vertical}"
    ".sh-previews{display:grid;gap:1rem;grid-template-columns:repeat(auto-fit,minmax(16rem,1fr))}"
    ".sh-fig{margin:0}"
    ".sh-fig figcaption{color:var(--color-neutral-solid-gray-600);margin-block-end:.375rem}"
    ".sh-canvas{display:grid;place-items:center;min-block-size:14rem;"
    "background:var(--color-neutral-solid-gray-50);"
    "border:1px solid var(--color-neutral-solid-gray-200);"
    "border-radius:12px;padding:1rem;overflow:auto}"
    ".sh-canvas svg{inline-size:auto;block-size:auto;max-inline-size:100%}"
    ".sh-plates{display:grid;gap:1rem;grid-template-columns:repeat(auto-fit,minmax(11rem,1fr))}"
    ".sh-plate{border:1px solid var(--color-neutral-solid-gray-200);border-radius:12px;"
    "padding:.75rem;background:var(--color-neutral-white)}"
    ".sh-plate svg{inline-size:100%;block-size:auto;display:block}"
    ".sh-plate-name{margin:.5rem 0 0;font-weight:700}"
    ".sh-swatch{display:inline-block;inline-size:.75rem;block-size:.75rem;"
    "border-radius:2px;border:1px solid var(--color-neutral-solid-gray-400);"
    "margin-inline-end:.375rem;vertical-align:-1px}"
    ".sh-palette{display:flex;flex-wrap:wrap;gap:.5rem;align-items:center}"
    ".sh-chip{display:inline-flex;align-items:center;gap:.375rem;"
    "border:1px solid var(--color-neutral-solid-gray-300);border-radius:999px;"
    "padding:.125rem .625rem;font-variant-numeric:tabular-nums}"
    ".sh-fig-wide{grid-column:1 / -1}"
    ".sh-canvas-tall{min-block-size:24rem}"
    ".sh-note{color:var(--color-neutral-solid-gray-600);margin:.25rem 0 0}"
    ".sh-ok{color:var(--color-neutral-solid-gray-600)}"]))

;; ---------------------------------------------------------------- 文書

(defn view []
  (dds/container
   [:div {:class "dds-ext-hero"}
    (dds/heading 1 "白版をつくる" {:size "45"})
    [:p {:class "dds-ext-lead"}
     "Tシャツなどに刷るベクタの図案から、白インクの下地（白版）・白抜き・"
     "スポットカラーの版を組み、見当合わせマーク付きの版下として書き出します。"]]

   (dds/section {:title "図案と条件"}
                (dds/grid {:min "24rem"} (controls) (previews)))

   (dds/section {:title "できあがった版"} (plates))

   (dds/section {:title "所見"} (findings))

   (dds/section {:title "この道具が「しないこと」"} (limits))

   (dds/section {:title "しくみ"} (how))))

(defn document
  "完全な HTML 文書。

  `dds-css` は vendor 済みの `resources/jp_go_dds/dds.css` の**中身**。
  `jp-go-dds.page` は I/O を持たない純関数を保つため、ファイルを読むのは
  呼び出し側（`scripts/generate-shirohan-site.cljs`）の仕事。

  ## script は body の末尾に置く

  `<head>` に置くと `page.cljs` が body の生成前に走り、`getElementById` が
  すべて nil を返して**何も起きないまま静かに終わる**（inkan で実測済み）。"
  [dds-css]
  (dds-page/->page
   {:title "白版をつくる — shirohan"
    :description "Tシャツ等に刷るベクタ図案から、白インク下地（白版）・白抜き・スポットカラー版を組み、見当合わせ付きの版下 SVG として書き出します。ファイルは送信されません。"
    :lang "ja"
    :css dds-css
    :app-css app-css}
   (view)
   ;; 失敗したときに黙って空白にしない。
   [:script (str "window.addEventListener('error',function(e){"
                 "var s=document.getElementById('status');"
                 "if(s){s.textContent='読み込みに失敗しました: '+(e.message||e);}});")]
   ;; `crossorigin` が無いと、CDN スクリプト内で起きた例外は `window.onerror` に
   ;; "Script error." としか渡らず、原因が読めない。
   [:script {:src "https://cdn.jsdelivr.net/npm/scittle@0.6.22/dist/scittle.js"
             :crossorigin "anonymous"}]
   [:script {:type "application/x-scittle" :src "./shirohan/geom.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/path.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/artwork.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/plate.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/svg.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/raster.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/mockup.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/pdf.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/psd.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/advice.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/core.cljs"}]
   [:script {:type "application/x-scittle" :src "./page.cljs"}]))
