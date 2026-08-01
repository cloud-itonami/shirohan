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
   ["0.1" "0.1mm（既定・実務値）"]
   ["0.15" "0.15mm"]
   ["0.2" "0.2mm"]
   ["0.3" "0.3mm（手刷り・見当が甘い環境）"]
   ["0.5" "0.5mm（厚地・大判）"]])

(def ^:private widths
  [["120" "120mm（ワンポイント）"]
   ["180" "180mm（胸中央）"]
   ["220" "220mm"]
   ["260" "260mm（前身頃・標準）"]
   ["280" "280mm（前身頃・上限）"]])

;; ---------------------------------------------------------------- 入力

(defn- stage []
  "画面の上に**固定**される結果表示。条件をいじる間も視界から消えない。"
  [:div {:class "sh-stage"}
   [:div {:id "stage" :class "sh-canvas" :role "img"
          :aria-label "仕上がりのプレビュー"}]
   [:p {:class "sh-stage-note" :id "status"} "図案を読み込んでいます…"]
   [:div {:class "sh-seg" :role "tablist" :aria-label "表示の切り替え"}
    ;; **白版が既定。** この道具の成果物はこれで、他は確認用。
    [:button {:type "button" :role "tab" :data-view "white" :aria-selected "true"}
     "白版"]
    [:button {:type "button" :role "tab" :data-view "cut" :aria-selected "false"}
     "カットライン"]
    [:button {:type "button" :role "tab" :data-view "underbase" :aria-selected "false"}
     "重なり確認"]
    [:button {:type "button" :role "tab" :data-view "print" :aria-selected "false"}
     "刷り上がり"]
    [:button {:type "button" :role "tab" :data-view "mockup" :aria-selected "false"}
     "Tシャツ"]]])

(defn- artwork-controls []
  (dds/stack
   (dds/form-field
    {:label "図案（SVG / PNG / JPEG）" :for "f-file" :support-id "f-file-support"
     :support "画像を選ぶだけで、色を拾って輪郭を起こし、白版まで作ります。ファイルは送信されません。"}
    [:input {:type "file" :id "f-file" :name "file"
             :accept ".svg,image/svg+xml,image/png,image/jpeg,image/webp"
             :class "sh-file" :aria-describedby "f-file-support"}])

   (dds/form-field
    {:label "刷り幅" :for "f-width"}
    (dds/select {:id "f-width" :name "width" :value "260"} widths))

   (dds/form-field
    {:label "ボディ色" :for "f-garment"}
    (dds/select {:id "f-garment" :name "garment" :value "#1f1f1f"} garment-colors))

   (dds/form-field
    {:label "刷り位置" :for "f-placement"}
    (dds/select {:id "f-placement" :name "placement" :value "chest-center"}
                (mapv (fn [{:keys [id label]}] [(name id) label]) mockup/placements)))

   (dds/form-field
    {:label "版の数（画像から起こすとき）" :for "f-colors"}
    (dds/select {:id "f-colors" :name "colors" :value "4"}
                [["2" "2色"] ["3" "3色"] ["4" "4色"] ["5" "5色"] ["6" "6色"]]))

   (dds/row
    (dds/button "おまかせで決める" {:type :outline :attrs {:data-act "advise"}})
    (dds/button "組み直す" {:type :text :attrs {:data-act "rerender"}}))))

(defn- advanced-controls []
  "**畳んでおく。** 既定のまま使えることが多いので、最初から全部見せない。"
  [:details {:class "sh-adv"}
   [:summary "細かい設定"]
   (dds/stack
    ;; DADS の `select` は **`[opts options]`** の順（逆に渡すと opts が options として
    ;; map され、選択肢が黙って壊れる）。`:value` を必ず渡す —— 渡さないと HTML 上は
    ;; 先頭の option が選ばれ、見た目は正常なまま既定値がずれる。
    (dds/form-field
     {:label "choke（外側を削る量）" :for "f-choke" :support-id "f-choke-support"
      :support "白インクと色インクの刷りは若干ずれるので、図案の外側をこの量だけ削ります（縮小ではありません）。現場の実務値は 0.1mm。"}
     (dds/select {:id "f-choke" :name "choke" :value "0.1"
                  :aria-describedby "f-choke-support"}
                 chokes))

    (dds/form-field
     {:label "カットライン（断裁線）" :for "f-cut" :support-id "f-cut-support"
      :support "アクリルスタンド・ステッカーの外形を切る線。インクが載る面から外側へ出します。"}
     (dds/select {:id "f-cut" :name "cut" :value "3"
                  :aria-describedby "f-cut-support"}
                 [["0" "作らない"] ["2" "2mm"] ["3" "3mm（既定）"]
                  ["4" "4mm"] ["5" "5mm"]]))

    (dds/form-field
     {:label "色版も作る" :for "f-sep" :support-id "f-sep-support"
      :support "成果物は白版なので、既定では作りません。色ごとの版に分けたいときだけ。図案の色を拾って分解するので、そのぶん時間がかかります。"}
     (dds/select {:id "f-sep" :name "sep" :value "0"
                  :aria-describedby "f-sep-support"}
                 [["0" "作らない（白版だけ・速い）"] ["1" "作る"]]))

    (dds/form-field
     {:label "白版の解像度" :for "f-res" :support-id "f-res-support"
      :support "白版の輪郭を追う細かさ。上げると細い髪の隙間まで拾えますが、そのぶん時間がかかります（ブラウザの中で処理するため）。"}
     (dds/select {:id "f-res" :name "res" :value "768"
                  :aria-describedby "f-res-support"}
                 [["512" "標準 512px（速い）"]
                  ["768" "高 768px（既定）"]
                  ["1024" "最高 1024px（数秒かかります）"]]))

    (dds/form-field
     {:label "地の抜き方" :for "f-seg" :support-id "f-seg-support"
      :support "「どこがインクの載る面か」の決め方。透明な PNG なら透明度が正解なので既定のままで十分です。白背景の JPEG や写真のように透明度が使えない絵のときだけ、AI に抜かせます。どれもブラウザの中で動くのでファイルは送信されません（AI は初回だけモデルの読み込みに数十MBかかります）。切り替えると同じ図案で抜き直すので、そのまま見比べられます。"}
     (dds/select {:id "f-seg" :name "seg" :value "alpha"
                  :aria-describedby "f-seg-support"}
                 [["alpha" "透明度で抜く（既定・一瞬・透明な PNG 向き）"]
                  ["Xenova/modnet" "MODNet で抜く（軽い・商用可）"]
                  ["briaai/RMBG-1.4" "RMBG-1.4 で抜く（高品質・非商用のみ）"]
                  ["onnx-community/BiRefNet_lite" "BiRefNet lite で抜く（最高品質・重い）"]]))

    (dds/form-field
     {:label "白版" :for "f-underbase"}
     (dds/select {:id "f-underbase" :name "underbase" :value "1"}
                 [["1" "白版を作る"] ["0" "作らない（淡色ボディ）"]]))

    (dds/form-field
     {:label "白抜き（生地を見せる穴）" :for "f-knockout" :support-id "f-ko-support"
      :support "既定は使いません。白版は「白インクを塗る部分の指示」なので、図案の白い部分も白インクで刷ります（穴になるのは透明な地だけ）。"}
     (dds/select {:id "f-knockout" :name "knockout" :value "none"
                  :aria-describedby "f-ko-support"}
                 [["none" "使わない（白も白インクで刷る）"]
                  ["#ffffff" "白（#ffffff）を生地見せの穴にする"]]))

    (dds/form-field
     {:label "おまかせに伝えること（任意）" :for "f-note" :support-id "f-note-support"
      :support "「落ち着いた色で」「予算優先で版を減らして」など。判断は推論モデルが行うため 40 秒〜2 分かかります。"}
     (dds/input-text {:id "f-note" :name "note" :aria-describedby "f-note-support"}))

    (dds/form-field
     {:label "SVG のソース" :for "f-svg" :support-id "f-svg-support"
      :support "直接貼っても構いません。文字はアウトライン化してから入稿してください。"}
     ;; `dds/textarea` は opts しか取らず初期内容を渡せない（HTML の textarea は
     ;; value 属性ではなく子要素で中身を持つ）。上流と同じ markup を手で組む。
     [:span {:class "dads-textarea"}
      [:textarea {:id "f-svg" :name "svg" :rows "5"
                  :class "dads-textarea__textarea sh-source"
                  :spellcheck "false" :aria-describedby "f-svg-support"}
       sample-svg]]))])

(defn- plates-panel []
  (dds/stack
   (dds/heading 2 "版" {:size "24"})
   [:p {:class "sh-note"}
    "この道具の成果物は" [:strong "白版"] "（0 番）です。以降の色版は、"
    "図案から拾った色を参考までに分解したもの。版下は" [:strong "白地に黒"]
    "（感光乳剤は光の有無しか見ないため、インクの色を付けても意味がありません）。"
    "CMYK は入稿用の初期値で、" [:strong "ICC プロファイルを通していません"] "。"]
   [:div {:id "palette" :class "sh-palette"}]
   [:div {:id "plate-list" :class "sh-plates"}]))

(defn- findings-panel []
  (dds/stack
   (dds/heading 2 "刷る前に見るところ" {:size "24"})
   [:div {:id "finding-list"}]))

(defn- actions []
  [:div {:class "sh-actions"}
   ;; **白版が主。** 他は必要な人だけが押す。
   (dds/button "白版 SVG" {:type :solid-fill :attrs {:data-act "save-white"}})
   (dds/button "カットライン" {:type :outline :attrs {:data-act "save-cut"}})
   (dds/button "AI（PDF）" {:type :outline :attrs {:data-act "save-ai"}})
   (dds/button "PSD" {:type :outline :attrs {:data-act "save-psd"}})
   (dds/button "版下一式" {:type :text :attrs {:data-act "save-films"}})
   (dds/button "着用イメージ" {:type :text :attrs {:data-act "save-mockup"}})])

(def app-css
  "アプリ固有の CSS。**DADS token だけを参照し raw hex は書かない。**
  色は全部ドメインの値（ボディ色・インク色）なので DOM 側（JS が生成する
  style 属性）に出る —— CSS には 1 つも書かない。

  ## mobile first / single screen

  この道具は**片手で完結する**のが正しい形。図案を選ぶ → 出来上がりを見る →
  書き出す、の 3 手しかないので、ページを縦に長く積むと本質が見えなくなる。

  - **ステージ（仕上がり）を上に固定**（`position: sticky`）。条件をいじる間も
    結果が視界から消えない —— スクロールして戻る操作が要らない
  - 表示は**セグメント切替**（仕上がり / 版 / 白版の確認）。3 つ同時に並べない
  - 条件は下でスクロール、**書き出しは最下部に固定**
  - 広い画面では 2 段組みに開く（mobile first の素直な拡張。desktop 用に別の
    レイアウトを書かない）

  タップ標的は 44px 以上、`env(safe-area-inset-*)` を尊重する。"
  (str/join
   "\n"
   [;; --- app shell ---
    ".sh-app{display:grid;gap:0;grid-template-columns:1fr}"
    ".sh-stage{position:sticky;top:0;z-index:2;"
    "background:var(--color-neutral-white);"
    "border-block-end:1px solid var(--color-neutral-solid-gray-200);"
    "padding:.5rem .25rem;margin-inline:-.5rem}"
    ".sh-canvas{display:grid;place-items:center;block-size:38dvh;min-block-size:13rem;"
    "background:var(--color-neutral-solid-gray-50);"
    "border-radius:12px;padding:.5rem;overflow:hidden}"
    ".sh-canvas svg{inline-size:auto;block-size:100%;max-inline-size:100%}"
    ".sh-stage-note{margin:.375rem .25rem 0;color:var(--color-neutral-solid-gray-600)}"

    ;; --- セグメント切替（表示の選択）---
    ".sh-seg{display:flex;gap:.25rem;margin:.5rem .25rem 0;"
    "background:var(--color-neutral-solid-gray-100);border-radius:999px;padding:.25rem}"
    ".sh-seg button{flex:1;min-block-size:2.75rem;border:0;border-radius:999px;"
    "background:transparent;font:inherit;color:var(--color-neutral-solid-gray-700);"
    "cursor:pointer}"
    ".sh-seg button[aria-selected='true']{background:var(--color-neutral-white);"
    "color:var(--color-neutral-solid-gray-900);font-weight:700;"
    "box-shadow:0 1px 3px rgb(0 0 0 / .12)}"
    ".sh-seg button:focus-visible{outline:3px solid var(--color-primitive-blue-900);"
    "outline-offset:2px}"

    ;; --- 条件パネル ---
    ".sh-panel{padding-block:1rem}"
    ".sh-form .dads-input-text,.sh-form .dads-input-text__input,"
    ".sh-form .dads-select,.sh-form .dads-select__control,"
    ".sh-form .dads-select__select{inline-size:100%}"
    ".sh-form .dads-select__select{min-block-size:2.75rem}"
    ".sh-file{inline-size:100%;font:inherit;min-block-size:2.75rem;"
    "border:2px dashed var(--color-neutral-solid-gray-400);border-radius:12px;"
    "background:var(--color-neutral-white);padding:.75rem}"
    ".sh-source{inline-size:100%;font-family:ui-monospace,monospace;resize:vertical}"
    ".sh-adv{margin-block-start:.5rem}"
    ".sh-adv>summary{min-block-size:2.75rem;display:flex;align-items:center;"
    "cursor:pointer;font-weight:700}"

    ;; --- 版の一覧 ---
    ".sh-plates{display:grid;gap:.75rem;grid-template-columns:repeat(auto-fill,minmax(9rem,1fr))}"
    ".sh-plate{border:1px solid var(--color-neutral-solid-gray-200);border-radius:12px;"
    "padding:.625rem;background:var(--color-neutral-white)}"
    ".sh-plate svg{inline-size:100%;block-size:auto;display:block}"
    ".sh-plate-name{margin:.5rem 0 0;font-weight:700}"
    ".sh-cmyk{font-variant-numeric:tabular-nums;color:var(--color-neutral-solid-gray-600);"
    "margin:.125rem 0 0}"
    ".sh-swatch{display:inline-block;inline-size:.75rem;block-size:.75rem;"
    "border-radius:2px;border:1px solid var(--color-neutral-solid-gray-400);"
    "margin-inline-end:.375rem;vertical-align:-1px}"
    ".sh-palette{display:flex;flex-wrap:wrap;gap:.5rem;align-items:center}"
    ".sh-chip{display:inline-flex;align-items:center;gap:.375rem;"
    "border:1px solid var(--color-neutral-solid-gray-300);border-radius:999px;"
    "padding:.125rem .625rem;font-variant-numeric:tabular-nums}"
    ".sh-note{color:var(--color-neutral-solid-gray-600);margin:.25rem 0 0}"
    ".sh-ok{color:var(--color-neutral-solid-gray-600)}"

    ;; --- 書き出し（最下部に固定）---
    ".sh-actions{position:sticky;bottom:0;z-index:2;"
    "background:var(--color-neutral-white);"
    "border-block-start:1px solid var(--color-neutral-solid-gray-200);"
    "padding:.625rem .25rem calc(.625rem + env(safe-area-inset-bottom));"
    "margin-inline:-.5rem;display:flex;gap:.5rem;overflow-x:auto}"
    ".sh-actions .dads-button{flex:0 0 auto;min-block-size:2.75rem}"

    ;; --- 広い画面では 2 段組みに開く ---
    "@media (min-width:60rem){"
    ".sh-app{grid-template-columns:minmax(0,1fr) minmax(0,22rem);gap:1.5rem;"
    "align-items:start}"
    ".sh-stage{margin-inline:0;border:0;padding-inline:0}"
    ".sh-canvas{block-size:52dvh}"
    ".sh-side{position:sticky;top:0;max-block-size:100dvh;overflow-y:auto}"
    ".sh-actions{margin-inline:0}"
    "}"]))

(defn- what-is-shirohan []
  (dds/card
   [:p "白インクの上に色インクを刷らないと色が出ないので、"
    [:strong "①白版（1色ベタ塗り）を刷る → ②その上に色インクを刷る"] " という順になります。"]
   [:p "つまり白版は図案の色分けではなく、"
    [:strong "図案が乗る面のシルエットを1色でベタに塗ったもの"] "です。"
    "図案の中の白い部分も「白インクで刷る部分」なので白版に含まれます —— "
    "穴になるのは、インクを一切載せない透明な地だけです。"]
   [:p "白インクと色インクの刷りは若干ずれるため、白版は図案の "
    [:strong "外側を 0.1mm 削り"] "ます（choke）。"]
   [:p {:class "sh-note"}
    [:strong "「0.1mm 縮小」ではありません。"]
    "ドーナツ型で単純に縮小すると穴も一緒に小さくなり、"
    [:strong "穴の部分に白版がはみ出します"] "。"
    "正しくはインクが載る面を全周から削るので、外周は内側へ、"
    [:strong "穴は外側へ"] "動きます。"]))

(defn- limits []
  (dds/stack
   (dds/notification-banner
    {:type :warning :heading "文字はアウトライン化してから入稿してください"}
    [:p [:code "<text>"] " はフォントのアウトライン化が要るため版に載せません"
     "（黙って落とさず所見として報告します）。" [:code "<image>"] "（ラスタ）・"
     [:code "<use>"] "・グラデーション/パターン塗り・" [:code "fill=\"none\""]
     " の線だけの図形も同様です。"])
   (dds/notification-banner
    {:type :info-1 :heading "入稿は透明 PNG か SVG が確実です"}
    [:p "白背景の画像でも、画像の縁から繋がった地を自動で判定します。ただし"
     [:strong "「白い地」と「白い図柄」は色だけでは区別できません"]
     "——ドーナツの穴のように" [:strong "抜きたい部分"] "がある場合、"
     "その部分を透明にした PNG（または SVG）で入稿してください。"
     "判定した内容は所見に出します。"])
   (dds/notification-banner
    {:type :info-1 :heading "CMYK は ICC プロファイルを通していません"}
    [:p "素朴な式による変換なので、入稿データに置く"[:strong "初期値"]
     "であって刷り色の保証ではありません。色校正の代わりにはなりません。"
     "そもそもガーメントのスクリーン印刷は" [:strong "スポットカラー"]
     "（調合インキ）で刷るのが普通で、版の分解自体はこの道具のスポットカラー分解が正本です。"])
   (dds/notification-banner
    {:type :info-1 :heading "多角形のブール演算はしていません"}
    [:p "白抜きは SVG の " [:code "<mask>"] " で表し、ラスタライザに解かせています。"
     "choke は頂点法線オフセットで、Straight Skeleton ではありません。"
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
    "ブラウザの中でも" [:strong "同じ .cljc をそのまま"] "実行しています（scittle）。"
    "ファイルはどこにも送信されません。"]
   [:p {:class "sh-note"}
    "受発注・校正・刷りの工程管理は "
    [:a {:class "dads-link" :href "/marketplace/"} "業種別の実装"]
    " の ISIC 1313（繊維の仕上げ ＝ printing on textiles and clothing）が持ちます。"]))

(defn view []
  (dds/container
   [:div {:class "dds-ext-hero"}
    (dds/heading 1 "白版をつくる" {:size "32"})
    [:p {:class "dds-ext-lead"}
     "画像を選ぶと、"[:strong "図案があった場所を黒くベタ塗りした白版"]
     "が出ます。白インクを塗る部分の指示なので、イラストそのものは要りません。"]]

   [:div {:class "sh-app"}
    [:div
     (stage)
     (actions)]
    [:div {:class "sh-side"}
     [:div {:class "sh-panel sh-form"}
      (artwork-controls)
      (advanced-controls)]
     (dds/section {:title "版"} (plates-panel))
     (dds/section {:title "所見"} (findings-panel))]]

   (dds/section {:title "白版とは"} (what-is-shirohan))
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
   ;; 切り抜きモデルは**必要になってから**読む（初回数十MB）。既定の経路は
   ;; これに一切触らないので、今までどおり即座に版が出る。
   [:script {:type "module"}
    (str "window.shirohanSeg=async function(url,model,side,note){"
         "const T=await import('https://cdn.jsdelivr.net/npm/@huggingface/transformers@3.7.6');"
         "window.__shSeg=window.__shSeg||{};"
         "const t0=performance.now();"
         "if(!window.__shSeg[model]){note('モデルを読み込んでいます…（初回だけ数十MB）');"
         " try{window.__shSeg[model]=await T.pipeline('background-removal',model,{device:'webgpu'});}"
         " catch(e){window.__shSeg[model]=await T.pipeline('background-removal',model);}}"
         "note('切り抜いています…');"
         "const out=await window.__shSeg[model](url);"
         "const src=out[0].toCanvas();"
         "const k=Math.min(1,side/Math.max(src.width,src.height));"
         "const w=Math.max(1,Math.round(src.width*k)),h=Math.max(1,Math.round(src.height*k));"
         "const c=document.createElement('canvas');c.width=w;c.height=h;"
         "const g=c.getContext('2d');g.imageSmoothingEnabled=true;g.drawImage(src,0,0,w,h);"
         "return {w:w,h:h,data:g.getImageData(0,0,w,h).data,ms:Math.round(performance.now()-t0)};};")]
   [:script {:src "https://cdn.jsdelivr.net/npm/scittle@0.6.22/dist/scittle.js"
             :crossorigin "anonymous"}]
   [:script {:type "application/x-scittle" :src "./shirohan/geom.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/curve.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/path.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/artwork.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/color.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/raster.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/cut.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/plate.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/svg.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/mockup.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/pdf.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/psd.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/advice.cljs"}]
   [:script {:type "application/x-scittle" :src "./shirohan/core.cljs"}]
   [:script {:type "application/x-scittle" :src "./page.cljs"}]))
