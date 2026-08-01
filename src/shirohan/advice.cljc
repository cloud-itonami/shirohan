(ns shirohan.advice
  "murakumo（vLLM）に**判断だけ**を出し、返ってきたものを clamp して spec にする。

  ## 何を LLM に任せ、何を任せないか

  | | 誰がやるか |
  |---|---|
  | 量子化・輪郭追跡・choke・版分解・面積 | **決定論**（`shirohan.raster` / `plate`） |
  | 版を何色にするか | LLM が提案 → clamp |
  | どの色を白抜きにするか | LLM が提案 → **パレットに実在する色でなければ棄却** |
  | ボディ色・刷り位置・choke | LLM が提案 → clamp |

  幾何を LLM に触らせないのは、承認した版と刷った版が同じであることを示せなく
  なるから。逆に「この絵は 3 色で足りるか」「白は生地を見せる穴か、白インクか」は
  絵を見ないと決まらない判断で、そこが LLM の持ち場。

  ## モデル名を焼かない

  fleet の SSoT は murakumo KV の alias entry **`murakumo-main`**（ADR-2607173100）。
  具体的な model id（`qwen3.6-35b-a3b` 等）をここに書かない —— 書いた瞬間、
  モデルを差し替えるたびにこの repo を触ることになる。`model` には常に
  `\"murakumo-main\"` を送り、解決は worker 側の KV に任せる。

  ## この ns は HTTP を持たない

  リクエストの**形**を作り、レスポンスを**検証して spec にする**だけの純関数。
  実際に投げるのは host（ブラウザなら `page.cljs` → cloud-itonami の edge
  function）。純関数なので、同じ助言からは必ず同じ spec が出る。"
  (:require [clojure.string :as str]))

(def model-alias
  "fleet main の alias。**具体的な model id を書かない**（ADR-2607173100）。"
  "murakumo-main")

(def ^:private schema-note
  (str "次の JSON だけを返してください。説明文・コードフェンスは付けないでください。\n"
       "{\"colors\": <2-6 の整数>,\n"
       " \"knockout_fill\": <パレットにある色の16進、または null>,\n"
       " \"garment_color\": <#rrggbb>,\n"
       " \"choke_mm\": <0 から 0.5 の数>,\n"
       " \"placement\": <\"chest-center\" | \"chest-high\" | \"chest-left\">,\n"
       " \"rationale\": <日本語 1-2 文>}"))

(defn request
  "murakumo の `/v1/messages` に投げる本体を作る。

  `palette` は `shirohan.raster/trace` が返した `#rrggbb` の列、`image-data-url`
  は縮小済みのサムネイル（省略可。渡すと絵そのものを見て判断できる）。"
  ([palette] (request palette {}))
  ([palette {:keys [image-data-url note max-tokens] :or {max-tokens 512}}]
   (let [text (str "あなたはTシャツのスクリーン印刷の製版担当です。"
                   "これから渡す図案について、版の作り方を決めてください。\n\n"
                   "図案から抽出された色: " (str/join ", " palette) "\n"
                   (when note (str "依頼者のメモ: " note "\n"))
                   "\n判断してほしいこと:\n"
                   "- 版の数（色数）。少ないほど安く刷れるので、意味が失われない範囲で減らす\n"
                   "- 白が「生地を見せる穴（白抜き）」か「白インク」か\n"
                   "- 図案が映えるボディ色\n"
                   "- choke（白版の縮み代）。細い図案ほど小さく\n"
                   "- 刷り位置\n\n"
                   schema-note)]
     {:model model-alias
      :max_tokens max-tokens
      :messages [{:role "user"
                  :content (if image-data-url
                             [{:type "text" :text text}
                              {:type "image_url" :image_url {:url image-data-url}}]
                             text)}]})))

;; ---------------------------------------------------------------- 受け取り

(defn- clamp [x lo hi] (cond (nil? x) nil (< x lo) lo (> x hi) hi :else x))

(defn- hex? [s] (boolean (and (string? s) (re-matches #"#[0-9a-fA-F]{6}" s))))

(defn extract-json
  "モデルの出力から JSON 本体を取り出す。コードフェンスや前置きが付いてくることが
  あるので、**最初の `{` から最後の `}` まで**を切る（`json/parse` は host 側）。"
  [s]
  (let [t (str s)
        a (str/index-of t "{")
        b (str/last-index-of t "}")]
    (when (and a b (< a b)) (subs t a (inc b)))))

(def placements #{"chest-center" "chest-high" "chest-left"})

(defn apply-advice
  "助言（decode 済みの map、キーは文字列）を spec に変換する。**gate はここ。**

  返り値は `{:spec {…} :accepted {…} :rejected [{:field :reason}]}`。
  棄却したものは黙って捨てず必ず返す —— 「AI が決めた」と言われたものが実は
  無視されていた、という状態を作らない。"
  [advice palette]
  (let [pal (set (map str/lower-case (or palette [])))
        rej (atom [])
        rej! (fn [f r] (swap! rej conj {:field f :reason r}) nil)
        colors (let [v (get advice "colors")]
                 (cond (not (number? v)) (rej! :colors "数値でない")
                       (not= v (clamp v 2 6)) (do (rej! :colors "2〜6 の外だったので丸めた")
                                                  (clamp v 2 6))
                       :else (int v)))
        ko (let [v (get advice "knockout_fill")]
             (cond (nil? v) nil
                   (not (hex? v)) (rej! :knockout-fill "#rrggbb でない")
                   (not (contains? pal (str/lower-case v)))
                   (rej! :knockout-fill "パレットに無い色は白抜きにできない")
                   :else (str/lower-case v)))
        garment (let [v (get advice "garment_color")]
                  (if (hex? v) (str/lower-case v) (rej! :garment-color "#rrggbb でない")))
        choke (let [v (get advice "choke_mm")]
                (cond (not (number? v)) (rej! :choke-mm "数値でない")
                      (not= (double v) (double (clamp v 0.0 0.5)))
                      (do (rej! :choke-mm "0〜0.5mm の外だったので丸めた")
                          (clamp (double v) 0.0 0.5))
                      :else (double v)))
        placement (let [v (get advice "placement")]
                    (if (contains? placements v)
                      (keyword v)
                      (rej! :placement "既知の刷り位置でない")))
        spec (cond-> {}
               colors (assoc :colors colors)
               garment (assoc :garment-color garment)
               choke (assoc :choke-mm choke)
               placement (assoc :placement-id placement)
               ;; knockout は「nil を明示的に選ぶ」ことがある（白もインクとして刷る）
               (contains? advice "knockout_fill") (assoc :knockout-fill ko))]
    {:spec spec
     :accepted spec
     :rationale (let [r (get advice "rationale")] (when (string? r) r))
     :rejected @rej}))
