(ns shirohan.mockup
  "Tシャツのボディに版を載せた**着用イメージ**を SVG で組む。

  ## なぜ写真の下地を使わないのか

  ボディは**ベクタで描く**（写真を持たない）。理由は 3 つ:

  - 写真を置くと、この repo が誰かの著作物と撮影条件に縛られる。ボディ色を変える
    たびに撮り直すか色被せの誤魔化しが要る。
  - 公開サイトは外部リクエストゼロで動く設計（`site/shirohan/page.cljc`）。
    数百 KB の写真を base64 で抱えるのは、その設計と釣り合わない。
  - **寸法の嘘をつかない。** ベクタなら「身幅 52cm のボディに 26cm の図案」を
    実寸比のまま描ける。写真に合成すると、遠近と撮影角度のぶん必ずずれる。

  代わりにボディはシルエット + 縫い目 + 襟リブだけの素直な図で、色は塗りで変える。
  **これは仕上がりの色校正には使えない**（生地の質感・インクの沈み・発色は再現
  しない）。位置と大きさの確認のための図であることは UI 側にも書いてある。

  ## 座標

  viewBox は 400×480 の抽象単位。ボディ身幅（脇から脇）が 240 単位で、これが
  `:body-width-mm`（既定 520mm ＝ メンズ L の平置き身幅あたり）に対応する。
  版はこの比率で縮めて胸位置に置くので、**画面上の大小がそのまま実寸の大小**。"
  (:require [clojure.string :as str]
            [shirohan.geom :as geom]
            [shirohan.svg :as ssvg]))

(def ^:private fmt geom/fmt)

;; ---------------------------------------------------------------- ボディ

(def ^:private body-d
  "前身頃のシルエット。左肩→左袖→脇→裾→右脇→右袖→右肩→襟、で一周する。"
  (str "M160 44 "
       "C152 46 146 50 140 54 "        ; 左肩
       "L48 88 "                       ; 袖山へ
       "C38 92 32 102 34 112 "
       "L52 196 "                      ; 袖口
       "C54 206 64 212 74 210 "
       "L88 206 "                      ; 脇の付け根
       "L86 452 "                      ; 脇線→裾
       "C86 458 90 462 96 462 "
       "L304 462 "
       "C310 462 314 458 314 452 "
       "L312 206 "
       "L326 210 "
       "C336 212 346 206 348 196 "
       "L366 112 "
       "C368 102 362 92 352 88 "
       "L260 54 "
       "C254 50 248 46 240 44 "        ; 右肩
       "C236 72 216 84 200 84 "        ; 襟ぐり（右→左）
       "C184 84 164 72 160 44 Z"))

(def ^:private neck-d
  "襟リブ。ボディより一段濃い色で入れると首まわりが立つ。"
  (str "M160 44 C164 72 184 84 200 84 C216 84 236 72 240 44 "
       "C236 38 216 30 200 30 C184 30 164 38 160 44 Z"))

(def ^:private seams
  "袖の付け根と裾の縫い目。無いとシルエットが紙のように見える。"
  [["M88 206 C110 224 140 232 200 232 C260 232 290 224 312 206" 0.35]
   ["M86 440 L314 440" 0.3]])

;; ---------------------------------------------------------------- 版の配置

(def ^:private body-width-units 228.0)   ; 脇(86)〜脇(314)
(def ^:private chest-center-x 200.0)

(def placements
  "刷り位置。`:top` は viewBox 上の版の上端。

  数値は「身幅 520mm のボディに対して、襟ぐり下 何 mm から刷るか」を単位換算した
  もの。前身頃中央は襟から 70〜80mm 下が定番で、ワンポイントはもっと上・小さい。"
  [{:id :chest-center :label "前身頃・中央" :top 120.0}
   {:id :chest-high :label "前身頃・高め" :top 100.0}
   {:id :chest-left :label "左胸ワンポイント" :top 118.0 :center-x 148.0}])

(defn placement [id]
  (or (first (filter #(= id (:id %)) placements)) (first placements)))

;; ---------------------------------------------------------------- 出力

(defn mockup-svg
  "着用イメージ。ボディ色の上に、刷る順で版を重ねた図を実寸比で置く。

  opts:
  - `:garment-color`  ボディ色（既定は job の spec のもの）
  - `:placement`      `:chest-center` / `:chest-high` / `:chest-left`
  - `:body-width-mm`  ボディの平置き身幅（既定 520.0）
  - `:px-per-unit`    `width`/`height` 属性の換算（既定 1.2）
  - `:show-seams?`    縫い目を描くか（既定 true）"
  ([job] (mockup-svg job {}))
  ([{:keys [size spec plates] :as job}
    {:keys [garment-color placement-id body-width-mm px-per-unit show-seams?]
     :or {placement-id :chest-center body-width-mm 520.0 px-per-unit 1.2
          show-seams? true}}]
   (let [color (or garment-color (:garment-color spec))
         p (placement placement-id)
         ;; 版の mm を viewBox 単位へ。身幅 body-width-mm が body-width-units。
         k (/ body-width-units body-width-mm)
         w (* (:width-mm size) k)
         h (* (:height-mm size) k)
         cx (or (:center-x p) chest-center-x)
         x (- cx (/ w 2.0))
         y (:top p)
         ordered (sort-by :order plates)]
     (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
          " width=\"" (fmt (* 400 px-per-unit)) "\""
          " height=\"" (fmt (* 480 px-per-unit)) "\""
          " viewBox=\"0 0 400 480\" role=\"img\""
          " aria-label=\"Tシャツに刷った着用イメージ\">"
          "<title>Tシャツに刷った着用イメージ（位置と大きさの確認用）</title>"
          ;; 生地の陰。ベクタなので色を変えても破綻しない。
          "<defs><linearGradient id=\"mk-shade\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"0\">"
          "<stop offset=\"0\" stop-color=\"#000000\" stop-opacity=\"0.18\"/>"
          "<stop offset=\"0.25\" stop-color=\"#000000\" stop-opacity=\"0\"/>"
          "<stop offset=\"0.75\" stop-color=\"#000000\" stop-opacity=\"0\"/>"
          "<stop offset=\"1\" stop-color=\"#000000\" stop-opacity=\"0.18\"/>"
          "</linearGradient>"
          "<clipPath id=\"mk-body\"><path d=\"" body-d "\"/></clipPath></defs>"

          "<path d=\"" body-d "\" fill=\"" color "\"/>"
          "<path d=\"" neck-d "\" fill=\"#000000\" fill-opacity=\"0.16\"/>"
          "<rect x=\"0\" y=\"0\" width=\"400\" height=\"480\""
          " fill=\"url(#mk-shade)\" clip-path=\"url(#mk-body)\"/>"
          (when show-seams?
            (str/join
             (map (fn [[d o]]
                    (str "<path d=\"" d "\" fill=\"none\" stroke=\"#000000\""
                         " stroke-opacity=\"" o "\" stroke-width=\"1.2\"/>"))
                  seams)))

          ;; --- 版を実寸比で胸に置く ---
          "<g transform=\"translate(" (fmt x) "," (fmt y) ") scale(" (fmt k) ")\">"
          (str/join
           (map-indexed
            (fn [i pl]
              (let [mid (str "mk-ko-" i)
                    d (geom/contours->d (:art pl))]
                (str (when (seq (:knockout pl))
                       (str "<defs><mask id=\"" mid "\" maskUnits=\"userSpaceOnUse\""
                            " x=\"0\" y=\"0\" width=\"" (fmt (:width-mm size))
                            "\" height=\"" (fmt (:height-mm size)) "\">"
                            "<rect x=\"0\" y=\"0\" width=\"" (fmt (:width-mm size))
                            "\" height=\"" (fmt (:height-mm size)) "\" fill=\"#ffffff\"/>"
                            "<path d=\"" (geom/contours->d (:knockout pl))
                            "\" fill=\"#000000\"/></mask></defs>"))
                     (when-not (str/blank? d)
                       (str "<path d=\"" d "\" fill=\"" (:color pl) "\""
                            (when (seq (:knockout pl))
                              (str " mask=\"url(#" mid ")\""))
                            "/>")))))
            ordered))
          "</g>"
          "</svg>"))))

(defn print-size-note
  "「この図案は実寸で何 cm か」を人が読める形で返す。**画面の見た目と実寸は
  別物**なので、数字を必ず添える。"
  [{:keys [size]} {:keys [body-width-mm] :or {body-width-mm 520.0}}]
  (str "図案 " (fmt (/ (:width-mm size) 10.0)) "×" (fmt (/ (:height-mm size) 10.0))
       "cm（見当マーク込み）／ボディ平置き身幅 " (fmt (/ body-width-mm 10.0)) "cm"))
