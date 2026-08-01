(ns shirohan.color
  "RGB → CMYK。**カラーマネジメントはしていない**ことを名前と doc で先に言う。

  ## 何をしているか

  教科書どおりの素朴な変換:

  ```
  K = 1 - max(R,G,B)
  C = (1-R-K)/(1-K)   M = (1-G-K)/(1-K)   Y = (1-B-K)/(1-K)
  ```

  ## 何をしていないか —— ここが重要

  これは **ICC プロファイルを通した色変換ではない。** 実際の印刷の CMYK は
  用紙・インキ・機械で発色が変わるので、Japan Color 2001 Coated のような
  出力プロファイルを通して初めて「刷ったらこの色になる」と言える。素朴な式は
  その情報を一切持たないので、**濃度は合っても色は合わない**（特に紺・深緑・
  鮮やかなオレンジで大きくずれる）。

  だからこの ns が返す値は「入稿データに載せる CMYK の**初期値**」であって、
  色校正の代わりにはならない。`shirohan.plate` はこれを所見
  `:cmyk-uncalibrated` として必ず添える —— **黙って「CMYK 対応済み」の顔を
  しない**のがこの ns の設計方針。

  ## そもそもシルクスクリーンでは CMYK を使わないことが多い

  ガーメントのスクリーン印刷は**スポットカラー**（DIC / PANTONE の調合インキ）
  で刷るのが普通で、CMYK 4 色分解はプロセス印刷（オフセット・インクジェット）の
  やり方。ここで CMYK を出すのは、入稿先が Illustrator の `cmyk` レイヤーを
  期待する場合に**そのまま置ける値**を持たせるためで、版の分解自体は
  `shirohan.plate` のスポットカラー分解が正本。"
  (:require [clojure.string :as str]))

(defn- hex->rgb01 [hex]
  (let [h (str/replace (str hex) "#" "")
        p (fn [i] (/ #?(:clj (Integer/parseInt (subs h i (+ i 2)) 16)
                        :cljs (js/parseInt (subs h i (+ i 2)) 16))
                     255.0))]
    (if (= 6 (count h)) [(p 0) (p 2) (p 4)] [0.0 0.0 0.0])))

(defn- pct [x]
  (let [v (* 100.0 (max 0.0 (min 1.0 (double x))))]
    (/ #?(:clj (Math/round v) :cljs (js/Math.round v)) 1.0)))

(defn rgb->cmyk
  "`#rrggbb` → `{:c :m :y :k}`（各 0〜100 の整数パーセント）。

  **未較正**。ICC プロファイルを通していないので、刷り色の保証は無い。"
  [hex]
  (let [[r g b] (hex->rgb01 hex)
        k (- 1.0 (max r g b))
        d (- 1.0 k)]
    (if (< d 1e-9)
      {:c 0.0 :m 0.0 :y 0.0 :k 100.0}      ; 純黒
      {:c (pct (/ (- 1.0 r k) d))
       :m (pct (/ (- 1.0 g k) d))
       :y (pct (/ (- 1.0 b k) d))
       :k (pct k)})))

(defn cmyk->label
  "人が読む表記。`C0 M85 Y40 K12` の形。"
  [{:keys [c m y k]}]
  (str "C" (int c) " M" (int m) " Y" (int y) " K" (int k)))

(defn cmyk->pdf
  "PDF の `k` 演算子に渡す 0..1 の 4 値（`c m y k k`）。

  PDF は DeviceCMYK を素で持つので、**変換せずにそのまま書ける** —— RGB に
  落としてから CMYK に戻す往復をしない。"
  [{:keys [c m y k]}]
  [(/ c 100.0) (/ m 100.0) (/ y 100.0) (/ k 100.0)])

(def total-ink-limit
  "総インキ量（TAC）の上限の目安 [%]。枚葉オフセットのコート紙で 320〜350%。

  超えると乾かない・裏移りする。素朴な変換は K を大きく取るので普通は超えないが、
  **超えたら黙って通さない**ために持っている。"
  320.0)

(defn total-ink
  [{:keys [c m y k]}]
  (+ c m y k))

(defn over-ink-limit?
  [cmyk]
  (> (total-ink cmyk) total-ink-limit))
