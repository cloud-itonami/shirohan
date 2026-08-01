(ns shirohan.export-test
  "書き出した PSD / PDF を **他人の実装で読み直す**往復検証。

  `shirohan.psd` / `shirohan.pdf` は依存ゼロで書く（ブラウザに載せるため）が、
  自分で書いて自分で読むと仕様の誤解に気付けない。そこで読み手は kotoba-lang の
  既存リーダーを使う:

  - `kasane`（`resources/kasane/grammar/psd.edn` + `kasane.decode`）—— PSD
  - `org-iso-pdf`（`pdf.core/parse`）—— PDF

  この ns は `test/` ではなく `verify/` にある。`clojure -M:test` は依存ゼロで
  回る本体テストだけを見て、こちらは `clojure -M:verify` で回す —— 検証のために
  ライブラリ本体へ依存が忍び込むのを構造で防ぐ。"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [kasane.decode :as kd]
            [pdf.core :as pdf-reader]
            [shirohan.core :as shirohan])
  (:import [java.io ByteArrayOutputStream]))

(def ^:private two-colour
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='100' fill='#1d4ed8'/>"
       "<rect x='30' y='30' width='40' height='40' fill='#ffffff'/>"
       "</svg>"))

(defn- job [] (shirohan/plan two-colour {:print-width-mm 60 :margin-mm 6}))

;; kasane / org-iso-pdf はどちらも **0-255 の符号なし int 列**を読む
;; （`kasane.bytes/cursor` の docstring が明記）。Java の signed byte-array を
;; 渡すと 144 が -112 になって全部ずれる（実測 2026-08-01）。writer の出力が
;; もともと int ベクタなので、そのまま渡すのが正しい。
(defn- ->signed [v] (byte-array (map #(unchecked-byte %) v)))

;; ---------------------------------------------------------------- PSD

(def ^:private psd-grammar
  (read-string (slurp (io/resource "kasane/grammar/psd.edn"))))

(deftest psd-is-readable-by-kasane
  (let [{:keys [bytes width height findings]} (shirohan/photoshop (job) {:px-per-mm 2.0})]
    (is (empty? findings))
    (is (some? bytes))
    (let [doc (kd/decode psd-grammar bytes)]
      (testing "ヘッダ"
        (is (= 1 (:ver doc)))
        (is (= 3 (:chans doc)))
        (is (= :rgb (:mode doc)))
        (is (= 8 (:depth doc)))
        (is (= width (:width doc)))
        (is (= height (:height doc))))
      (testing "1 版 = 1 レイヤー、名前は版のラベル"
        (let [layers (get-in doc [:limask :layers])]
          (is (= (count (:plates (job))) (count layers)))
          (is (every? #(= 4 (:nchan %)) layers))
          (is (every? #(= "norm" (:blend %)) layers))
          (is (every? #(= 255 (:opacity %)) layers))))
      (testing "合成画像は非圧縮"
        (is (= :raw (get-in doc [:image :compression])))))))

(deftest psd-refuses-to-silently-shrink
  (let [{:keys [bytes findings]} (shirohan/photoshop (job) {:px-per-mm 200.0})]
    (is (nil? bytes) "上限を超えたら書き出さない")
    (is (= [:psd-too-large] (mapv :kind findings)))))

;; ---------------------------------------------------------------- PDF

(deftest pdf-is-readable-by-org-iso-pdf
  (let [bytes (shirohan/ai-pdf (job))
        parsed (pdf-reader/parse bytes)
        pages (pdf-reader/pages parsed)]
    (testing "1 ページ目が刷り上がり予想、以降が刷る順の版"
      (is (= (inc (count (:plates (job)))) (count pages))))
    (testing "各ページのコンテンツストリームがパスの塗りを含む"
      (doseq [p pages]
        ;; `pages` は parse 結果全体を取るが、`page-content-str` は
          ;; **`:objects` マップ**を取る（引数の形が違う）。
          (let [s (pdf-reader/page-content-str (:objects parsed) p)]
          (is (re-find #"\bm\b" s))
          (is (re-find #"\bf\b" s))
          (is (re-find #"\brg\b" s)))))))

(deftest pdf-declares-no-font-because-there-is-no-text
  (let [s (String. (->signed (shirohan/ai-pdf (job))) "ISO-8859-1")]
    (is (not (re-find #"/Font" s))
        "使わないリソースを宣言すると『文字がある』と嘘をつくことになる")))
