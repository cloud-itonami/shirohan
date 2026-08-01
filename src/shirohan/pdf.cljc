(ns shirohan.pdf
  "版一式を **PDF** として書き出す。拡張子を `.ai` にすれば Illustrator が開ける。

  ## 「AI 書き出し」と称するものの正体

  現行の `.ai` は **PDF 互換形式**で、Illustrator は自分の私的データ
  （`AIPrivateData` ストリーム）を PDF の中に同居させている。私的データは非公開
  仕様なので、**ここが書くのは私的データ抜きの素の PDF** —— Illustrator で開いて
  パスとして編集できるが、Illustrator が保存した `.ai` と同一ではない。
  `shirohan` はそれを `.ai` と偽らず、`:label` で「PDF ベース」と明記する。

  ## なぜ `org-iso-pdf` の `write-document` を使わないのか

  使う —— **JVM のテストで**。`pdf.core/write-document` は `org-ietf-deflate`
  （Huffman + LZ77 の完全実装、1300行超）を ns 依存で引くので、ブラウザの
  scittle に載せると起動が重くなる。ここが必要なのは非圧縮のコンテンツ
  ストリームだけで deflate は 1 バイトも要らない。

  そのかわり**書き手を2つにしない**ために、テストでは `pdf.core/parse` に
  この出力を食わせて往復させる（`test/shirohan/export_test.clj`）—— 独立した
  実装が読めることが、この writer が仕様に合っている根拠。

  ## 座標

  PDF は原点が左下・y が上。版は左上原点・y が下（SVG と同じ）なので、
  ページ先頭で `1 0 0 -1 0 H cm` を掛けて反転する。単位は pt（1/72 inch）で、
  mm からは 72/25.4 倍。"
  (:require [clojure.string :as str]
            [shirohan.geom :as geom]))

(def ^:private mm->pt (/ 72.0 25.4))
(def ^:private fmt geom/fmt)

(defn- hex->unit
  "`#rrggbb` → PDF の `r g b`（0..1）。"
  [hex]
  (let [h (str/replace (str hex) "#" "")
        p (fn [i] (/ #?(:clj (Integer/parseInt (subs h i (+ i 2)) 16)
                        :cljs (js/parseInt (subs h i (+ i 2)) 16))
                     255.0))]
    (if (= 6 (count h))
      (str (fmt (p 0)) " " (fmt (p 2)) " " (fmt (p 4)))
      "0 0 0")))

(defn- contour->ops [{:keys [points]} reverse?]
  (let [pts (if reverse? (vec (reverse points)) points)]
    (str (str/join " " (map-indexed
                        (fn [i [x y]]
                          (str (fmt (* x mm->pt)) " " (fmt (* y mm->pt))
                               (if (zero? i) " m" " l")))
                        pts))
         " h")))

(defn plate-ops
  "版 1 枚のパス塗り。

  白抜きは **art の外周と逆向きに回した輪郭**として同じパスに足し、nonzero で
  塗る —— PDF には「消す」演算が無い（透明グループを使わない限り）ので、巻き数を
  打ち消して穴にする。白抜きどうしが重なると打ち消しが 2 重になって穴が閉じるが、
  それは `shirohan.svg` の mask 版と違う既知の差で、README に書いてある。"
  [{:keys [art knockout]} ink]
  (let [s (if (seq art) (geom/orientation (first art)) 1)
        ops (concat (map #(contour->ops % false) art)
                    (map #(contour->ops % (= s (geom/orientation %))) knockout))]
    (when (seq ops)
      (str (hex->unit ink) " rg\n" (str/join "\n" ops) "\nf\n"))))

(defn- page-content [{:keys [size]} body]
  (str "q\n1 0 0 -1 0 " (fmt (* (:height-mm size) mm->pt)) " cm\n" body "Q\n"))

(defn pages
  "書き出すページ列。1 ページ目が刷り上がり予想、以降が刷る順の版。

  版下ページを黒 100% にするのは製版と同じ理由（感光乳剤は光の有無しか見ない）。"
  [{:keys [size spec plates] :as job}]
  (let [w (* (:width-mm size) mm->pt)
        h (* (:height-mm size) mm->pt)
        ordered (sort-by :order plates)
        preview (str (hex->unit (:garment-color spec)) " rg\n"
                     "0 0 " (fmt w) " " (fmt h) " re\nf\n"
                     (str/join (keep #(plate-ops % (:color %)) ordered)))]
    (into [{:width w :height h :content (page-content job preview) :label "刷り上がり予想"}]
          (map (fn [p]
                 {:width w :height h
                  :content (page-content job (or (plate-ops p "#000000") ""))
                  :label (:label p)})
               ordered))))

;; ---------------------------------------------------------------- 文書

(defn- pad10 [n]
  (let [s (str n)] (str (apply str (repeat (max 0 (- 10 (count s))) "0")) s)))

(defn write-document
  "ページ列 → PDF 1.4 のバイト列（0-255 の整数ベクタ）。

  非圧縮・古典的な xref/trailer だけの最小構成。フォントを持たないのは、この
  文書に文字が 1 つも無いから（版はパスだけ）—— 使わないリソースを宣言すると、
  読み手に「文字がある」と嘘をつくことになる。"
  [pages]
  (when-not (seq pages)
    (throw (ex-info "PDF には最低 1 ページ要る" {})))
  (let [pages (vec pages)
        page-id #(+ 3 (* 2 %))
        content-id #(+ 4 (* 2 %))
        kids (str/join " " (map #(str (page-id %) " 0 R") (range (count pages))))
        bodies (vec (concat
                     ["<< /Type /Catalog /Pages 2 0 R >>"
                      (str "<< /Type /Pages /Kids [" kids "] /Count " (count pages) " >>")]
                     (mapcat (fn [i {:keys [width height content]}]
                               [(str "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 "
                                     (fmt width) " " (fmt height)
                                     "] /Resources << >> /Contents " (content-id i)
                                     " 0 R >>")
                                (str "<< /Length " (count content) " >>\nstream\n"
                                     content "endstream")])
                             (range) pages)))
        objects (mapv (fn [i b] (str (inc i) " 0 obj\n" b "\nendobj\n")) (range) bodies)
        header "%PDF-1.4\n"
        offsets (loop [pos (count header) rem objects out []]
                  (if-let [o (first rem)]
                    (recur (+ pos (count o)) (next rem) (conj out pos))
                    out))
        body (apply str objects)
        xref-at (+ (count header) (count body))
        xref (str "xref\n0 " (inc (count objects)) "\n0000000000 65535 f \n"
                  (apply str (map #(str (pad10 %) " 00000 n \n") offsets)))
        trailer (str "trailer\n<< /Size " (inc (count objects)) " /Root 1 0 R >>\n"
                     "startxref\n" xref-at "\n%%EOF\n")]
    (mapv #(bit-and (int %) 0xff) (str header body xref trailer))))

(defn job->pdf
  "版一式 → PDF バイト列。`.ai` として保存してよい（PDF ベース）。"
  [job]
  (write-document (pages job)))
