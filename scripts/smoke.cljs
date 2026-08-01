;; SCI（nbb / scittle）で組版カーネルが動くことの煙試験。
;; ブラウザ側は scittle が同じ .cljc を読むので、ここが通れば公開サイトでも動く。
(require '[shirohan.core :as shirohan])

(def svg
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='40' fill='#ff2d95'/>"
       "<circle cx='50' cy='75' r='20' fill='#ffffff'/>"
       "</svg>"))

(let [job (shirohan/plan svg {:choke-mm 0.2 :print-width-mm 260})
      s (shirohan/summary job)]
  (println "plates:" (:plate-count s) (mapv :label (:plates s)))
  (println "size:" (:size s))
  (println "findings:" (:findings s) "blocking:" (:blocking s))
  (println "films:" (sort (keys (shirohan/films job))))
  (println "preview bytes:" (count (shirohan/preview job)))
  (assert (= 2 (:plate-count s)) "白版 + スポット 1 版")
  (assert (zero? (:blocking s)) "この図案に blocking は無いはず")
  (println "SCI smoke: OK"))
