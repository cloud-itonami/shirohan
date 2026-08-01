(ns shirohan.plate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [shirohan.geom :as geom]
            [shirohan.plate :as plate]
            [shirohan.svg :as svg]
            [shirohan.core :as shirohan]))

(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- near? [a b tol] (< (abs* (- a b)) tol))

(def ^:private two-colour
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='40' fill='#ff2d95'/>"
       "<rect x='0' y='60' width='100' height='40' fill='#1d4ed8'/>"
       "</svg>"))

;; 「O」の字: 外周と内周（穴）。塗りは 1 色なので 1 版になる。
(def ^:private donut
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<path fill='#000000' d='M0 0 L100 0 L100 100 L0 100 Z"
       " M30 30 L30 70 L70 70 L70 30 Z'/>"
       "</svg>"))

;; ---------------------------------------------------------------- 版の構成

(deftest white-plate-comes-first-then-lightest-spot
  (let [{:keys [plates]} (shirohan/plan two-colour)]
    (is (= 3 (count plates)))
    (is (= :white (:id (first (sort-by :order plates)))))
    (testing "スポット版は明るい順（マゼンタ→青）"
      (let [spots (sort-by :order (remove :underbase? plates))]
        (is (= ["#ff2d95" "#1d4ed8"] (mapv :color spots)))))))

(deftest one-plate-per-distinct-fill
  (let [{:keys [plates]} (shirohan/plan two-colour)]
    (is (= 2 (count (remove :underbase? plates))))))

(deftest underbase-can-be-turned-off
  (let [{:keys [plates]} (shirohan/plan two-colour {:white-underbase? false})]
    (is (empty? (filter :underbase? plates)))))

;; ---------------------------------------------------------------- choke

(deftest choke-shrinks-the-white-plate
  (let [{:keys [plates]} (shirohan/plan two-colour {:choke-mm 0.5 :print-width-mm 100})
        white (first (filter :underbase? plates))
        spots (remove :underbase? plates)
        white-area (reduce + 0.0 (map geom/area (:art white)))
        art-area (reduce + 0.0 (map geom/area (mapcat :art spots)))]
    (is (< white-area art-area) "白版は必ず図案より小さい")
    (testing "縮み量が choke に見合う（4 辺 × 2 枚の帯）"
      ;; 100mm × 40mm の帯 2 枚 → choke 0.5mm で (99×39)×2
      (is (near? (* 2 (* 99.0 39.0)) white-area 0.5)))))

(deftest choke-grows-the-hole-not-shrinks-it
  (testing "「O」の内周は穴なので、白版では **広がる**（白が痩せる）"
    (let [{:keys [plates spec]} (shirohan/plan donut {:choke-mm 1.0 :print-width-mm 100})
          white (first (filter :underbase? plates))
          spot (first (remove :underbase? plates))
          ;; 面積の小さい方が内周（穴）
          hole-before (apply min (map geom/area (:art spot)))
          hole-after (apply min (map geom/area (:art white)))
          outer-before (apply max (map geom/area (:art spot)))
          outer-after (apply max (map geom/area (:art white)))]
      (is (> hole-after hole-before) "穴は広がる")
      (is (< outer-after outer-before) "外周は縮む")
      (testing "その結果、白のインク面積は両側から痩せる"
        (is (< (- outer-after hole-after) (- outer-before hole-before)))))))

(deftest nesting-depth-distinguishes-outer-from-hole
  (let [outer {:points [[0 0] [100 0] [100 100] [0 100]] :closed? true}
        inner {:points [[30 30] [30 70] [70 70] [70 30]] :closed? true}
        both [outer inner]]
    (is (= 0 (plate/nesting-depth outer both)))
    (is (= 1 (plate/nesting-depth inner both)))))

;; ---------------------------------------------------------------- 白抜き

(def ^:private with-knockout
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='100' fill='#1d4ed8'/>"
       "<rect x='30' y='30' width='40' height='40' fill='#ffffff'/>"
       "</svg>"))

(deftest knockout-lands-on-every-plate
  (let [{:keys [plates]} (shirohan/plan with-knockout)]
    (is (every? #(= 1 (count (:knockout %))) plates))))

(deftest knockout-area-is-subtracted-from-plate-area
  (let [{:keys [plates]} (shirohan/plan with-knockout {:choke-mm 0.0 :print-width-mm 100})
        spot (first (remove :underbase? plates))]
    (is (near? (- (* 100.0 100.0) (* 40.0 40.0)) (:area-mm2 spot) 0.5))))

(deftest knockout-outside-the-art-is-reported
  (let [svg (str "<svg viewBox='0 0 200 100'>"
                 "<rect x='0' y='0' width='50' height='50' fill='#000000'/>"
                 "<rect x='120' y='20' width='20' height='20' fill='#ffffff'/>"
                 "</svg>")
        kinds (set (map :kind (:findings (shirohan/plan svg))))]
    (is (contains? kinds :knockout-outside-art))))

;; ---------------------------------------------------------------- QC

(deftest choke-that-would-erase-a-feature-is-reported
  (let [svg (str "<svg viewBox='0 0 100 100'>"
                 "<rect x='0' y='0' width='100' height='100' fill='#000'/>"
                 ;; 刷り幅 100mm に対して 0.2mm の細線
                 "<rect x='10' y='50' width='80' height='0.2' fill='#0000ff'/>"
                 "</svg>")
        fs (:findings (shirohan/plan svg {:choke-mm 0.5 :print-width-mm 100}))]
    (is (contains? (set (map :kind fs)) :choke-erases-feature))
    (is (some shirohan/blocking? fs) "白版から消える図形は刷る前に止める")))

(deftest thin-line-below-min-width-is-reported-but-not-blocking
  (let [f {:kind :below-min-line}]
    (is (not (shirohan/blocking? f))))
  (is (shirohan/blocking? {:kind :no-art}))
  (is (shirohan/blocking? {:kind :text-not-outlined})))

(deftest clean-artwork-has-no-blocking-findings
  (let [fs (:findings (shirohan/plan two-colour {:choke-mm 0.2 :print-width-mm 280}))]
    (is (empty? (filter shirohan/blocking? fs))
        (str "想定外の blocking finding: " (pr-str (filter shirohan/blocking? fs))))))

;; ---------------------------------------------------------------- 寸法

(deftest plate-size-is-art-plus-margin-on-both-sides
  (let [{:keys [size]} (shirohan/plan two-colour {:print-width-mm 200 :margin-mm 15})]
    (is (near? (+ 200.0 30.0) (:width-mm size) 1e-6))
    (is (near? (+ 200.0 30.0) (:height-mm size) 1e-6))))

(deftest all-plates-share-one-origin-and-size
  (testing "見当が合うことは「全版が同じ viewBox を持つ」ことに等しい"
    (let [job (shirohan/plan two-colour)
          vbs (set (map #(second (re-find #"viewBox=\"([^\"]+)\"" (svg/plate-svg job %)))
                        (:plates job)))]
      (is (= 1 (count vbs))))))

;; ---------------------------------------------------------------- SVG 出力

(deftest film-is-black-on-white-not-the-ink-colour
  (let [job (shirohan/plan two-colour)
        spot (first (remove :underbase? (:plates job)))
        film (svg/plate-svg job spot {:as :film})]
    (is (str/includes? film "fill=\"#000000\""))
    (is (not (str/includes? film "fill=\"#ff2d95\"")))))

(deftest ink-rendering-uses-the-real-colour
  (let [job (shirohan/plan two-colour)
        spot (first (remove :underbase? (:plates job)))]
    (is (str/includes? (svg/plate-svg job spot {:as :ink}) "fill=\"#ff2d95\""))))

(deftest knockout-becomes-a-mask-not-a-boolean
  (let [job (shirohan/plan with-knockout)
        s (svg/plate-svg job (first (:plates job)))]
    (is (str/includes? s "<mask"))
    (is (str/includes? s "mask=\"url(#ko-white)\""))))

(deftest plates-without-knockout-carry-no-mask
  (let [job (shirohan/plan two-colour)
        s (svg/plate-svg job (first (:plates job)))]
    (is (not (str/includes? s "<mask")))))

(deftest registration-marks-are-on-every-film
  (let [job (shirohan/plan two-colour)]
    (doseq [p (:plates job)]
      (let [s (svg/plate-svg job p)]
        ;; 四隅に円 1 個ずつ
        (is (= 4 (count (re-seq #"<circle" s))) (str (:label p) " に見当が 4 つない"))))))

(deftest registration-can-be-turned-off
  (let [job (shirohan/plan two-colour {:registration? false})
        s (svg/plate-svg job (first (:plates job)))]
    (is (zero? (count (re-seq #"<circle" s))))))

(deftest film-filenames-sort-in-press-order
  (let [names (sort (keys (shirohan/films (shirohan/plan two-colour))))]
    (is (= "00-white.svg" (first names)) "白版は刷る順 0 番")
    (is (= 3 (count names)))
    (is (every? #(re-matches #"\d\d-[a-z0-9]+\.svg" %) names))))

(deftest preview-paints-plates-in-press-order-on-the-garment
  (let [job (shirohan/plan two-colour {:garment-color "#111111"})
        s (shirohan/preview job)]
    (is (str/includes? s "fill=\"#111111\""))
    (testing "白版が最初に塗られる（後の色版が上に乗る）"
      (is (< (str/index-of s "#ffffff") (str/index-of s "#ff2d95"))))))

(deftest underbase-check-outlines-the-art-over-the-white
  (let [s (shirohan/underbase-check (shirohan/plan two-colour))]
    (is (str/includes? s "stroke=\"#ff2d95\""))
    (is (str/includes? s "fill=\"#ffffff\""))))

;; ---------------------------------------------------------------- 決定性

(deftest plan-is-a-pure-function
  (testing "同じ入力から同じ版が出る（版下を差分でレビューできる根拠）"
    (is (= (shirohan/films (shirohan/plan two-colour))
           (shirohan/films (shirohan/plan two-colour))))))

(deftest summary-is-audit-shaped
  (let [s (shirohan/summary (shirohan/plan two-colour {:choke-mm 0.25}))]
    (is (= 3 (:plate-count s)))
    (is (= 0.25 (:choke-mm s)))
    (is (= 0 (:blocking s)))
    (is (every? #(contains? % :area-mm2) (:plates s)))))
