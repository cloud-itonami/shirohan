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
  "白抜き（生地を見せる穴）は **明示指定**。塗りが白かどうかでは決まらない ——
  白版は『白インクを塗る部分の指示』なので、図案の白い部分は既定では白版に
  **含まれる**（実務家の指摘、2026-08-01）。"
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='100' fill='#1d4ed8'/>"
       "<rect id='knockout-window' x='30' y='30' width='40' height='40' fill='#ffffff'/>"
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
                 "<rect id='knockout-stray' x='120' y='20' width='20' height='20' fill='#ffffff'/>"
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

;; ---------------------------------------------------------------- 白版の意味
;;
;; 実務家の指摘（2026-08-01）:
;;   「白版は白インクを塗る部分の指示です。白インクの上に色インクを印刷しないと
;;    色が出ないので、1色ベタ塗り部分＝白インク を印刷してから色インクを印刷します。
;;    その際に、若干白インクと色インクの印刷がズレるため、白インクは 0.1mm 小さく
;;    作ります」
;;
;; 当初この repo は白（#ffffff）を既定で白抜き（穴）として扱っていた。意味が逆で、
;; そのままだと図案の白い部分が刷られずに生地が出る。

(def ^:private white-inside-colour
  "青い面の中に白い図形。**白い部分も白インクで刷る**ので、白版はベタのまま。"
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='100' fill='#1d4ed8'/>"
       "<rect x='30' y='30' width='40' height='40' fill='#ffffff'/>"
       "</svg>"))

(deftest the-default-choke-is-the-shop-floor-value
  (is (= 0.1 (:choke-mm plate/default-spec))
      "白インクと色インクのズレぶんだけ小さく作る = 0.1mm"))

(deftest white-artwork-is-white-ink-not-a-hole
  (testing "既定では白を白抜きにしない"
    (is (nil? (:knockout-fill plate/default-spec)))
    (let [{:keys [plates]} (shirohan/plan white-inside-colour)]
      (is (every? #(empty? (:knockout %)) plates)
          "白い図形が穴になってはいけない"))))

(deftest the-white-plate-is-a-solid-silhouette
  (testing "白版は図案が乗る面のベタ塗り —— 中の白い部分も含む"
    (let [{:keys [plates]} (shirohan/plan white-inside-colour
                                          {:choke-mm 0.0 :print-width-mm 100})
          white (first (filter :underbase? plates))
          outer (apply max (map geom/area (:art white)))]
      (is (near? (* 100.0 100.0) outer 0.5)
          "外周は図案全体（100×100mm）")
      (testing "内側の白い四角は穴ではなく、同じ向きで重なるので塗りは solid"
        (let [inner (filter #(< (geom/area %) 5000.0) (:art white))]
          (is (= 1 (count inner)))
          (is (= (geom/orientation (first (:art white)))
                 (geom/orientation (first inner)))
              "向きが同じ = nonzero で union（穴にならない）"))))))

(deftest no-redundant-white-spot-plate-under-a-white-underbase
  (testing "白版の上にもう一度白を刷っても意味が無い"
    (let [{:keys [plates]} (shirohan/plan white-inside-colour)]
      (is (nil? (some #(and (not (:underbase? %)) (= "#ffffff" (:color %))) plates))))
    (testing "白版を出さない設定（淡色ボディ）では、白も普通の 1 色として刷る"
      (let [{:keys [plates]} (shirohan/plan white-inside-colour {:white-underbase? false})]
        (is (some #(= "#ffffff" (:color %)) plates))))))

(deftest a-genuine-subpath-hole-is-still-a-hole
  (testing "同じ path の中のサブパス（「O」の内周）は穴のまま"
    (let [{:keys [plates]} (shirohan/plan donut {:choke-mm 1.0 :print-width-mm 100})
          white (first (filter :underbase? plates))
          [outer inner] (sort-by (comp - geom/area) (:art white))]
      (is (not= (geom/orientation outer) (geom/orientation inner))
          "向きが逆 = nonzero で穴"))))

;; ---------------------------------------------------------------- 縮小ではなく削る
;;
;; 実務家の指摘（2026-08-01、2回目）:
;;   「単純に 0.1mm 縮小ではなく、ベクターの外側を 0.1mm 削るが正しいです。
;;    例えばドーナツ型の場合、単純に 0.1mm 縮小だと、ドーナツの穴の部分に
;;    白版が出てきてしまいます」
;;
;; 縮小（相似変換）と侵食（オフセット）は、穴のある形で結果が逆になる:
;;
;;   縮小 : 外周も穴も**同じ比率で小さくなる** → 穴が縮む → 穴に白版がはみ出す
;;   侵食 : インクの面を全周から削る          → 外周は内へ、**穴は外へ**
;;
;; この試験は「穴の半径が **増える**」ことを固定する。減っていたら縮小になっている。

(def ^:private donut-svg
  "1 本の path のサブパスで作ったドーナツ（穴は透明）。外周 r=80、穴 r=35。"
  (str "<svg viewBox='0 0 200 200'><path fill='#000000' "
       "d='M100 20 A80 80 0 1 0 100 180 A80 80 0 1 0 100 20 Z"
       " M100 65 A35 35 0 1 1 100 135 A35 35 0 1 1 100 65 Z'/></svg>"))

(defn- radius-of [c]
  (Math/sqrt (/ (geom/area c) Math/PI)))

(deftest choke-erodes-the-ink-face-it-does-not-scale-the-shape
  (let [job (shirohan/plan donut-svg {:choke-mm 1.0 :print-width-mm 160})
        white (first (filter :underbase? (:plates job)))
        spot (first (remove :underbase? (:plates job)))
        [art-outer art-hole] (sort-by (comp - geom/area) (:art spot))
        [w-outer w-hole] (sort-by (comp - geom/area) (:art white))]
    (testing "外周は内側へ 1mm"
      (is (near? (- (radius-of art-outer) 1.0) (radius-of w-outer) 0.05)))
    (testing "**穴は外側へ 1mm**（縮小ならここが逆になり、穴に白版がはみ出す）"
      (is (near? (+ (radius-of art-hole) 1.0) (radius-of w-hole) 0.05))
      (is (> (radius-of w-hole) (radius-of art-hole))
          "穴が縮んでいる = 相似縮小になっている"))
    (testing "白版の面積は図案より小さい（両側から削られている）"
      (let [ink (fn [o h] (- (geom/area o) (geom/area h)))]
        (is (< (ink w-outer w-hole) (ink art-outer art-hole)))))))

(deftest a-transparent-hole-survives-the-raster-path-too
  (testing "ラスタから起こしても穴は穴のまま広がる"
    (let [W 64
          img {:width W :height W
               :data (vec (mapcat
                           (fn [i]
                             (let [x (mod i W) y (quot i W)
                                   dx (- x 31.5) dy (- y 31.5)
                                   d (Math/sqrt (+ (* dx dx) (* dy dy)))]
                               (if (and (<= d 28) (>= d 12)) [0 0 0 255] [0 0 0 0])))
                           (range (* W W))))}
          job (shirohan/plan-image img {:colors 2 :print-width-mm 160 :choke-mm 1.0})
          white (first (filter :underbase? (:plates job)))
          [outer hole] (sort-by (comp - geom/area) (:art white))]
      (is (= 2 (count (:art white))) "外周と穴の 2 本")
      (is (not= (geom/orientation outer) (geom/orientation hole))
          "向きが逆 = nonzero で穴になる")
      (is (> (radius-of hole) 34.0)
          "穴は元の r≈34.3mm より広がっている（狭まっていたら縮小になっている）"))))
