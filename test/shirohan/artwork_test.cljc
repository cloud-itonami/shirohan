(ns shirohan.artwork-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [shirohan.path :as path]
            [shirohan.geom :as geom]
            [shirohan.artwork :as artwork]))

(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- near? [a b tol] (< (abs* (- a b)) tol))

;; ---------------------------------------------------------------- path

(deftest parses-absolute-polygon
  (let [cs (path/parse "M0 0 L10 0 L10 10 L0 10 Z")]
    (is (= 1 (count cs)))
    (is (true? (:closed? (first cs))))
    (is (= [[0.0 0.0] [10.0 0.0] [10.0 10.0] [0.0 10.0]] (:points (first cs))))))

(deftest parses-relative-and-implicit-repeat
  (testing "l の暗黙繰り返し（1 コマンドで 3 辺）"
    (let [cs (path/parse "m0 0 l10 0 0 10 -10 0 z")]
      (is (= 1 (count cs)))
      (is (= 4 (count (:points (first cs)))))
      (is (near? 100.0 (geom/area (first cs)) 1e-9))))
  (testing "M の後の数は暗黙の L"
    (let [cs (path/parse "M0 0 10 0 10 10 0 10 Z")]
      (is (= 4 (count (:points (first cs))))))))

(deftest parses-h-v-and-unspaced-negatives
  (let [cs (path/parse "M0 0H10V10H0Z")]
    (is (near? 100.0 (geom/area (first cs)) 1e-9)))
  (testing "区切り無しの負数 `10-5` を 2 トークンに割る"
    (is (= ["M" "0" "0" "L" "10" "-5"] (path/tokenize "M0 0L10-5")))))

(deftest z-returns-to-subpath-start
  (testing "Z のあと M 無しで描き続けると同じ始点から始まる"
    (let [cs (path/parse "M0 0H10V10H0Z L20 20 L20 30 L0 30 Z")]
      (is (= 2 (count cs)))
      (is (every? :closed? cs)))))

(deftest flattens-curves-within-tolerance
  (testing "2 つの円弧で作った円の面積が πr² に寄る"
    (let [d "M0 5 A5 5 0 1 0 10 5 A5 5 0 1 0 0 5 Z"
          c (first (path/parse d {:tolerance-mm 0.01}))
          expected (* #?(:clj Math/PI :cljs js/Math.PI) 25.0)]
      (is (near? expected (geom/area c) 0.05)
          (str "area=" (geom/area c) " expected=" expected))))
  (testing "許容誤差を緩めると点数が減る"
    (let [d "M0 0 C0 10 10 10 10 0"
          fine (count (:points (first (path/parse d {:tolerance-mm 0.01}))))
          coarse (count (:points (first (path/parse d {:tolerance-mm 1.0}))))]
      (is (> fine coarse)))))

(deftest open-subpaths-are-kept-not-dropped
  (let [cs (path/parse "M0 0 L10 0 L10 10")]
    (is (= 1 (count cs)))
    (is (false? (:closed? (first cs))))))

;; ---------------------------------------------------------------- 色

(deftest normalizes-fill
  (is (= "#aabbcc" (artwork/normalize-fill "#abc")))
  (is (= "#ff0000" (artwork/normalize-fill "red")))
  (is (= "#123456" (artwork/normalize-fill "#123456")))
  (is (= "#ffffff" (artwork/normalize-fill "  #FFFFFF ")))
  (is (= "#0a141e" (artwork/normalize-fill "rgb(10, 20, 30)")))
  (is (= :none (artwork/normalize-fill "none")))
  (is (= :unresolvable (artwork/normalize-fill "url(#grad1)")))
  (testing "塗り指定が無ければ SVG 既定の黒"
    (is (= "#000000" (artwork/normalize-fill nil)))))

;; ---------------------------------------------------------------- transform

(deftest composes-transforms-left-to-right
  (let [m (artwork/parse-transform "translate(10,20) scale(2)")]
    ;; scale が先に、translate が後に効く: (1,1) → (2,2) → (12,22)
    (is (= [2.0 0.0 0.0 2.0 10.0 20.0] m))))

;; ---------------------------------------------------------------- load-svg

(def ^:private svg-basic
  (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
       "<rect x='0' y='0' width='100' height='50' fill='#ff2d95'/>"
       "<circle cx='50' cy='75' r='20' fill='#ffffff'/>"
       "</svg>"))

(deftest loads-shapes-and-scales-to-print-width
  (let [{:keys [contours bbox]} (artwork/load-svg svg-basic {:print-width-mm 200})]
    (is (= 2 (count contours)))
    (testing "図案の幅が指定の刷り幅になる"
      (is (near? 200.0 (- (:x1 bbox) (:x0 bbox)) 1e-6)))
    (testing "原点は左上"
      (is (near? 0.0 (:x0 bbox) 1e-6))
      (is (near? 0.0 (:y0 bbox) 1e-6)))))

(deftest white-fill-is-ink-not-a-hole-by-default
  (testing "白版は『白インクを塗る部分の指示』—— 白い図形も刷る対象（実務家の指摘 2026-08-01）"
    (let [{:keys [contours]} (artwork/load-svg svg-basic)]
      (is (= #{:art} (set (map :role contours))))
      (is (some #(= "#ffffff" (:fill %)) contours)))))

(deftest knockout-can-be-opted-into-by-fill
  (testing "「生地を見せる穴」が要るときだけ明示する"
    (let [{:keys [contours]} (artwork/load-svg svg-basic {:knockout-fill "#ffffff"})]
      (is (= #{:art :knockout} (set (map :role contours))))
      (is (= "#ffffff" (:fill (first (filter #(= :knockout (:role %)) contours))))))))

(deftest contours-from-one-element-share-a-shape-id
  (testing "穴かどうかは同じ :shape の中でしか判定できない —— 別要素は上に乗るだけ"
    (let [{:keys [contours]} (artwork/load-svg svg-basic)]
      (is (= 2 (count (distinct (map :shape contours))))
          "2 要素なら :shape は 2 種類"))
    (let [donut "<svg><path fill='#000' d='M0 0H50V50H0Z M10 10H40V40H10Z'/></svg>"
          {:keys [contours]} (artwork/load-svg donut)]
      (is (= 1 (count (distinct (map :shape contours))))
          "1 つの d から出たサブパスは同じ :shape"))))

(deftest id-marks-knockout-regardless-of-fill
  (let [svg (str "<svg><rect width='100' height='100' fill='#000000'/>"
                 "<rect id='knockout-1' x='10' y='10' width='20' height='20' fill='#00ff00'/></svg>")
        {:keys [contours]} (artwork/load-svg svg)]
    (is (= 1 (count (filter #(= :knockout (:role %)) contours))))))

(deftest reports-what-it-cannot-read
  (let [svg (str "<svg>"
                 "<rect width='10' height='10' fill='#000'/>"
                 "<text x='1' y='1'>アウトライン化していない文字</text>"
                 "<image href='a.png' x='0' y='0' width='5' height='5'/>"
                 "<path d='M0 0 L5 5 L5 0 Z' fill='none' stroke='#000'/>"
                 "<path d='M0 0 L5 5 L5 0 Z' fill='url(#g)'/>"
                 "</svg>")
        kinds (set (map :kind (:findings (artwork/load-svg svg))))]
    (is (contains? kinds :text-not-outlined))
    (is (contains? kinds :raster-image))
    (is (contains? kinds :stroke-only))
    (is (contains? kinds :unresolvable-fill))))

(deftest defs-subtree-is-not-drawn
  (let [svg (str "<svg>"
                 "<defs><path id='hidden' d='M0 0 L100 0 L100 100 Z' fill='#000'/></defs>"
                 "<rect width='10' height='10' fill='#000'/>"
                 "</svg>")
        {:keys [contours]} (artwork/load-svg svg)]
    (is (= 1 (count contours)) "defs の中のパスを図案として拾ってはいけない")))

(deftest group-transform-is-inherited
  (let [svg (str "<svg>"
                 "<g transform='translate(100,0)'>"
                 "<rect x='0' y='0' width='10' height='10' fill='#000'/></g>"
                 "<rect x='0' y='0' width='10' height='10' fill='#000'/>"
                 "</svg>")
        {:keys [bbox]} (artwork/load-svg svg {:print-width-mm 110})]
    (testing "g の translate が効いて図案全体の幅が 110 になる"
      (is (near? 110.0 (- (:x1 bbox) (:x0 bbox)) 1e-6)))))

(deftest group-fill-is-inherited
  (let [svg "<svg><g fill='#00ff00'><rect width='10' height='10'/></g></svg>"
        {:keys [contours]} (artwork/load-svg svg)]
    (is (= "#00ff00" (:fill (first contours))))))

(deftest empty-artwork-is-reported-not-crashed
  (let [{:keys [contours findings]} (artwork/load-svg "<svg></svg>")]
    (is (empty? contours))
    (is (contains? (set (map :kind findings)) :no-art))))

(deftest style-block-does-not-break-the-scanner
  (let [svg (str "<svg><style>.a{fill:#000} rect{stroke:none}</style>"
                 "<rect width='10' height='10' fill='#000'/></svg>")
        {:keys [contours]} (artwork/load-svg svg)]
    (is (= 1 (count contours)))))

(deftest no-inline-regex-flags-in-source
  (testing "(?s) / (?i) は JS の RegExp に無い —— ブラウザで読めなくなる"
    (doseq [f ["src/shirohan/geom.cljc" "src/shirohan/path.cljc"
               "src/shirohan/artwork.cljc" "src/shirohan/plate.cljc"
               "src/shirohan/svg.cljc" "src/shirohan/core.cljc"]]
      #?(:clj (let [src (slurp f)]
                ;; 正規表現リテラルの中だけを見る（docstring での言及は対象外）
                (is (not (re-find #"#\"[^\"]*\(\?[sim]\)" src))
                    (str f " の正規表現リテラルに inline flag がある")))
         :cljs (is true)))))
