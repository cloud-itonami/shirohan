(ns shirohan.raster-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [shirohan.geom :as geom]
            [shirohan.curve :as curve]
            [shirohan.raster :as raster]
            [shirohan.core :as shirohan]))

(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- near? [a b tol] (< (abs* (- a b)) tol))

;; ---------------------------------------------------------------- 合成画像

(defn- image
  "`f` は [x y] → `[r g b a]`。"
  [w h f]
  {:width w :height h
   :data (vec (mapcat (fn [i] (f (mod i w) (quot i w))) (range (* w h))))})

(def ^:private donut
  "青い正方形の中に白い穴。地は透過。"
  (image 24 24
         (fn [x y]
           (let [in? (and (>= x 3) (< x 21) (>= y 3) (< y 21))
                 hole? (and (>= x 9) (< x 15) (>= y 9) (< y 15))]
             (cond hole? [255 255 255 255]
                   in? [29 78 216 255]
                   :else [0 0 0 0])))))

(deftest traces-outer-and-hole-with-opposite-winding
  (let [{:keys [contours palette]} (raster/trace donut {:colors 2 :min-area-px 4})]
    (is (= ["#1d4ed8" "#ffffff"] (sort palette)))
    (testing "青は外周と穴の2本、白は面が1本"
      (is (= {"#1d4ed8" 2 "#ffffff" 1} (frequencies (map :fill contours)))))
    (testing "外周と穴は逆向き（`shirohan.plate` の入れ子判定がこれに乗る）"
      (let [blue (filter #(= "#1d4ed8" (:fill %)) contours)
            [outer hole] (sort-by (comp - geom/area) blue)]
        (is (not= (geom/orientation outer) (geom/orientation hole)))))))

(deftest transparent-background-is-not-ink
  (let [{:keys [contours]} (raster/trace donut {:colors 2 :min-area-px 4})
        total (reduce + 0.0 (map geom/area contours))]
    ;; 地まで拾っていたら 24*24=576 になる
    (is (< total 400.0))))

(deftest empty-image-is-reported-not-crashed
  (let [{:keys [contours findings]}
        (raster/trace (image 8 8 (fn [_ _] [0 0 0 0])) {:colors 2})]
    (is (empty? contours))
    (is (= [:no-art] (mapv :kind findings)))))

;; ---------------------------------------------------------------- 量子化

(def ^:private measured-histogram
  "**ブラウザの canvas で実際に縮小した図案**の標本分布（2026-08-01 実測）。

  赤い円 + 黄の帯 + 白い穴を 300×200 で描き、192×128 へ縮小したもの。縁の
  アンチエイリアスで 42 色になる。合成画像（混色ゼロ）では出なかったバグが
  ここでだけ出たので、**実測値をそのまま固定してある** —— 手で作った綺麗な
  分布に置き換えると、この回帰試験は意味を失う。"
  [[[225 29 72] 1690] [[250 204 21] 648] [[255 255 255] 306] [[250 205 21] 72]
   [[225 29 73] 6] [[226 29 72] 6] [[225 30 72] 6] [[225 30 73] 4] [[226 29 73] 4]
   [[224 29 72] 4] [[255 252 253] 4] [[249 213 221] 2] [[230 65 101] 2]
   [[225 28 72] 2] [[231 74 108] 2] [[239 137 160] 2] [[226 41 82] 2]
   [[254 247 249] 2] [[225 30 74] 2] [[248 203 213] 2] [[255 253 254] 2]
   [[241 148 168] 2] [[236 115 142] 2] [[225 28 73] 2] [[226 35 77] 2]
   [[227 45 85] 2] [[246 184 198] 2] [[244 174 189] 2] [[234 96 126] 2]
   [[240 143 164] 2] [[254 246 247] 2] [[231 78 112] 2] [[232 84 117] 2]
   [[227 43 83] 2] [[228 52 90] 2] [[226 36 77] 2] [[244 173 189] 2]
   [[230 66 102] 2] [[224 29 73] 2] [[237 122 147] 2] [[237 116 142] 2]
   [[250 219 226] 2]])

(def ^:private measured-samples
  (vec (mapcat (fn [[c n]] (repeat n c)) measured-histogram)))

(deftest quantiser-keeps-a-real-colour-over-antialiasing-blends
  (testing "3 色なら赤・黄・白。混色は 1 つも代表色にならない"
    (let [p (mapv raster/rgb->hex (raster/median-cut measured-samples 3))]
      (is (= #{"#e11d48" "#facc15" "#ffffff"} (set p))
          (str "実際に出たパレット: " (pr-str p)))))
  (testing "黄は全体の 26% を占める実在の色 —— 消えてはいけない"
    (doseq [k [3 4 5 6]]
      (let [p (set (mapv raster/rgb->hex (raster/median-cut measured-samples k)))]
        (is (contains? p "#facc15") (str "k=" k " で黄が消えた: " (pr-str p)))))))

(deftest palette-never-repeats-a-colour
  (testing "図案の色数より多い k を指定しても、同じ色の版を2枚作らない"
    (let [p (raster/median-cut measured-samples 12)]
      (is (= (count p) (count (distinct p)))))))

(deftest quantiser-is-deterministic
  (is (= (raster/median-cut measured-samples 4)
         (raster/median-cut measured-samples 4))))

;; ---------------------------------------------------------------- 簡略化

(deftest douglas-peucker-keeps-the-shape-and-drops-collinear-points
  (let [square (vec (concat (map (fn [i] [i 0]) (range 0 21))
                            (map (fn [i] [20 i]) (range 1 21))
                            (map (fn [i] [(- 20 i) 20]) (range 1 21))
                            (map (fn [i] [0 (- 20 i)]) (range 1 20))))
        simple (raster/douglas-peucker square 0.5)]
    (is (< (count simple) 10) (str "頂点が減っていない: " (count simple)))
    (is (< (Math/abs (- 400.0 (geom/area {:points simple :closed? true}))) 4.0))))

;; ---------------------------------------------------------------- 通し

(deftest plan-image-produces-a-white-plate
  (let [job (shirohan/plan-image donut {:colors 2 :print-width-mm 100 :choke-mm 0.2
                                                :separate-colors? true})]
    (is (some :underbase? (:plates job)))
    (testing "白版を出すとき、白のスポット版は作らない（白版の上に白を刷らない）"
      (is (not (some #(= "#ffffff" (:color %)) (remove :underbase? (:plates job))))))
    (testing "版は刷り幅ちょうどに収まる"
      (is (< (Math/abs (- 100.0 (- (:width-mm (:size job)) (* 2 (:margin-mm (:spec job))))))
             1e-6)))))

;; ---------------------------------------------------------------- 白版のシルエット
;;
;; 実務家の指摘（2026-08-01）に対する回帰。赤い円の中の白い円は「赤に空いた穴」
;; だが、**そこには白インクが載る**ので白版では穴ではない。色版の和で白版を作ると
;; ここが抜ける（実測: 本番のブラウザで抜けていた）。

(def ^:private white-inside-red
  "赤い正方形の中に白い正方形。地は透過。"
  (image 24 24
         (fn [x y]
           (let [in? (and (>= x 3) (< x 21) (>= y 3) (< y 21))
                 white? (and (>= x 9) (< x 15) (>= y 9) (< y 15))]
             (cond white? [255 255 255 255]
                   in? [225 29 72 255]
                   :else [0 0 0 0])))))

(deftest the-silhouette-is-the-ink-face-not-the-colour-union
  (let [{:keys [silhouette contours]} (raster/trace white-inside-red
                                                    {:colors 2 :min-area-px 4})]
    (testing "シルエットは地との境目だけを見る —— 外周 1 本"
      (is (= 1 (count silhouette)))
      (is (near? (* 18.0 18.0) (geom/area (first silhouette)) 1.0)))
    (testing "色ごとの輪郭は色の切れ目を見るので、赤は外周+穴の 2 本になる"
      (is (= 2 (count (filter #(= "#e11d48" (:fill %)) contours)))))
    (testing "色ごとの輪郭には色ごとの :shape が付く（穴判定を色の中に閉じる）"
      (is (every? :shape contours)))))

(deftest the-white-plate-does-not-hole-out-the-white-artwork
  (testing "白版は白インクを塗る部分の指示 —— 白い部分も塗る"
    (let [job (shirohan/plan-image white-inside-red
                                   {:colors 2 :print-width-mm 100 :choke-mm 0.1
                                    :separate-colors? true})
          white (first (filter :underbase? (:plates job)))]
      (is (= 1 (count (:art white))) "白版はベタの外周 1 本")
      (is (empty? (:knockout white)))
      (testing "面積は図案全体（穴が空いていない）"
        (is (near? (* 100.0 100.0) (geom/area (first (:art white))) 60.0))))))

;; ---------------------------------------------------------------- 白背景の画像
;;
;; 現場でいちばん多い入稿形は「白背景の PNG」。α が全部 255 なので、α だけを見ると
;; 画像の四角全体がインクになり、**白版が長方形になる**（実測 2026-08-01、本番で確認）。
;; 区別できる唯一の手掛かりは「画像の外と繋がっているか」。

(def ^:private donut-on-white
  "白背景・黒ドーナツ・白い穴。**透明は 1 画素も無い。**"
  (image 64 64
         (fn [x y]
           (let [dx (- x 31.5) dy (- y 31.5)
                 d (Math/sqrt (+ (* dx dx) (* dy dy)))]
             (if (and (<= d 26) (>= d 11)) [0 0 0 255] [255 255 255 255])))))

(deftest a-white-background-is-not-ink
  (testing "縁から繋がった白は地。白版が画像全体にならない"
    (let [job (shirohan/plan-image donut-on-white
                                   {:colors 2 :print-width-mm 100 :choke-mm 0.1
                                    :separate-colors? true})
          white (first (filter :underbase? (:plates job)))
          outer (apply max (map geom/area (:art white)))]
      (is (< outer (* 100.0 100.0))
          "白版が画像の四角全体になっている（地を拾えていない）")
      (is (near? (* Math/PI 50.0 50.0) outer 400.0)
          "白版の外周は図案（ドーナツの外径）"))))

(deftest the-assumption-about-an-opaque-background-is-reported
  (let [{:keys [findings]} (raster/trace donut-on-white {:colors 2})
        kinds (set (map :kind findings))]
    (is (contains? kinds :opaque-background-assumed)
        "何を地と見なしたかを黙って決めない")
    (testing "囲まれた同色領域（穴かもしれないし白い図柄かもしれない）は報告する"
      (is (contains? kinds :enclosed-background-region)))))

(deftest a-transparent-png-needs-no-guessing
  (testing "透明があるならそちらが正 —— 地の推測はしない"
    (let [{:keys [findings]} (raster/trace donut {:colors 2})]
      (is (not (contains? (set (map :kind findings)) :opaque-background-assumed))))))

;; ---------------------------------------------------------------- 斜めの接点
;;
;; 4 方向の辺は 1 画素の中では始点が全部違うが、**斜めに接する 2 画素**では
;; 別々の画素が出した辺が同じ格子点から出る。map で持つと片方が黙って消え、
;; 鎖が繋ぎ違えて図形の中に切れ込みが入る（実測 2026-08-01、キャラのシルエットの
;; 頭部に白い楔が出た）。

(def ^:private diagonal-touch
  "対角にだけ接する 2 つの塊。格子点 (6,6) から 2 本の辺が出る。

  塊を 6×6 にしてあるのは、**DP の許容差（1 画素）より十分大きい**必要が
  あるため —— 2×2 だと DP が三角形に潰し、鎖の健全性ではなく DP の挙動を
  試すことになる。"
  (image 20 20
         (fn [x y]
           (if (or (and (< x 6) (< y 6))
                   (and (>= x 6) (>= y 6) (< x 12) (< y 12)))
             [0 0 0 255] [0 0 0 0]))))

(deftest a-diagonal-pinch-does-not-break-the-chain
  (let [{:keys [silhouette]} (raster/trace diagonal-touch {:colors 2 :min-area-px 1})
        total (reduce + 0.0 (map geom/area silhouette))]
    (testing "2 つの塊の面積が両方とも出る（辺が落ちていない）"
      ;; 許容差 4 は接点そのもののぶん —— 2 塊が 1 点で触れているので、
      ;; 追跡はその格子点を通り、わずかな楔が両側に足される。
      ;; **肝心なのは 36（片方だけ）にならないこと。**
      (is (near? (+ (* 6.0 6.0) (* 6.0 6.0)) total 4.0)
          (str "面積 " total " —— 辺が落ちると塊が欠ける")))
    (testing "向きはすべて外周（切れ込みが穴として現れない）"
      (is (= 1 (count (distinct (map geom/orientation silhouette))))))))

;; ---------------------------------------------------------------- 曲線出力
;;
;; 折れ線のままだと拡大時に必ず角張る。ラスタ由来の輪郭はベジェに当てはめて出す。

(deftest raster-contours-are-emitted-as-curves
  (let [{:keys [silhouette]} (raster/trace donut {:colors 2 :min-area-px 4})
        d (geom/contour->d (first silhouette))]
    (is (seq (:corners (first silhouette)))
        "角の添字を持っている（焼いた path 文字列ではない —— 拡縮で古くなるため）")
    (testing "path は C（3次ベジェ）で出る —— L だけの折れ線ではない"
      (is (re-find #"C" d))
      (is (not (re-find #"L" d))))))

(deftest curve-output-survives-scaling
  (testing "拡縮しても path は点と一致する —— 焼いた文字列を持たないから"
    (let [c (first (:silhouette (raster/trace donut {:colors 2 :min-area-px 4})))
          scaled (update c :points #(mapv (fn [[x y]] [(* 10.0 x) (* 10.0 y)]) %))
          d (geom/contour->d scaled)
          [x0 y0] (first (:points scaled))]
      (is (re-find #"^M" d))
      (is (str/starts-with? d (str "M" (geom/fmt x0) " " (geom/fmt y0)))
          "拡縮後の座標で出ている"))))

(deftest curve-fitting-passes-through-the-points
  (testing "点を動かさないので面積が変わらない —— 「なめらかにする」で痩せない"
    (let [pts (mapv (fn [i] (let [a (* 2 Math/PI (/ i 24.0))]
                              [(* 20 (Math/cos a)) (* 20 (Math/sin a))]))
                    (range 24))
          f (curve/fit pts)]
      (is (= pts (:points f)) "頂点はそのまま"))))

(deftest a-real-corner-stays-sharp
  (testing "角では接線を 0 にするので、その区間は直線のまま"
    (let [square [[0 0] [40 0] [40 40] [0 40]]
          cs (curve/corners square {})]
      (is (= 4 (count cs)) "正方形の 4 隅は角"))
    (testing "階段の 1 画素の段差は角にしない（前後の辺が短い）"
      (let [stair [[0 0] [1 0] [1 1] [2 1] [2 2] [20 2] [20 20] [0 20]]
            cs (curve/corners stair {})]
        (is (not (cs 1)) "1 画素の段差は角ではない")
        (is (not (cs 2)))))))

(defn- hv-stair
  "原点から n 段の 1px 階段で (n,n) へ。"
  [n]
  (vec (cons [0.0 0.0]
             (mapcat (fn [i] [[(double (inc i)) (double i)]
                              [(double (inc i)) (double (inc i))]])
                     (range n)))))

(deftest pixel-stairs-collapse-to-a-diagonal
  (testing "短い軸平行の段は対角線に畳む。正方形の頂点は動かさない"
    (let [stair (hv-stair 8)
          out (curve/collapse-stairs stair)]
      (is (< (count out) (count stair))
          (str "段が残っている: " (count stair) " → " (count out)))
      (is (>= (count out) 2)))
    (let [square [[0.0 0.0] [40.0 0.0] [40.0 40.0] [0.0 40.0]]]
      (is (= square (curve/collapse-stairs square))
          "正方形は 4 点のまま")
      (is (= square (:points (curve/fit square)))
          "fit しても正方形の点は動かない"))))

(deftest collapsed-stairs-are-not-marked-as-corners
  (testing "畳んだあとの斜め輪郭に 90° の偽角が残らない"
    (let [closed (into (hv-stair 10) [[10.0 24.0] [0.0 24.0]])
          f (curve/fit closed)]
      (is (< (count (:corners f)) 6)
          (str "角が " (count (:corners f)) " 個 — 段の 90° を尖らせている"))
      (is (< (count (:points f)) (count closed))))))

;; ---------------------------------------------------------------- 色版は任意
;;
;; 成果物は白版で、色版は版ずれの確認用。色の量子化と色ごとの輪郭追跡は
;; この経路でいちばん重いので、要らない人に払わせない。

(deftest colour-separation-is-opt-in
  (testing "既定では白版だけ"
    (let [job (shirohan/plan-image donut {:print-width-mm 100})]
      (is (= 1 (count (:plates job))))
      (is (:underbase? (first (:plates job))))
      (is (empty? (:palette job)))))
  (testing "明示すれば色版も出る"
    (let [job (shirohan/plan-image donut {:print-width-mm 100 :colors 2
                                          :separate-colors? true})]
      (is (> (count (:plates job)) 1))
      (is (seq (:palette job))))))

(deftest the-white-plate-is-the-same-either-way
  (testing "色版を作るかどうかで白版が変わってはいけない"
    (let [a (shirohan/plan-image donut {:print-width-mm 100 :choke-mm 0.1})
          b (shirohan/plan-image donut {:print-width-mm 100 :choke-mm 0.1
                                        :colors 2 :separate-colors? true})
          area (fn [j] (:area-mm2 (first (filter :underbase? (:plates j)))))]
      (is (near? (area a) (area b) 0.5)))))

;; ---------------------------------------------------------------- 偽の角
;;
;; 角と判定した頂点は接線を 0 にする＝**意図的に尖らせる**ので、誤判定はそのまま
;; ギザギザになる。DP は階段を完全には潰さず 2〜3 画素のジグザグを残すため、
;; 閾値が甘いとそれが全部「角」になり、曲線に当てはめたはずの輪郭が
;; 折れ線に戻る（実測 2026-08-01、これが「まだギザギザ」の正体）。

(defn- mask-image [w h f]
  {:width w :height h
   :data (vec (mapcat (fn [i] [0 0 0 (if (f (mod i w) (quot i w)) 255 0)])
                      (range (* w h))))})

(defn- traced-points [im]
  (:points (first (:contours (raster/trace-silhouette im {})))))

(deftest a-smooth-shape-gets-no-corners
  (testing "なめらかな楕円に角は 1 つも無い —— あればそこが尖ってギザになる"
    (let [im (mask-image 256 256
                         (fn [x y] (let [dx (- x 128.0) dy (- y 128.0)]
                                     (< (+ (/ (* dx dx) (* 100.0 100.0))
                                           (/ (* dy dy) (* 80.0 80.0))) 1.0))))
          c (first (:contours (raster/trace-silhouette im {})))
          pts (:points c)
          d (geom/contour->d c)]
      (is (empty? (curve/corners pts {}))
          (str "偽の角が " (count (curve/corners pts {})) " 個"))
      (is (contains? c :corners)
          "角が 0 でも :corners キーは持つ —— 無いと折れ線に落ちる")
      (is (empty? (:corners c)))
      (testing "角 0 のシルエットも C で出す。空集合を seq で折ると人物の輪郭が階段の L になる"
        (is (re-find #"C" d))
        (is (not (re-find #"L" d))))
      (testing "生の 1px 段を DP の前に畳むので、楕円が画素階段のまま残らない"
        (is (< (count pts) 80)
            (str "点が " (count pts) " 個 — DP だけの階段近似が残っている"))))))

(deftest real-corners-still-survive
  (testing "正方形の 4 隅"
    (let [pts (traced-points (mask-image 256 256
                                         (fn [x y] (and (> x 40) (< x 216)
                                                        (> y 40) (< y 216)))))]
      (is (= 4 (count (curve/corners pts {}))))))
  ;; 尖った角は辺自体が短いので、「前後の辺が長いこと」を条件にすると星だけが
  ;; 落ちる。弧長で測った弦なら残る。
  (testing "星の 5 つの尖り —— **解像度を変えても 5 のまま**"
    (doseq [side [256 512]]
      (let [k (/ side 256.0)
            pts (traced-points
                 (mask-image side side
                             (fn [x y] (let [dx (- x (* k 128.0)) dy (- y (* k 128.0))
                                             a (Math/atan2 dy dx)
                                             d (Math/sqrt (+ (* dx dx) (* dy dy)))]
                                         (< d (* k (+ 70 (* 40 (Math/cos (* 5 a))))))))))]
        (is (= 5 (count (curve/corners pts {})))
            (str side "px で " (count (curve/corners pts {})) " 個"))))))
