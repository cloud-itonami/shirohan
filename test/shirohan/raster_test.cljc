(ns shirohan.raster-test
  (:require [clojure.test :refer [deftest is testing]]
            [shirohan.geom :as geom]
            [shirohan.raster :as raster]
            [shirohan.core :as shirohan]))

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
  (let [job (shirohan/plan-image donut {:colors 2 :print-width-mm 100 :choke-mm 0.2})]
    (is (some :underbase? (:plates job)))
    (testing "白は既定で白抜きになるので、白のスポット版は作られない"
      (is (not (some #(= "#ffffff" (:color %)) (remove :underbase? (:plates job))))))
    (testing "版は刷り幅ちょうどに収まる"
      (is (< (Math/abs (- 100.0 (- (:width-mm (:size job)) (* 2 (:margin-mm (:spec job))))))
             1e-6)))))
