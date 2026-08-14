(ns shirohan.geom-test
  (:require [clojure.test :refer [deftest is testing]]
            [shirohan.geom :as geom]))

(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(defn- near? [a b] (< (abs* (- a b)) 1e-6))

(defn- sq
  "一辺 n の正方形。`rev?` で向きを反転する。"
  ([n] (sq n false))
  ([n rev?]
   (let [p [[0 0] [n 0] [n n] [0 n]]]
     {:points (if rev? (vec (reverse p)) p) :closed? true})))

(deftest signed-area-and-orientation
  (is (= 100.0 (geom/area (sq 10))))
  (is (= 100.0 (geom/area (sq 10 true))))
  (testing "向きが逆なら符号も逆"
    (is (= (- (geom/signed-area (sq 10))) (geom/signed-area (sq 10 true))))
    (is (not= (geom/orientation (sq 10)) (geom/orientation (sq 10 true))))))

(deftest bbox-covers-all-points
  (let [b (geom/bbox [(sq 10) {:points [[-5 3] [-4 3] [-4 4]] :closed? true}])]
    (is (= {:x0 -5 :y0 0 :x1 10 :y1 10} (select-keys b [:x0 :y0 :x1 :y1])))))

(deftest offset-shrinks-and-grows-regardless-of-winding
  (testing "δ<0 は囲む面を縮める —— 10 角の正方形を 1mm 縮めれば 8x8"
    (doseq [rev? [false true]]
      (let [o (geom/offset (sq 10 rev?) -1.0)]
        (is (near? 64.0 (geom/area o))
            (str "reversed=" rev? " area=" (geom/area o))))))
  (testing "δ>0 は広げる —— 12x12"
    (doseq [rev? [false true]]
      (let [o (geom/offset (sq 10 rev?) 1.0)]
        (is (near? 144.0 (geom/area o)))))))

(deftest offset-is-symmetric-around-zero
  (let [a (geom/area (geom/offset (sq 10) -2.0))
        b (geom/area (geom/offset (sq 10) 2.0))]
    (is (near? 36.0 a))
    (is (near? 196.0 b))))

(deftest miter-limit-caps-spikes
  (testing "非常に鋭い角でも頂点が limit*|δ| より遠くへ飛ばない"
    (let [spike {:points [[0 0] [100 0.5] [0 1]] :closed? true}
          o (geom/offset spike 1.0 {:miter-limit 2.0})
          far (apply max (map (fn [[x _]] x) (:points o)))]
      (is (< far (+ 100 (* 2.0 1.0) 1e-6))))))

(deftest min-feature-width-finds-the-narrow-part
  (testing "正方形の最小特徴幅は一辺"
    (is (near? 10.0 (geom/min-feature-width (sq 10)))))
  (testing "細長い帯は幅の方を返す"
    (let [strip {:points [[0 0] [50 0] [50 0.4] [0 0.4]] :closed? true}]
      (is (near? 0.4 (geom/min-feature-width strip))))))

(deftest inside-uses-ray-casting
  (is (geom/inside? (sq 10) [5 5]))
  (is (not (geom/inside? (sq 10) [15 5])))
  (is (not (geom/inside? (sq 10) [5 -1]))))

(deftest normalize-drops-degenerate-contours
  (is (nil? (geom/normalize-contour {:points [[0 0] [1 1]] :closed? true})))
  (testing "重複点を落とす（長さ 0 の辺があると法線が定義できない）"
    (let [c (geom/normalize-contour {:points [[0 0] [0 0] [10 0] [10 10] [0 10] [0 0]]
                                     :closed? true})]
      (is (= 4 (count (:points c)))))))

(deftest fmt-is-stable
  (is (= "1" (geom/fmt 1.0)))
  (is (= "1.235" (geom/fmt 1.23456)))
  (is (= "-0.5" (geom/fmt -0.5))))

(deftest contour->d-closes-only-closed-contours
  (is (= "M0 0L10 0L10 10L0 10Z" (geom/contour->d (sq 10))))
  (is (= "M0 0L1 1" (geom/contour->d {:points [[0 0] [1 1]] :closed? false}))))

(deftest a-raster-contour-with-no-corners-is-still-a-curve
  (testing "`:corners #{}` は「尖らせない」であって「折れ線に戻す」ではない"
    (let [circle (mapv (fn [i]
                         (let [a (* 2.0 Math/PI (/ i 12.0))]
                           [(* 10.0 (Math/cos a)) (* 10.0 (Math/sin a))]))
                       (range 12))
          d (geom/contour->d {:points circle :closed? true :corners #{}})]
      (is (re-find #"C" d) "ベジェで出る")
      (is (not (re-find #"L" d)) "L の折れ線に落ちない")))
  (testing "`:corners` キーが無い輪郭（SVG 由来）は従来どおり折れ線"
    (is (re-find #"L" (geom/contour->d (sq 10))))
    (is (not (re-find #"C" (geom/contour->d (sq 10)))))))
