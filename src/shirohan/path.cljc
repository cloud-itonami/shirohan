(ns shirohan.path
  "SVG の `path` データ（`d` 属性）を折れ線の輪郭に落とす。

  版下は最終的に**折れ線**で扱う —— choke（`shirohan.geom/offset`）も細り検査も
  面積計算も、曲線のままでは閉じた式にならない。曲線を残したまま「だいたいの
  オフセット」を出すより、**先に決められた許容誤差で折る**方が、どれだけずれるかが
  1 個の数（`:tolerance-mm`）で言い切れる。

  ## 対応するコマンド

  `M m L l H h V v C c S s Q q T t A a Z z` の全部。相対・絶対、暗黙の繰り返し
  （`L 1 2 3 4` が 2 回の L になる）、`M` の後の暗黙 `L`、`10-5` のような
  区切り無しの負数も読む。

  ## 折り方

  Bézier は制御点多角形の長さから分割数を決めて等間隔にサンプルする（適応的
  再分割ではない）。`:tolerance-mm` を弦の許容誤差とみなし、分割数は
  `ceil(制御点多角形長 / tolerance)` を 2〜256 に丸める。円弧は W3C の
  endpoint→center 変換をして角度で刻む。

  誤差は**必ず内側に入る**わけではない（弦は曲線の内側を通るので、凸部では
  面が痩せる）。既定 0.05mm は choke の下限 0.1mm の半分で、版ずれ許容
  ±0.5mm に対しては十分に小さい。"
  (:require [clojure.string :as str]))

(defn- sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))
(defn- sin [x] #?(:clj (Math/sin (double x)) :cljs (js/Math.sin x)))
(defn- cos [x] #?(:clj (Math/cos (double x)) :cljs (js/Math.cos x)))
(defn- atan2 [y x] #?(:clj (Math/atan2 (double y) (double x)) :cljs (js/Math.atan2 y x)))
(defn- acos [x] #?(:clj (Math/acos (double x)) :cljs (js/Math.acos x)))
(defn- ceil [x] #?(:clj (long (Math/ceil (double x))) :cljs (js/Math.ceil x)))
(defn- abs* [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))

(defn- parse-num [s]
  #?(:clj (Double/parseDouble s) :cljs (js/parseFloat s)))

(def ^:private token-re
  ;; コマンド 1 文字 か 数（指数・先頭ドット・符号つき）。`,` と空白は捨てる。
  #"[MmLlHhVvCcSsQqTtAaZz]|[-+]?(?:\d*\.\d+|\d+\.?)(?:[eE][-+]?\d+)?")

(defn tokenize [d]
  (vec (re-seq token-re (or d ""))))

(defn- cmd? [t] (and (= 1 (count t)) (re-matches #"[MmLlHhVvCcSsQqTtAaZz]" t)))

;; ---------------------------------------------------------------- 曲線を折る

(defn- segments-for [len tol]
  (let [n (ceil (/ (max len 1e-9) (max tol 1e-6)))]
    (cond (< n 2) 2 (> n 256) 256 :else n)))

(defn- dist [ax ay bx by] (sqrt (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay)))))

(defn- cubic [x0 y0 x1 y1 x2 y2 x3 y3 tol]
  (let [len (+ (dist x0 y0 x1 y1) (dist x1 y1 x2 y2) (dist x2 y2 x3 y3))
        n (segments-for len tol)]
    (mapv (fn [i]
            (let [t (/ (double i) n) u (- 1.0 t)
                  a (* u u u) b (* 3 u u t) c (* 3 u t t) e (* t t t)]
              [(+ (* a x0) (* b x1) (* c x2) (* e x3))
               (+ (* a y0) (* b y1) (* c y2) (* e y3))]))
          (range 1 (inc n)))))

(defn- quad [x0 y0 x1 y1 x2 y2 tol]
  (let [len (+ (dist x0 y0 x1 y1) (dist x1 y1 x2 y2))
        n (segments-for len tol)]
    (mapv (fn [i]
            (let [t (/ (double i) n) u (- 1.0 t)
                  a (* u u) b (* 2 u t) c (* t t)]
              [(+ (* a x0) (* b x1) (* c x2))
               (+ (* a y0) (* b y1) (* c y2))]))
          (range 1 (inc n)))))

(defn- arc
  "endpoint 記法の楕円弧を折れ線にする（W3C SVG 1.1 Appendix F.6.5）。"
  [x0 y0 rx0 ry0 phi-deg large? sweep? x1 y1 tol]
  (if (and (< (abs* (- x1 x0)) 1e-12) (< (abs* (- y1 y0)) 1e-12))
    []
    (let [rx1 (abs* rx0) ry1 (abs* ry0)]
      (if (or (< rx1 1e-12) (< ry1 1e-12))
        [[x1 y1]]                                   ; 半径 0 は直線（仕様どおり）
        (let [phi (/ (* phi-deg pi) 180.0)
              cp (cos phi) sp (sin phi)
              dx2 (/ (- x0 x1) 2.0) dy2 (/ (- y0 y1) 2.0)
              x1p (+ (* cp dx2) (* sp dy2))
              y1p (- (* cp dy2) (* sp dx2))
              ;; 半径が足りなければ仕様どおり拡大する
              lam (+ (/ (* x1p x1p) (* rx1 rx1)) (/ (* y1p y1p) (* ry1 ry1)))
              s (if (> lam 1.0) (sqrt lam) 1.0)
              rx (* rx1 s) ry (* ry1 s)
              num (- (* rx rx ry ry) (* rx rx y1p y1p) (* ry ry x1p x1p))
              den (+ (* rx rx y1p y1p) (* ry ry x1p x1p))
              co (* (if (= large? sweep?) -1.0 1.0)
                    (sqrt (max 0.0 (/ (max 0.0 num) (max den 1e-12)))))
              cxp (* co (/ (* rx y1p) ry))
              cyp (* co (- (/ (* ry x1p) rx)))
              cx (+ (- (* cp cxp) (* sp cyp)) (/ (+ x0 x1) 2.0))
              cy (+ (+ (* sp cxp) (* cp cyp)) (/ (+ y0 y1) 2.0))
              ang (fn [ux uy vx vy]
                    (let [d (+ (* ux vx) (* uy vy))
                          n1 (sqrt (+ (* ux ux) (* uy uy)))
                          n2 (sqrt (+ (* vx vx) (* vy vy)))
                          c (/ d (max (* n1 n2) 1e-12))
                          c (cond (> c 1.0) 1.0 (< c -1.0) -1.0 :else c)
                          a (acos c)]
                      (if (neg? (- (* ux vy) (* uy vx))) (- a) a)))
              ux (/ (- x1p cxp) rx) uy (/ (- y1p cyp) ry)
              vx (/ (- (- x1p) cxp) rx) vy (/ (- (- y1p) cyp) ry)
              th1 (ang 1.0 0.0 ux uy)
              dth0 (ang ux uy vx vy)
              dth (cond (and (not sweep?) (pos? dth0)) (- dth0 (* 2 pi))
                        (and sweep? (neg? dth0)) (+ dth0 (* 2 pi))
                        :else dth0)
              rmax (max rx ry)
              n (segments-for (* (abs* dth) rmax) tol)]
          (mapv (fn [i]
                  (let [t (+ th1 (* dth (/ (double i) n)))
                        ct (cos t) st (sin t)]
                    [(+ cx (- (* cp rx ct) (* sp ry st)))
                     (+ cy (+ (* sp rx ct) (* cp ry st)))]))
                (range 1 (inc n))))))))

;; ---------------------------------------------------------------- 実行

(defn parse
  "`d` → 輪郭の列 `[{:points [[x y]…] :closed? bool} …]`。

  `Z` が来た subpath だけ `:closed? true`。閉じていない subpath も落とさずに
  返す —— 面としては使えないが、`shirohan.plate` が `:open-contour` として
  報告できるように残す（黙って消すと図案の一部が版から消えた理由が分からない）。"
  ([d] (parse d {}))
  ([d {:keys [tolerance-mm] :or {tolerance-mm 0.05}}]
   (let [ts (tokenize d)
         tol tolerance-mm]
     (loop [i 0
            cmd nil
            cur []            ; いま組んでいる subpath の点
            start nil         ; subpath の始点（Z の戻り先）
            pos [0.0 0.0]
            prev-c nil        ; 直前の 3 次制御点（S 用）
            prev-q nil        ; 直前の 2 次制御点（T 用）
            out []]
       (if (>= i (count ts))
         (let [out (if (>= (count cur) 2) (conj out {:points cur :closed? false}) out)]
           out)
         (let [t (nth ts i)]
           (if (cmd? t)
             (if (or (= t "Z") (= t "z"))
               ;; Z の後は始点に戻る。M を挟まずに次の描画が来たら、その subpath は
               ;; 同じ始点から始まる（SVG 仕様）ので `cur` に始点を残しておく。
               (recur (inc i) cmd (if start [start] [])
                      start (or start pos) nil nil
                      (if (>= (count cur) 2)
                        (conj out {:points cur :closed? true})
                        out))
               (recur (inc i) t cur start pos prev-c prev-q out))
             ;; 数トークン。直前のコマンドを暗黙に繰り返す。
             (let [n (fn [k] (parse-num (nth ts (+ i k))))
                   [px py] pos
                   rel? (contains? #{"m" "l" "h" "v" "c" "s" "q" "t" "a"} cmd)
                   ax (fn [v] (if rel? (+ px v) v))
                   ay (fn [v] (if rel? (+ py v) v))]
               (case (str/upper-case (or cmd "L"))
                 "M" (let [x (ax (n 0)) y (ay (n 1))]
                       (recur (+ i 2)
                              ;; M の後に続く数は暗黙の L（m なら l）
                              (if rel? "l" "L")
                              [[x y]] [x y] [x y] nil nil
                              (if (>= (count cur) 2)
                                (conj out {:points cur :closed? false})
                                out)))
                 "L" (let [x (ax (n 0)) y (ay (n 1))]
                       (recur (+ i 2) cmd (conj cur [x y]) start [x y] nil nil out))
                 "H" (let [x (ax (n 0))]
                       (recur (inc i) cmd (conj cur [x py]) start [x py] nil nil out))
                 "V" (let [y (ay (n 0))]
                       (recur (inc i) cmd (conj cur [px y]) start [px y] nil nil out))
                 "C" (let [c1x (ax (n 0)) c1y (ay (n 1))
                           c2x (ax (n 2)) c2y (ay (n 3))
                           x (ax (n 4)) y (ay (n 5))
                           pts (cubic px py c1x c1y c2x c2y x y tol)]
                       (recur (+ i 6) cmd (into cur pts) start [x y] [c2x c2y] nil out))
                 "S" (let [[rx ry] (or prev-c [px py])
                           c1x (- (* 2 px) rx) c1y (- (* 2 py) ry)
                           c2x (ax (n 0)) c2y (ay (n 1))
                           x (ax (n 2)) y (ay (n 3))
                           pts (cubic px py c1x c1y c2x c2y x y tol)]
                       (recur (+ i 4) cmd (into cur pts) start [x y] [c2x c2y] nil out))
                 "Q" (let [c1x (ax (n 0)) c1y (ay (n 1))
                           x (ax (n 2)) y (ay (n 3))
                           pts (quad px py c1x c1y x y tol)]
                       (recur (+ i 4) cmd (into cur pts) start [x y] nil [c1x c1y] out))
                 "T" (let [[rx ry] (or prev-q [px py])
                           c1x (- (* 2 px) rx) c1y (- (* 2 py) ry)
                           x (ax (n 0)) y (ay (n 1))
                           pts (quad px py c1x c1y x y tol)]
                       (recur (+ i 2) cmd (into cur pts) start [x y] nil [c1x c1y] out))
                 "A" (let [rx (n 0) ry (n 1) rot (n 2)
                           large? (not (zero? (n 3))) sweep? (not (zero? (n 4)))
                           x (ax (n 5)) y (ay (n 6))
                           pts (arc px py rx ry rot large? sweep? x y tol)]
                       (recur (+ i 7) cmd (into cur pts) start [x y] nil nil out))
                 ;; 未知のコマンドは 1 数だけ捨てて進む（止まらない）
                 (recur (inc i) cmd cur start pos prev-c prev-q out))))))))))
