(ns shirohan.curve
  "階段状の折れ線を**曲線**に当てはめる。ラスタから起こした輪郭の仕上げ。

  ## なぜ折れ線のままでは駄目か

  クラック追跡が返すのは画素の辺そのものなので、輪郭は必ず軸に平行な階段になる。
  Douglas–Peucker は点を減らすだけで、出てくるのは相変わらず `L` だけの折れ線
  —— **拡大すれば必ず角張って見える**し、カッティングプロッタに渡せば節ごとに
  減速して切り口に段が残る。版下は原寸で使うものなので、これは実害がある。

  ## 手順

  ```
  階段の折れ線 ─▶ ①角を見つける ─▶ ②角以外を曲線でつなぐ
  ```

  **点は動かさない。** Catmull-Rom は**通過する補間**なので、輪郭は元の位置から
  1 ミクロンも離れない —— 「なめらかにする」を「頂点をならす」で実装すると必ず
  面積が痩せる。実測（2026-08-01）:

  | やり方 | ドーナツの穴 r=34.3mm が |
  |---|---|
  | Chaikin の角切り 2 回 | 実在する角まで丸まり、18×18 の正方形が 324 → 317 |
  | 頂点の移動平均 2 回 | **30.4mm まで縮む**（離散的な曲線短縮流そのもの） |
  | Catmull-Rom で補間（採用） | **34.3mm のまま**。点を通るので動かない |

  **角では接線を 0 にする** ので制御点が頂点に重なり、その区間だけ直線になる ——
  角が丸まらないのはこの 1 行のため。

  ならし（`smooth`）は残してあるが**既定 0 回**。意図的に丸めたいときだけ使う。"
  (:require [shirohan.geom :as geom]))

(defn- sqrt [x] #?(:clj (Math/sqrt (double x)) :cljs (js/Math.sqrt x)))
(defn- acos [x] #?(:clj (Math/acos (double x)) :cljs (js/Math.acos x)))
(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))

(defn- turn-angle
  "頂点 i での曲がり角[度]。0 が直進、180 が折り返し。"
  [pts i]
  (let [n (count pts)
        [px py] (nth pts (mod (+ (dec i) n) n))
        [cx cy] (nth pts i)
        [nx ny] (nth pts (mod (inc i) n))
        ax (- cx px) ay (- cy py)
        bx (- nx cx) by (- ny cy)
        la (sqrt (+ (* ax ax) (* ay ay)))
        lb (sqrt (+ (* bx bx) (* by by)))]
    (if (or (< la 1e-9) (< lb 1e-9))
      0.0
      (let [c (/ (+ (* ax bx) (* ay by)) (* la lb))
            c (cond (> c 1.0) 1.0 (< c -1.0) -1.0 :else c)]
        (/ (* (acos c) 180.0) pi)))))

(defn corners
  "図案の角とみなす頂点の添字。

  **階段の段差と本物の角を、曲がり角の大きさだけでは区別できない**（どちらも
  90 度）。区別できるのは**辺の長さ**で、段差は 1 画素、本物の角は前後に
  ずっと長い辺を持つ。だから「曲がり角が閾値以上 **かつ** 前後の辺が
  `min-edge` 以上」を角とする。"
  [pts {:keys [corner-deg min-edge] :or {corner-deg 50.0 min-edge 2.0}}]
  (let [n (count pts)
        len (fn [i j] (let [[x1 y1] (nth pts i) [x2 y2] (nth pts j)]
                        (sqrt (+ (* (- x2 x1) (- x2 x1))
                                 (* (- y2 y1) (- y2 y1))))))]
    (into #{}
          (filter (fn [i]
                    (and (>= (turn-angle pts i) corner-deg)
                         (>= (len (mod (+ (dec i) n) n) i) min-edge)
                         (>= (len i (mod (inc i) n)) min-edge))))
          (range n))))

(defn- average-once
  "角以外の頂点を隣と平均して 1 段ならす。角は動かさない。"
  [pts corner-set]
  (let [n (count pts)]
    (vec (map-indexed
          (fn [i p]
            (if (corner-set i)
              p
              ;; (前 + 2×自分 + 次) / 4 —— 動く量は階段の振れ幅の半分以下。
              (let [[px py] (nth pts (mod (+ (dec i) n) n))
                    [cx cy] p
                    [nx ny] (nth pts (mod (inc i) n))]
                [(/ (+ px cx cx nx) 4.0)
                 (/ (+ py cy cy ny) 4.0)])))
          pts))))

(defn smooth
  "角を保ったまま `n` 段ならす。"
  [pts corner-set n]
  (if (or (zero? n) (< (count pts) 4))
    pts
    (recur (average-once pts corner-set) corner-set (dec n))))

(defn fit
  "折れ線 → `{:points [...] :corners #{…} }`。

  opts:
  - `:corner-deg` 角とみなす曲がり角の下限（既定 50）
  - `:min-edge`   角とみなすのに必要な前後の辺長（既定 2.0。階段の段差 1 を外す）
  - `:passes`     ならす段数（**既定 0**。点を動かすと面積が痩せるので、
                  意図があるときだけ上げる）"
  ([pts] (fit pts {}))
  ([pts opts]
   (if (< (count pts) 4)
     {:points pts :corners #{}}
     (let [cs (corners pts opts)
           sm (smooth pts cs (get opts :passes 0))]
       {:points sm :corners cs}))))

(defn ->d
  "後方互換の薄い委譲。実体は `shirohan.geom/curve->d`（引くのは出力層の仕事）。"
  [{:keys [points corners]}]
  (geom/curve->d points (or corners #{})))
