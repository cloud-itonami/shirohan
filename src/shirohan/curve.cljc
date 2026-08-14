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
(defn- abs* [x] (if (neg? x) (- x) x))
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

(defn- walk
  "頂点 i から `dir` 方向へ**弧長 span 以上**進んだ先の点。輪郭が細かく刻まれて
  いても、進む距離が同じなので同じ形が同じ角度に見える。"
  [pts i dir span]
  (let [n (count pts)]
    (loop [k i acc 0.0 steps 0]
      (let [k' (mod (+ k dir n) n)
            [ax ay] (nth pts k) [bx by] (nth pts k')
            acc' (+ acc (sqrt (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay)))))]
        (if (or (>= acc' span) (>= steps (quot n 3)))
          (nth pts k')
          (recur k' acc' (inc steps)))))))

(defn- almost-zero? [x] (< (abs* x) 1e-6))

(defn- axis-aligned?
  "辺が軸に平行（画素の階段）。"
  [[ax ay] [bx by]]
  (or (almost-zero? (- ax bx)) (almost-zero? (- ay by))))

(defn- turn-sign
  "頂点 i の曲がり符号。+1 左、-1 右、0 直進。"
  [pts i]
  (let [n (count pts)
        [ax ay] (nth pts (mod (+ i -1 n) n))
        [bx by] (nth pts i)
        [cx cy] (nth pts (mod (inc i) n))
        z (- (* (- bx ax) (- cy by)) (* (- by ay) (- cx bx)))]
    (cond (> z 1e-9) 1
          (< z -1e-9) -1
          :else 0)))

(defn- hv-right-angle?
  "軸平行の 90°（画素の段の角）。星の尖り（もっと鋭い）や斜めの辺は外れる。"
  [pts i]
  (let [n (count pts)
        a (nth pts (mod (+ i -1 n) n))
        b (nth pts i)
        c (nth pts (mod (inc i) n))]
    (and (axis-aligned? a b)
         (axis-aligned? b c)
         (< (abs* (- (turn-angle pts i) 90.0)) 25.0))))

(defn- edge-len [pts i]
  (let [n (count pts)
        [ax ay] (nth pts i)
        [bx by] (nth pts (mod (inc i) n))]
    (sqrt (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))))

(defn collapse-stairs
  "DP が残す短い軸平行の段を、対角線に畳む。

  Catmull-Rom は**点を通る**ので、段の 90° 頂点を残したまま当てはめると
  そこを必ず通り波打つ（実測 2026-08-14: 人物シルエットで p90 曲がり 12°、
  Illustrator は 5°）。角検出もその 90° を本物の角と誤認し、接線 0 で
  さらに尖らせる（同日、295 点中 54 が角）。

  畳む条件は 3 つ全部:

  1. 入出の辺が軸平行で約 90°
  2. 前後の辺が短い（周長の 0.5%、床 3px。正方形の辺はこれより長い）
  3. 曲がりの符号が隣と逆（階段は +90/-90 が交互。正方形は全部同じ向き）

  点をならす（Chaikin / 移動平均）のではない。段の頂点を落とすだけなので
  正方形の 4 隅は動かない。楕円は 1px 段が対角線の短い折れ線になり、
  点数は半分近く残る。"
  ([pts] (collapse-stairs pts {}))
  ([pts {:keys [span-ratio min-step]
         :or {span-ratio 0.005 min-step 3.0}}]
   (let [n0 (count pts)]
     (if (< n0 6)
       pts
       (let [seg (fn [i] (edge-len pts i))
             perim (reduce + (map seg (range n0)))
             max-step (max min-step (* span-ratio perim))]
         (loop [cur (vec pts) guard 0]
           (let [m (count cur)]
             (if (or (< m 6) (> guard (* 2 n0)))
               cur
               (let [drop? (fn [i]
                             (and (hv-right-angle? cur i)
                                  (let [a (edge-len cur (mod (+ i -1 m) m))
                                        b (edge-len cur i)]
                                    (<= (min a b) max-step))
                                  (let [s (turn-sign cur i)
                                        sp (turn-sign cur (mod (+ i -1 m) m))
                                        sn (turn-sign cur (mod (inc i) m))]
                                    (and (not= s 0)
                                         (or (= sp (- s)) (= sn (- s)))))))
                     i (first (filter drop? (range m)))]
                 (if (nil? i)
                   cur
                   (recur (into (subvec cur 0 i) (subvec cur (inc i)))
                          (inc guard))))))))))))

(defn corners
  "図案の角とみなす頂点の添字。

  角と判定した頂点は接線を 0 にする＝**意図的に尖らせる**ので、誤判定はそのまま
  ギザギザになる。だから「大きく曲がっている」だけでは足りない —— DP が残す
  2〜3 画素のジグザグも 90 度に曲がっており、曲線に当てはめたはずの輪郭が折れ線に
  戻る（これが『まだギザギザ』の正体。実測 2026-08-01）。

  ## **弧長で測った前後の弦**の角度で判定する

  1 辺ずつ見ると階段の往復（+90, -90, +90 …）も本物の角も同じ 90 度になる。
  前後へ**周長の 0.5% だけ進んだ点**との弦どうしの角度なら、階段の往復は打ち
  消し合って小さくなり、本物の角だけが残る。

  進む距離を周長比にするのが要点 —— 階段の残りかすは追跡画素の大きさ（数画素）
  で決まり図案が大きくなっても伸びないが、本物の角は図案と一緒に伸びる。
  たどり着けなかった案の記録:

  | 案 | 落ちた理由 |
  |---|---|
  | 絶対画素の辺長（5px） | 256px で星の尖りが全部落ちた |
  | 辺長の中央値に対する比 | 尖った角ほど辺が短く、星が 0 個 |
  | 前後の頂点と比較（孤立判定） | 正方形は角が 4 つ連続していて落ちる |
  | 頂点数固定の弦（前後 3 頂点） | 高解像度では弧が短すぎ、星を 20 個以上に誤検出 |
  | 前後の**辺**が周長比以上 | 尖った角は辺自体が短いので 512px 以上で落ちる |"
  [pts {:keys [corner-deg span-ratio min-span]
        :or {corner-deg 50.0 span-ratio 0.005}}]
  (let [n (count pts)
        seg (fn [i] (let [[ax ay] (nth pts i) [bx by] (nth pts (mod (inc i) n))]
                      (sqrt (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))))
        perim (reduce + (map seg (range n)))
        span (or min-span (max 2.0 (* span-ratio perim)))
        ang (fn [i]
              (let [[ax ay] (walk pts i -1 span)
                    [cx cy] (nth pts i)
                    [bx by] (walk pts i 1 span)
                    ux (- cx ax) uy (- cy ay) vx (- bx cx) vy (- by cy)
                    lu (sqrt (+ (* ux ux) (* uy uy)))
                    lv (sqrt (+ (* vx vx) (* vy vy)))]
                (if (or (< lu 1e-9) (< lv 1e-9))
                  0.0
                  (let [c (/ (+ (* ux vx) (* uy vy)) (* lu lv))
                        c (cond (> c 1.0) 1.0 (< c -1.0) -1.0 :else c)]
                    (/ (* (acos c) 180.0) pi)))))
        angs (mapv ang (range n))
        cand (into #{} (filter #(>= (nth angs %) corner-deg)) (range n))
        ;; 各頂点までの弧長（先頭から）。近さの判定に使う。
        cum (reduce (fn [v i] (conj v (+ (peek v) (seg i)))) [0.0] (range n))
        total (peek cum)
        arc (fn [i j] (let [d (abs (- (nth cum i) (nth cum j)))]
                        (min d (- total d))))
        ;; 1 つの尖りは複数の頂点にまたがって候補になる。**弧長 span 以内では
        ;; 一番鋭い 1 点だけ残す**（非最大抑制）。残さないと星の尖り 5 個が 18 個の
        ;; 角になり、そこだけ折れ線に戻る。
        best? (fn [i] (every? (fn [j]
                                (or (= i j)
                                    (> (arc i j) span)
                                    (> (nth angs i) (nth angs j))
                                    (and (== (nth angs i) (nth angs j)) (< i j))))
                              cand))]
    (into #{} (filter best?) cand)))

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

  先に `collapse-stairs` で短い軸平行の段を畳んでから角を見る。

  opts:
  - `:corner-deg` 角とみなす曲がり角の下限（既定 50）
  - `:min-edge`   角とみなすのに必要な前後の辺長（既定 2.0。階段の段差 1 を外す）
  - `:passes`     ならす段数（**既定 0**。点を動かすと面積が痩せるので、
                  意図があるときだけ上げる）"
  ([pts] (fit pts {}))
  ([pts opts]
   (if (< (count pts) 4)
     {:points pts :corners #{}}
     (let [collapsed (collapse-stairs pts opts)
           cs (corners collapsed opts)
           sm (smooth collapsed cs (get opts :passes 0))]
       {:points sm :corners cs}))))

(defn ->d
  "後方互換の薄い委譲。実体は `shirohan.geom/curve->d`（引くのは出力層の仕事）。"
  [{:keys [points corners]}]
  (geom/curve->d points (or corners #{})))
