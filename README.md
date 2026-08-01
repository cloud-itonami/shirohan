# shirohan — 白版（しろはん）の製版エンジン

ベクタの図案から、**Tシャツ等のガーメント装飾に刷る版**を組む純 `.cljc` ライブラリ。
白版（白インクの下地）・白抜き（knockout）・スポットカラー版分解・見当合わせを、
LLM もネットワークも使わない決定論的な純関数で行う。

公開サービス: <https://itonami.cloud/cloud-itonami/shirohan/>

```clojure
(require '[shirohan.core :as shirohan])

(def job (shirohan/plan svg-string {:choke-mm 0.2 :print-width-mm 260}))

(:findings job)              ;; 刷る前に潰すべき問題
(map :label (:plates job))   ;; ("白版（下地）" "スポット版 #FF2D95" …)
(shirohan/films job)         ;; {"00-white.svg" "<svg…>" "01-spot….svg" "<svg…>"}
```

## 白版とは

濃色のボディに色を刷ると、生地の色が透けて図案が沈む。だから色版の下に
**白インクの下地＝白版**を先に刷る。白版が図案とぴったり同じ大きさだと、刷り位置が
0.1mm でもずれた瞬間に白が図案の外へはみ出して縁が白く光る（白フチ／ハロー）。
そこで白版は図案より **choke（縮み代）** ぶん内側に詰める。

```
図案の輪郭 ─┐
            ├─ choke 0.2mm 内側 ──▶ 白版
白抜きの穴 ─┴─ choke 0.2mm 外側 ──▶ 白版（穴が広がる＝白が縮む）
```

**穴が逆向きに動く**のが要点。目的は「白の面を全周 choke だけ痩せさせる」ことなので、
外周は内へ、穴は外へ動かす。どちらの輪郭かは**入れ子の深さの偶奇**で決める
（「O」の字は外周と内周の2本でできていて、内周が囲んでいるのは穴）。

## spec

```clojure
{:choke-mm        0.2        ; 白版の縮み代。0.15〜0.3mm が実用域
 :min-line-mm     0.3        ; スクリーンで刷れる最小線幅の目安
 :margin-mm       12.0       ; 版の余白（見当合わせマークが入る）
 :print-width-mm  280.0      ; 刷り上がりの幅
 :garment-color   "#1f1f1f"  ; プレビューのボディ色（版ではない）
 :white-underbase? true
 :registration?   true
 :knockout-fill   "#ffffff"  ; 白抜きとして扱う塗り（nil で無効）
 :tolerance-mm    0.05}      ; 曲線を折るときの弦の許容誤差
```

## 出力

| 関数 | 何が出るか |
|---|---|
| `plan` | 版一式（輪郭・所見・寸法）。純関数 |
| `films` | `{"00-white.svg" … "01-spot….svg" …}`。**白地に黒**の版下 |
| `preview` | ボディ色の上に刷る順で重ねた仕上がり予想 |
| `underbase-check` | 白版（白）と図案の輪郭（マゼンタ）の重なり。choke の確認用 |
| `summary` | 承認・監査に載せる要約 |

ファイル名は `00-white.svg` のように**刷る順が先頭に来る** —— 並べたときに順番が
崩れないことが、現場で版を取り違えないことに直結する。

版下は**黒 100% / 白 0%** の2値で出す。スクリーンの感光乳剤は光の有無しか見ないので、
インクの色を付けても意味がなく、むしろ濃度が下がって露光が甘くなる。

## 白抜きの指定

既定では **塗りが白（`#ffffff`）の図形を白抜きとして扱う**。濃色ボディに刷る版下では、
白は「生地を見せる穴」か「白インク」のどちらかで、図案データだけからは決まらない。
既定を白抜きにするのは、間違えたときに**インクを載せすぎるより版が抜ける方が
刷る前に気づける**から。`:knockout-fill` で変えられるし、`id`/`class` に `knockout` を
含む要素は塗りに関係なく白抜きになる。

## この道具が「しないこと」

読めなかったものを黙って落とさず、必ず `:findings` で報告する。版から図案が消えた
理由が分からないまま刷るのがいちばん高くつく。

| 所見 | 意味 |
|---|---|
| `:text-not-outlined` | `<text>` はアウトライン化していないので版に載せられない。**入稿前にパス化する** |
| `:raster-image` | `<image>` はラスタ。ベクタ版は作れない |
| `:use-reference` | `<use>` の参照解決を持たない。展開してから入稿する |
| `:unresolvable-fill` | グラデーション・パターン・`currentColor` は単色の版に分解できない |
| `:stroke-only` | `fill="none"` の図形。線を面に変換（stroke のアウトライン化）していない |
| `:open-contour` | 閉じていないサブパスは面にならない |
| `:choke-erases-feature` | 最小特徴幅が choke の2倍を下回る。**この部分は白版から消える** |
| `:below-min-line` | 刷れる最小線幅を下回る。刷ると潰れるか飛ぶ |
| `:knockout-outside-art` | 白抜きがどの図案の内側にもない。版に効果を持たない |
| `:no-art` | 塗りのある閉じた図形が1つも無い |

`shirohan/blocking?` が真の所見は**刷る前に必ず人が見る**。`:below-min-line` は
刷れるが潰れうるという注意なので、そこには含めない —— 止めるべきものだけを止める。

### 幾何の限界（明記しておく）

- **多角形のブール演算器を持たない。** 穴は SVG の `<mask>` で表し、ラスタライザに
  解かせる。輪郭が何本重なっても、穴の中に穴があっても正しい。自前のブール演算は
  交点計算の数値誤差でしか壊れないので、持たない方が強い。
- **choke は頂点法線オフセット（miter 継ぎ）** で、Straight Skeleton ではない。
  δ が特徴幅の半分を超えると輪郭は自己交差する。版下の choke は 0.1〜0.5mm で
  Tシャツの図案に対しては十分小さいのでこれで足りるが、足りない場合を黙って
  通さないために `:choke-erases-feature` を出す。
- **`min-feature-width` は頂点でしか測らない**近似。辺の途中がいちばん細い形は
  過大評価する（＝見逃す側に倒れる）ので、choke 値そのものとの比較も併せて出す。

## ランタイム

`shirohan.{geom,path,artwork,plate,svg,core}` は **`clojure.string` しか引かない**。
第一級のランタイムは ClojureScript（公開サイトではブラウザ内 scittle がこの `.cljc` を
そのまま読む）で、JVM はテストハーネス専用。正規表現に `(?s)` `(?i)` を書かない
（JavaScript の `RegExp` に無く、書いた瞬間にブラウザで死ぬ）—— これはテストで強制する。

```bash
clojure -M:test                                  # JVM テストハーネス（54 tests / 118 assertions）
npx nbb --classpath src scripts/smoke.cljs       # SCI（nbb / scittle）で動くことの確認
```

## ライセンス

MIT. cloud-itonami fleet の一部。
