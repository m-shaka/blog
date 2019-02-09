---
title: ElmのParserライブラリで論理演算(2)
date: 2018-02-17T13:12:57+09:00
tags:
  - Elm
---
### 2018/02/18追記
ここでの実装はあまり一般的なやり方ではないそうなので、苦労の痕跡として捉えて下さい。
[こちら](/post/elm-boolean-parser-3)で一般的な実装方法を試みています。

[前回](/post/elm-boolean-parser-1)は定数と否定まで作った。
今回は連言（論理積）を追加しようと思ったら苦戦したお話。

## Termを拡張しよう
前回作ったTerm型の定義に連言を追加する。

```elm
type Term =
  T
  | F
  | Not Term
  | And Term Term -- 追加分
```

連言は2つの項を受け取るので、定義上でも引数を2つ受け取るコンストラクタになる。
ここまでは無問題。

## パーサーの拡張
連言を表すシンボルには"&&"を採用する。
プログラミング言語では一般的[^1]。
まずは深く考えず以下のように拡張してみた。

[^1]: なら否定も"!"にして統一しろよという。雑さが伺える

```elm
import Parser exposing (..)

term: Parser Term
term =
    oneOf
        [succeed T
            |. keyword "T"
        , succeed F
            |. keyword "F"
        , succeed Not
            |. keyword "not"
            |. spaces
            |= lazy (\_ -> term)
        -- ここから連言
        , succeed And
            |= lazy (\_ -> term)
            |. spaces
            |. symbol "&&"
            |. spaces
            |= lazy (\_ -> term)
        ]
```

2つの項の間に"&&"が挟まる自然なかたち。

しかしこれはうまくいかない。
というのも、パーサーは与えられた文字列を左から走査していくのだが、まず一文字目の"T"を受け取った時点で`T`としてのパースに成功してしまうからだ（Fでも同様）。

では、oneOfは引数で受け取ったパーサーリストの先頭からパースできるか試していくのだから、リストの先頭に連言を置けばいいのではと思うかもしれない。
ところがこれも失敗に終わる。
無限ループに陥るのである。

"T && T"という文字列を与えたとしよう。
パーサーは、まずAndでのパースを試みる。
文字列の先頭の"T"を受け取った時、`lazy (\_ -> term)`によりこの"T"に対しまたtermパーサーが走る。
すると今度は"T"に対してAndのパースが実行され、以下はそれの繰り返しになってプログラムが終了しなくなってしまうのだ。

## &&の前後で処理を分割する
そこで筆者が思いついた解決策はこうだ。

1. まず、T, F, Not Termのいずれかとしてパースする
1. その後に"&&"が続けばAndとして解釈し、空白で終わればそのまま終了する

```elm
term : Parser Term
term =
    let
        true =
            succeed T |. keyword "T"

        false =
            succeed F |. keyword "F"

        not_ =
            succeed Not
                |. keyword "not"
                |. spaces
                |= oneOf [ true, false, lazy (\_ -> not_) ]
    in
        succeed identity
            |= oneOf [ true, false, not_ ]
            |. spaces
            |> andThen
                (\t ->
                    oneOf
                        [ succeed (And t)
                            |. symbol "&&"
                            |. spaces
                            |= lazy (\_ -> term)
                        , succeed t
                            |. spaces
                        ]
                )
```

既存コードへの変更点としては、基底形と否定は複数回使うのでローカル変数に束縛している。
否定については中身にも若干の変更を加えた。
否定の引数には**T, F, Not Termのいずれか**が入るようにしてある。
こうしておかないと"not T && T"が`Not (And T T)`とパースされてしまうが、選言自体の否定なら"not (T && T)"と対応させるのが自然だろう。
これを避けるために、さしあたっては否定を選言以外の項を受け取るように制限する[^2]。

[^2]: もちろんTermの定義上は任意の項を受け取れるようにしなければならないが、そのためには括弧を導入した方がよいので今回は目を瞑る

真理値ないし否定形としてパースした値を`andThen`に渡す（以下は型定義）。

```elm
andThen : (a -> Parser b) -> Parser a -> Parser b
```

型定義からもわかるように、andThenは任意の型A, Bについて、Aの値を受け取ってBのパーサーを返すコールバック関数と、Aのパーサーを受け取る。
コールバック関数が担っているのは、先に述べた手順の2つ目の処理だ。
`|= oneOf [ true, false, not_ ]`でパースした値はコールバック関数に渡され、続く文字列が"&&"ならばAndとして、空白ならばそのまま返される[^3]。

[^3]: `identity`は組み込み関数で、要するに恒等写像。 また`(And t)`は部分適用。

駆け足になったが、以上で連言のパースになんとか成功した。
選言も全く同様にパースできることは明らかだと思うので説明は不要でしょう。
また、これが最適解かもわからないのでよりよい実装があれば教えて頂きたい。

次回は括弧表記に対応して完結予定。
括弧を導入すると選言の否定が出来るようになる。