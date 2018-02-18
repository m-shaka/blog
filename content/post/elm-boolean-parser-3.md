---
title: "ElmのParserライブラリで論理演算(3)"
date: 2018-02-18T11:14:03+09:00
tags:
  - Elm
---
前回のを公開したら、親切な友人がよりよい（かつ一般的な）実装方法を教えてくれた。
そなたに感謝を。
調べたところ[再帰下降構文解析](https://ja.wikipedia.org/wiki/%E5%86%8D%E5%B8%B0%E4%B8%8B%E9%99%8D%E6%A7%8B%E6%96%87%E8%A7%A3%E6%9E%90)というものらしい。

BNFで書くと多分こんな感じになる。

```
<additiveExpr> ::= <multiplicativeExpr> [ '||' <additiveExpr> ]*  ## 式の本体
<multiplicativeExpr> ::= <factor> [ '&&' <multiplicativeExpr> ]*
<factor> ::= '(' <additiveExpr> ')' | <base>
<base> ::= 'T' | 'F' | '!'<factor>
```

日本語で書くとこうかしら。

<dl class='dl-horizontal'>
  <dt>additiveExpr</dt>
  <dd>multiplicativeExprか、それとadditiveExprの和</dd>
  <dt>multiplicativeExpr</dt>
  <dd>factorか、それとmultiplicativeExprの積</dd>
  <dt>factor</dt>
  <dd>括弧でくくられたadditiveExprかbase</dd>
  <dt>base</dt>
  <dd>TかFかfactorの否定</dd>
</dl>

公開していた[デモページ](https://m-shaka.github.io/boolean-parser-elm/)の方もこちらの実装に直したので、githubから直にソースコードを埋め込む[^1]。

[^1]: gist-itというサービスを使う。知らんかった

<script src="http://gist-it.appspot.com/https://github.com/m-shaka/boolean-parser-elm/blob/master/src/BooleanParser.elm?slice=2:86"></script>

パーサー本体はadditiveExprだが、前後に空白を許容するのと、式のあとに不適切な文字列が入った時に（"T hoge"とか）failするように`.| end`で文字列の終了を要求するためにexprでラップしている。
関数の定義が相互参照しまくりなので最初は戸惑ったが、Parser.lazyで遅延評価にしてやるとコンパイルが通る[^2]。
あとついでに否定を"not"から"!"に直した。

この実装でよいなと思ったのは、演算の優先度が規定されるという点だ。
括弧で順番を明示しない限り、かならず和より積が、積より否定が優先される。

これで論理和と括弧表記にもきれいに対応できた。うれぴよ。

[^2]: Elmは見た目はHaskellだけどデフォルトでは正格評価。
