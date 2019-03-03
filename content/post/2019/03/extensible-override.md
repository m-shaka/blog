---
title: "Extensibleのレコードを環境変数で上書き"
date: 2019-03-03T13:56:22+09:00
tags:
  - Haskell
---
Extensibleのレコードを食わせて、レコードのキーと一致する環境変数があったら上書きして返す、ということをやってみた（まあ同僚の案なんだけど）。
型の理解にめっちゃ苦労した、というか未だにモヤっとしているんだけど、頑張ったのでアウトプットしておく。

Extensibleの使ったことがないとわからない話になるので、それでも読みたいという善良な方は[Extensible攻略Wiki](http://wiki.hask.moe/)を先に読もう。
私も使い始めで全然理解できてはいないけど、めっちゃ便利でイケてるということはわかる。

## 準備
レコードの型とデフォルトの値を定義する。
あとインポートとGHC拡張もここに全部載せちゃう。

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Identity
import           Data.Extensible
import           Data.Maybe             (maybe)
import           Numeric.Natural        (Natural)
import           System.Environment     (getEnvironment, setEnv)
import           Text.Read              (readMaybe)

type MyRecord = Record
  '[ "HOGE_STR" :> String
   , "HOGE_NAT" :> Natural
   , "HOGE_BOOL" :> Bool]

-- System.Envrionment.getEnvironmentの戻り値の型のエイリアス
type EnvKeyValue = [(String, String)]

defRecord :: MyRecord
defRecord =
  itemAssoc (Proxy @ "HOGE_STR") @= "def"
  <: itemAssoc (Proxy @ "HOGE_NAT") @= 0
  <: itemAssoc (Proxy @ "HOGE_BOOL") @= False
  <: nil
```

環境変数のキーには慣習として大文字を用いるが、OverloadedLabels拡張では大文字始まりが許されていないので、あの便利な`#hoge @= "fuga"`という構文が使えない。
代わりに`itemAssoc (Proxy @ "hoge") @= "fuga"`を使う。記述量が増えるのはちょっと嫌だが仕方ない。


## 上書きする
そして本題の上書き部分。

```haskell
merge  :: Forall (KeyValue KnownSymbol Read) xs => Record xs -> IO (Record xs)
merge r = do
  e <- getEnvironment
  pure $ hmapWithIndexFor p (f e) r
  where
    p = Proxy @ (KeyValue KnownSymbol Read)
    f :: KeyValue KnownSymbol Read x => EnvKeyValue -> Membership xs x -> Field Identity x -> Field Identity x
    f e m (Field idVal) = Field $ maybe idVal pure (readEnv (stringAssocKey m) e)

readEnv :: Read a => String -> EnvKeyValue -> Maybe a
readEnv k kvs = readMaybe =<< lookup k kvs
```

`Forall`の制約は、文字通り型レベルリスト`xs`の任意の要素に関するもののよう。
ここではキーが`KnownSymbol`、つまり何らかの型レベル文字列、値が`Read`のインスタンスに限定されている。

`hmapWithIndexFor`も名前から推測できるように、レコードに対するmap処理のための関数。
ここが結構難しかったので、まず型を書いておいてから引数について頑張って理解していく。

```haskell
hmapWithIndexFor :: Forall c xs => proxy c -> (forall x. c x => Membership xs x -> g x -> h x) -> (g :* xs) -> h :* xs
```

### 第一引数
`xs`には`merge`の型宣言で制約を与えたので、第一引数にも同じ制約を`Proxy`のかたちで与えてあげる。

### 第二引数
第二引数は処理の中身になる。
まず任意のxについてxはcを満たすので、xのキーは文字列で値はReadのインスタンスの型だ。
`Membership`に関しては正直のところ[攻略Wiki](http://wiki.hask.moe/Membership)を読んでもあまりわかった気がしないんだけど、とりあえず`stringAssocKey`を適用することでキーを文字列として取得できる。

そんでgとhって何者かってことなんだけど、戻り値の型にある`:*`のドキュメントを読んだ理解だと、この文脈では`g :* xs`と書いたときは`RecordOf g xs`で、`g x`だと`Field g x`という型になるっぽい。

`MyRecord`の定義に使ってる`Record`は`RecordOf Indentity`のエイリアスなので、`g x`と`h x`は両方とも`Field Identity x`となる（`h x`が`Field Maybe x`とかになってもよいはず。種が`* -> *`ならおｋ）。

第二引数に与えてる関数`f`の具体的な中身をまとめると、

1. フィールドのキーを文字列で取得
1. 文字列に対応する環境変数を探し、無かったらもともとの値を、あったらそれを`Field . Identity`で包んで返す

という流れになっている。

## 試す
実際に動かしてみる。

```haskell
main :: IO ()
main = do
  print defRecord
  setEnv "HOGE_STR" "\"merged\""
  setEnv "HOGE_NAT" "100"
  setEnv "HOGE_BOOL" "True"
  merge defRecord >>= print
```

ちゃんと上書きされてる！

```
HOGE_STR @= "def" <: HOGE_NAT @= 0 <: HOGE_BOOL @= False <: nil
HOGE_STR @= "merged" <: HOGE_NAT @= 100 <: HOGE_BOOL @= True <: nil
```

ちなみに、このままだと`readMaybe`でそのまま文字列として持っておけばいいものまでreadしているため、環境変数の指定時にダブルクォートが必要になっちゃうので、型クラス作って`readEnv`を多相化して対応した。
NaturalとかBoolまでインスタンス宣言する必要が生じるのでもっとうまいやり方あれば教えて欲しい。
