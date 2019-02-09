---
title: "Persistentでテスト時だけSqliteを使う"
date: 2019-02-08T22:24:27+09:00
tags:
  - Haskell
---

仕事でpersistent (postgresql) を使っているのだけど、テスト用にDB作るのもなんだかな〜 [^1] ということで[これ](https://myuon.github.io/posts/haskell-di/)を参考に、DIしてテスト時だけsqlite（メモリ）を使うようにしてみた。

[^1]: django-pytestなんかは全部勝手にやってくれて便利だった……

まずテーブルを定義。コードそのまま載せると長いので定義部分だけ。

```
Person
    name String
    age Int Maybe
    deriving Show
```

DB接続の設計と実装をぽいっと。

```DI.hs
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

module DI where

import           Control.Monad.Logger         (runNoLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8        as B
import           Data.Reflection              (Given, given)
import           Database.Persist.Postgresql
import           Database.Persist.Sql

-- 設計
class DBAccess a where
  runSql :: a -> SqlPersistM b -> IO b

data SomeDBAccess = forall a. DBAccess a => SomeDBAccess a

instance DBAccess SomeDBAccess where
  runSql (SomeDBAccess a) = runSql a

type UseDBAccess = Given SomeDBAccess

useDBAccess :: UseDBAccess => SomeDBAccess
useDBAccess = given

-- Postgresの実装
data PostgresAccess = PostgresAccess

instance DBAccess PostgresAccess where
  runSql _ sql = runResourceT . runNoLoggingT $ withPostgresqlConn connStr $ runSqlConn sql
    where
      connStr = B.pack "host=localhost port=5432 user=postgres dbname=app password=password"

postgresAccess :: SomeDBAccess
postgresAccess = SomeDBAccess PostgresAccess
```

注入するときは`give :: a -> (Data.Reflection.Given a => r) -> r`を使う。これで動く。やったね！

```Main.hs
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Reflection        (give)
import           Database.Persist
import           Database.Persist.Sql
import           DI
import           Model

main :: IO ()
main = give postgresAccess $
  runSql useDBAccess $ do
    runMigration migrateAll
    pid <- insert samplePerson
    person <- get pid
    liftIO $ print (person :: Maybe Person)


samplePerson :: Person
samplePerson = Person "John Doe" $ Just 20
```

接続先を変えたいときは`give postgresAccess`の部分をいじるだけでよいので、ちゃんとDAOを書く場合も疎結合になってよい。テストコードはこんな感じ。

```Spec.hs
import           Control.Monad.Logger         (runNoLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Reflection              (give)
import qualified Data.Text                    as T
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           DI
import           Model
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "insert person" $
    it "insert correctly" $ do
      person <- give sqliteAccess $ runSql useDBAccess $ do
        runMigration migrateAll
        pid <- insert samplePerson
        get pid
      (Just . personName $ samplePerson) `shouldBe` fmap personName person

samplePerson :: Person
samplePerson = Person "John Doe" $ Just 20

-- sqliteの実装
data SqliteAccess = SqliteAccess

instance DBAccess SqliteAccess where
  runSql _ sql = runResourceT . runNoLoggingT $ withSqliteConn (T.pack ":memory:") $ runSqlConn sql

sqliteAccess :: SomeDBAccess
sqliteAccess = SomeDBAccess SqliteAccess
```

[Functor.Tokyo](https://functor.tokyo/blog/2015-11-20-testing-db-access)さんが、Stateを使ったやり方を紹介しているけど（Reflectionは使ってない）、外部依存を可能な限り排除したいならそっちの方がいいのかも知れない。あとメモリに乗っけるとrunSqlを呼ぶ度にマイグレートしなきゃならないのも少し面倒なんだけど、ユニットテストとしてはテストケース毎にDBがリセットされるのは正しい振る舞いよね。
