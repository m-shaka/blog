---
title: "elm/fileでファイルアップロード"
date: 2019-02-11T22:54:24+09:00
tags:
  - Elm
---
"elm file upload"とかでググるとJS側で処理する方法や0.18までのnativeモジュールを使う方法が出てくるんだけど、実は0.19から[elm/file](https://package.elm-lang.org/packages/elm/file/latest/)という公式ライブラリが追加されて、かなり簡単にファイル処理ができるようになった。
存在することさえわかれば後はドキュメント読めば使えると思うけど、いちおう使用例を3つ紹介しておく。

## そのままリクエストボディに入れる
自分はあまりやったことないけどもっともシンプルな方法。コード例は抜粋。

```Elm
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)
import Http

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model, Select.file [] CsvSelected )

        CsvSelected f ->
            ( model
            , Http.post
                { url = "/upload"
                , body = Http.fileBody f
                , expect = Http.expectWhatever Uploaded
                }
            )
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CsvRequested ] [ text "Upload csv" ]
        ]
```

ボタンをクリックすると`File.Select.file`が呼ばれてファイル選択ウィンドウが開き、GUIから選択すると引数として渡してある`CsvSelected : File -> Msg`メッセージが発火する。
ここでbodyに`Http.fileBody : File -> Http.Body`を受け取ったFile型の値に適用したものを渡してポストする。Content-Typeは自動で入れてくれるっぽい。

## multipart/form-data
フォームとして送信する。これには`Http.multipartBody : List Http.Part -> Http.Body`を使う。コード例はHttp.postの部分だけ上のものと異なる。

```Elm
CsvSelected f ->
    ( model
    , Http.post
        { url = "/upload"
        , body = Http.multipartBody [ Http.filePart "fieldName" file ]
        , expect = Http.expectWhatever Uploaded
        }
    )
```

## Base64にエンコードしてJSONで送る
`elm/file`はBase64へのエンコーディングもサポートしている。

```Elm
CsvRequested ->
    ( model, Task.perform Encoded <| File.toUrl f )

Encoded encodedString ->
    ( model
    , Http.post
        { url = "/upload"
        , body = Http.jsonBody <| E.object [ ( "file", E.string encodedString ) ]
        , expect = Http.expectWhatever Uploaded
        }
    )
```

`File.toUrl`でファイルをエンコーディングできるが、戻り値はTaskになるので別途Msgを作ってポストする（あるいは`Http.task`でもいいと思うが使ったことが無い）。
