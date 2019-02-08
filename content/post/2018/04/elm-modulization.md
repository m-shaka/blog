---
title: "Elmでviewを分割"
date: 2018-04-22T19:02:53+09:00
tags:
  - Elm
---
アーキテクチャを複数のファイルに切り分けて入れ子にするのにちょっとひと工夫必要だったのだけど、これに関して日本語での情報があまり見当たらなかったので残しておく。
先に結論を言っておくと`Html.map`と`Cmd.map`が肝。
なおElmのバージョンは0.18。

文字列の表示をオンオフするだけのアプリで考えてみよう。
プログラムの全体はこう。


```Main.elm
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program Never Bool Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    Bool


type Msg
    = Toggle


init : ( Bool, Cmd msg )
init =
    ( False, Cmd.none )


view : Model -> Html Msg
view model =
    let
        viewText =
            (if model then
                "block"
             else
                "none"
            )
                |> (\d -> p [ style [ ( "display", d ) ] ] [ text "text" ])
    in
    div []
        [ h1 [] [text "demo"]
        , button [ onClick Toggle ] [ text "toggle" ]
        , viewText
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( not model, Cmd.none )
```


ここからh1以外のviewの要素と状態更新に関わる部分を別ファイルに切り出す。
言うまでもなくこの規模のアプリではそんなことをする必要はないけど、規模が大きくなると実質必須となる。


```Toggle.elm
module Toggle exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    Bool


type Msg
    = Toggle


init : ( Bool, Cmd msg )
init =
    ( False, Cmd.none )


view : Model -> Html Msg
view model =
    let
        viewText =
            (if model then
                "block"
             else
                "none"
            )
                |> (\d -> p [ style [ ( "display", d ) ] ] [ text "text" ])
    in
    div []
        [ button [ onClick Toggle ] [ text "toggle" ]
        , viewText
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( not model, Cmd.none )
```


```Main.elm
module Main exposing (..)

import Html exposing (..)
import Toggle


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { toggle : Toggle.Model }


type Msg
    = ToggleMsg Toggle.Msg


init : ( Model, Cmd msg )
init =
    ( { toggle = False }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "demo" ]
        , Toggle.view model.toggle |> Html.map ToggleMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMsg msg_ ->
            let
                ( m_, cmd ) =
                    Toggle.update msg_ model.toggle
            in
            ( { model | toggle = m_ }, Cmd.map ToggleMsg cmd )
```


Toggle.elmを作って、Model, Msgとviewの一部を別モジュールとしMain.elmから呼び出すようにした。
今回は外部モジュールも状態も1つだけだが、複数になることを想定してModelやMsgを定義しなおした。
Msgを上記のように定義しておくと、呼び出すビューが増えた場合も


```elm
type Msg =
    ToggleMsg Toggle.Msg
    | HogeMsg Hoge.Msg
```


というかたちで拡張しやすく、updateも書きやすい。
ただ、`Toggle.update`は`Toggle.Msg -> Toggle.Model -> (Toggle.Model, Cmd Toggle.Msg)`という定義になっているので、Mainモジュールのupdateとは型が合わない。

そこで`Cmd.map`の出番となる。
mapという名前から大方予想はつくと思うけど、型はこうなっている（[elm-packageのドキュメント](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Platform-Cmd#map)）。

`(a -> msg) -> Cmd a -> Cmd msg`

ToggleMsgは`Toggle.Msg -> Msg`と定義されているので、Main.update関数の`Cmd.map ToggleMsg cmd`という式は`Cmd Main.Msg`を返す。
viewについても同様で、Toggle.viewの戻り値が`Html Toggle.Msg`なので`Html.map`で置換してあげる。

Html.mapについては[ドキュメント](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#map)で少し説明があるけど、Cmd.mapは公式情報がないような。
若い言語なので情報不足は否めない。

なお、モジュールの切り分け方は[elm-spa-sample](https://github.com/rtfeldman/elm-spa-example)を参考にした。
しかしいきなりこれに当たるのはヘビーなんだよな……。
