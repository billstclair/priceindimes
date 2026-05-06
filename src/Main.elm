---------------------------------------------------------------
--
-- Main.elm
-- priceindimes - convert USD to silver dimes.
-- Copyright (c) 2026 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Html exposing (Html, a, div, fieldset, img, input, legend, p, span, text, textarea, ul)
import Html.Attributes exposing (checked, disabled, href, name, src, style, type_, value, width)
import Json.Encode as JE exposing (Value)
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        , subscriptions = \model -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { url : Url
    , key : Key
    }


type Msg
    = Nop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { url = url
    , key = key
    }
        |> withNoCmd


h1 : String -> Html msg
h1 string =
    Html.h1 [] [ text string ]


view : Model -> Document Msg
view model =
    { title = "PriceInDimes"
    , body =
        [ div
            [ style "margin" "10px"
            , style "height" "90%"
            ]
            [ h1 "PriceInDimes.com" ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model |> withNoCmd

        OnUrlChange url ->
            model |> withNoCmd

        OnUrlRequest url ->
            model |> withNoCmd
