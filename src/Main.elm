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


port module Main exposing (main)

--

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Html exposing (Html, a, div, fieldset, iframe, img, input, legend, p, span, table, td, text, textarea, th, tr, ul)
import Html.Attributes exposing (align, checked, disabled, height, href, id, name, size, src, style, type_, value, width)
import Html.Events exposing (onClick, onFocus, onInput)
import Json.Encode as JE exposing (Value)
import Task exposing (Task)
import Url exposing (Url)


port selectAll : String -> Cmd msg


port openWindow : Value -> Cmd msg


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
    { price : Float
    , priceInput : String
    , dollarsPerOz : Float
    , dollarsPerOzInput : String
    , url : Url
    , key : Key
    }


type Msg
    = Nop
    | AfterFocus String
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ReloadFromServer
    | InputPrice String
    | InputDollarsPerOz String


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { price = 1.0
    , priceInput = "1.0"
    , dollarsPerOz = 76.83
    , dollarsPerOzInput = "76.83"
    , url = url
    , key = key
    }
        |> withNoCmd


h1 : String -> Html msg
h1 string =
    Html.h1 [] [ text string ]


b : String -> Html msg
b string =
    Html.b []
        [ Html.text string ]


br : Html msg
br =
    Html.br [] []


pINPUT_SIZE =
    5


view : Model -> Document Msg
view model =
    { title = "PriceInDimes"
    , body =
        [ div
            [ style "margin" "10px"
            , style "height" "90%"
            ]
            [ h1 "Price In Dimes"
            , p []
                [ img
                    [ src "images/icon-192.png"
                    , width 192
                    , height 192
                    ]
                    []
                ]
            , let
                -- One silver dollar is 0.7734 troy ounces.
                -- A dime is 1/10 that much.
                -- one dime = 0.0734 troy ounces.
                dimes =
                    model.price / model.dollarsPerOz / (0.7734 / 10)
              in
              table []
                [ tr []
                    [ th [] [ b "Price:" ]
                    , td [ style "text-align" "right" ]
                        [ input
                            [ type_ "text"
                            , size pINPUT_SIZE
                            , style "text-align" "right"
                            , id "price"
                            , onFocus (AfterFocus "price")
                            , onInput InputPrice
                            , value model.priceInput
                            ]
                            []
                        ]
                    , td [ style "text-align" "left" ]
                        [ text chars.nbsp
                        , text "$"
                        ]
                    ]
                , tr []
                    [ th [] [ b "Dollars/oz:" ]
                    , td [ style "text-align" "right" ]
                        [ input
                            [ type_ "text"
                            , size pINPUT_SIZE
                            , style "text-align" "right"
                            , onInput InputDollarsPerOz
                            , onFocus (AfterFocus "dollarsPerOz")
                            , id "dollarsPerOz"
                            , value model.dollarsPerOzInput
                            ]
                            []
                        ]
                    , td [ style "text-align" "left" ]
                        [ text chars.nbsp
                        , text "$/oz"
                        ]
                    ]
                , tr []
                    [ th [] [ b "Dimes:" ]
                    , td [ style "text-align" "right" ]
                        [ truncate (10.0 * dimes)
                            |> toFloat
                            |> (\x -> x / 10.0)
                            |> String.fromFloat
                            |> addPointZero
                            |> text
                        ]
                    , td [ style "text-align" "left" ]
                        [ text chars.nbsp
                        , text "0.7734/10 oz/dime"
                        ]
                    ]
                ]
            , p []
                [ text "Scroll down the iframe below to see the current silver price (from Kitco)."
                , br
                , text "Copy that to the \"Dollars/oz\" input."
                ]
            , iframe
                [ src "https://www.kitco.com/price/precious-metals"
                , width 400
                , height 400
                ]
                []
            , p []
                [ a
                    [ href "#"
                    , onClick ReloadFromServer
                    ]
                    [ text "Reload from Server" ]
                ]
            , p []
                [ text chars.copyright
                , text "Copyright 2026, Bill St. Clair"
                , br
                , a [ href "https://github.com/billstclair/priceindimes" ]
                    [ text "GitHub" ]
                ]
            ]
        ]
    }


codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }


addPointZero : String -> String
addPointZero string =
    if String.contains "." string then
        string

    else
        string ++ ".0"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model |> withNoCmd

        AfterFocus id ->
            model |> withCmd (selectAll <| Debug.log "SelectAll" id)

        InputPrice string ->
            let
                m =
                    { model | priceInput = Debug.log "InputPrice" string }
            in
            case String.toFloat string of
                Just price ->
                    { m | price = price }
                        |> withNoCmd

                Nothing ->
                    m |> withNoCmd

        InputDollarsPerOz string ->
            let
                m =
                    { model | dollarsPerOzInput = Debug.log "InputDollarsPerOz" string }
            in
            case String.toFloat string of
                Just dollarsPerOz ->
                    { m | dollarsPerOz = dollarsPerOz }
                        |> withNoCmd

                Nothing ->
                    m |> withNoCmd

        OnUrlChange url ->
            model |> withNoCmd

        OnUrlRequest urlRequest ->
            case urlRequest of
                External url ->
                    model |> withCmd (openWindow <| JE.string url)

                Internal url ->
                    model |> withNoCmd

        ReloadFromServer ->
            model |> withCmd Navigation.reloadAndSkipCache
