--------------------------------------------------------------
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
import Iso8601
import Json.Encode as JE exposing (Value)
import Task exposing (Task)
import Time exposing (Posix)
import Url exposing (Url)


port selectAll : String -> Cmd msg


port openWindow : Value -> Cmd msg


main =
    Browser.application
        { init = init
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 RecordTime


roundToDec : Int -> Float -> String
roundToDec dec number =
    let
        mult =
            10 ^ dec |> toFloat

        shifted =
            round (number * mult) |> toFloat |> String.fromFloat

        len =
            String.length shifted

        adjusted =
            if len > dec then
                shifted

            else
                String.repeat (dec - len + 1) "0" ++ shifted
    in
    String.dropRight dec adjusted ++ "." ++ String.right dec adjusted


type alias Model =
    { price : Float
    , priceInput : String
    , dollarsPerOz : Float
    , dollarsPerOzInput : String
    , dimes : Float
    , dimesInput : String
    , valid : Bool
    , validTime : Posix
    , now : Posix
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
    | InputDimes String
    | SetValidTime Posix
    | RecordTime Posix


truncateToDec : Int -> (Float -> Int) -> Float -> Float
truncateToDec dec rounder number =
    let
        mul =
            10 ^ dec |> toFloat
    in
    (number
        * mul
        |> rounder
        |> toFloat
    )
        / mul


priceToDimes : Float -> Float -> Float
priceToDimes price dollarsPerOz =
    (price
        / dollarsPerOz
        / (0.7734 / 10)
    )
        |> truncateToDec 1 round


dimesToPrice : Float -> Float -> Float
dimesToPrice dimes dollarsPerOz =
    (dimes
        * (0.7734 / 10)
        * dollarsPerOz
    )
        |> truncateToDec 2 round


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        price =
            1.0

        dollarsPerOz =
            1.0 / 0.7734

        dimes =
            priceToDimes price dollarsPerOz
    in
    { price = price
    , priceInput = roundToDec 2 price
    , dollarsPerOz = dollarsPerOz
    , dollarsPerOzInput = roundToDec 2 dollarsPerOz
    , dimes = dimes
    , dimesInput = roundToDec 1 dimes
    , valid = False
    , validTime = Time.millisToPosix 0
    , now = Time.millisToPosix 0
    , url = url
    , key = key
    }
        |> withCmd (Task.perform RecordTime Time.now)


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
            , table []
                [ tr []
                    [ td [] [ b "Price:" ]
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
                    [ td
                        (if not model.valid then
                            [ style "color" "red" ]

                         else
                            []
                        )
                        [ b "Dollars/oz:" ]
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
                , let
                    sinceLastSet =
                        Time.posixToMillis model.now
                            - Time.posixToMillis model.validTime

                    minutes =
                        sinceLastSet // (1000 * 60)
                  in
                  tr []
                    [ td []
                        [ b "Last set: " ]
                    , td [ style "text-align" "right" ]
                        [ if minutes >= 5 then
                            text "> 5"

                          else
                            span []
                                [ text <| String.fromInt minutes ]
                        ]
                    , td [ style "text-align" "left" ]
                        [ if minutes == 1 then
                            text " minute ago"

                          else
                            text " minutes ago"
                        ]
                    ]
                , tr []
                    [ td [] [ b "Dimes:" ]
                    , td [ style "text-align" "right" ]
                        [ input
                            [ type_ "text"
                            , size pINPUT_SIZE
                            , style "text-align" "right"
                            , onInput InputDimes
                            , onFocus (AfterFocus "dimes")
                            , id "dimes"
                            , value <| model.dimesInput
                            ]
                            []
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
                , br
                , text "Set \"Price\" to calculate \"Dimes\"."
                , br
                , text "Set \"Dimes\" to calcualte \"Price\"."
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


addZero : String -> String
addZero string =
    if string == "" then
        "0"

    else if String.left 1 string == "0" then
        addZero <| String.dropLeft 1 string

    else
        string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model |> withNoCmd

        AfterFocus id ->
            model |> withCmd (selectAll <| Debug.log "SelectAll" id)

        InputPrice string ->
            let
                priceString =
                    addZero string
            in
            case String.toFloat priceString of
                Nothing ->
                    { model | priceInput = string } |> withNoCmd

                Just price ->
                    let
                        roundedPrice =
                            truncateToDec 2 round price

                        dimes =
                            priceToDimes roundedPrice model.dollarsPerOz
                    in
                    { model
                        | priceInput = priceString
                        , price = roundedPrice
                        , dimes = dimes
                        , dimesInput = roundToDec 1 dimes
                    }
                        |> withNoCmd

        InputDollarsPerOz string ->
            let
                priceString =
                    addZero string
            in
            case String.toFloat priceString of
                Nothing ->
                    { model | dollarsPerOzInput = string } |> withNoCmd

                Just dollarsPerOz ->
                    let
                        roundedDollarsPerOz =
                            truncateToDec 2 round dollarsPerOz

                        dimes =
                            priceToDimes model.price roundedDollarsPerOz
                    in
                    { model
                        | dollarsPerOzInput = string
                        , dollarsPerOz = roundedDollarsPerOz
                        , dimes = dimes
                        , dimesInput = roundToDec 1 dimes
                        , valid = True
                    }
                        |> withCmd (Task.perform SetValidTime Time.now)

        InputDimes string ->
            let
                priceString =
                    addZero string
            in
            case String.toFloat priceString of
                Nothing ->
                    { model | dimesInput = string } |> withNoCmd

                Just dimes ->
                    let
                        roundedDimes =
                            truncateToDec 1 round dimes

                        price =
                            dimesToPrice roundedDimes model.dollarsPerOz
                    in
                    { model
                        | dimes = roundedDimes
                        , dimesInput = string
                        , price = price
                        , priceInput = roundToDec 2 price
                        , valid = True
                    }
                        |> withCmd (Task.perform SetValidTime Time.now)

        SetValidTime posix ->
            { model | validTime = posix }
                |> withNoCmd

        RecordTime posix ->
            let
                now =
                    Time.posixToMillis posix

                passed =
                    Time.posixToMillis posix - Time.posixToMillis model.validTime

                minutesPassed =
                    passed // (1000 * 60)
            in
            { model
                | now = posix
                , valid = minutesPassed < 5
            }
                |> withNoCmd

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
