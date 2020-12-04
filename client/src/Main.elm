module Main exposing (..)

import Basics
import Html exposing (Html, a, div, footer, h1, h2, header, text)
import Html.Attributes exposing (class, href, id)
import String


playerInfo : String -> Int -> String
playerInfo playerName gameNumber =
    playerName ++ " - Game #" ++ String.fromInt gameNumber


viewPlayerInfo : String -> Int -> Html msg
viewPlayerInfo playerName gameNumber =
    let
        playerInfoText =
            playerInfo playerName gameNumber
                |> String.toUpper
                |> text
    in
    h2
        [ id "info", class "classy" ]
        [ playerInfoText ]


viewHeader : String -> Html msg
viewHeader title =
    header [] [ h1 [] [ text title ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org/" ] [ text "Powered by Elm" ] ]


viewMain : Html msg
viewMain =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayerInfo "Spyros" 3
        , viewFooter
        ]


main : Html msg
main =
    viewMain
