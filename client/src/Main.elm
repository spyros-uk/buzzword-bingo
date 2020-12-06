module Main exposing (..)

import Basics
import Html exposing (Html, a, div, footer, h1, h2, header, li, span, text, ul)
import Html.Attributes exposing (class, href, id)
import List
import String



-- MODEL:


type alias Model =
    { playerName : String, gameNumber : Int, entries : List Entry }


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


initialModel : Model
initialModel =
    { playerName = "Spyros"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 1 "Future-Proof" 100 False
    , Entry 2 "Doing Agile" 200 False
    , Entry 3 "In The Cloud" 300 False
    , Entry 4 "Rock-Star Ninja" 400 False
    ]



-- VIEW:


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


viewEntryItem : Entry -> Html msg
viewEntryItem entry =
    li []
        [ span [ class "phrase" ]
            [ text entry.phrase ]
        , span [ class "points" ]
            [ text
                (String.fromInt entry.points)
            ]
        ]


viewEntryList : List Entry -> Html msg
viewEntryList entries =
    entries
        |> List.map viewEntryItem
        |> ul []


viewMain : Model -> Html msg
viewMain model =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayerInfo model.playerName model.gameNumber

        --, div [ class "debug" ] [ text (Debug.toString initialModel) ]
        , viewEntryList model.entries
        , viewFooter
        ]


main : Html msg
main =
    viewMain initialModel
