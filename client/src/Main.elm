module Main exposing (..)

import Basics
import Browser exposing (sandbox)
import Html exposing (Html, a, button, div, footer, h1, h2, header, li, span, text, ul)
import Html.Attributes exposing (class, classList, href, id)
import Html.Events exposing (onClick)
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



-- UPDATE:


type Msg
    = NewGame
    | Mark Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            { model | gameNumber = model.gameNumber + 1, entries = initialEntries }

        Mark id ->
            let
                updateMark entry =
                    if entry.id == id then
                        { entry | marked = not entry.marked }

                    else
                        entry
            in
            { model | entries = List.map updateMark model.entries }



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


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ]
            [ text entry.phrase ]
        , span [ class "points" ]
            [ text
                (String.fromInt entry.points)
            ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    entries
        |> List.map viewEntryItem
        |> ul []


viewMain : Model -> Html Msg
viewMain model =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayerInfo model.playerName model.gameNumber

        --, div [ class "debug" ] [ text (Debug.toString model) ]
        , viewEntryList model.entries
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            ]
        , viewFooter
        ]


main : Program () Model Msg
main =
    sandbox
        { init = initialModel
        , view = viewMain
        , update = update
        }
