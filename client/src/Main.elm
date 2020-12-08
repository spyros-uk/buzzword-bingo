module Main exposing (..)

import Basics
import Browser exposing (element)
import Html exposing (Html, a, button, div, footer, h1, h2, header, li, span, text, ul)
import Html.Attributes exposing (class, classList, href, id)
import Html.Events exposing (onClick)
import Http exposing (Error, expectJson, get)
import Json.Decode as Decode exposing (Decoder, field, succeed)
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
    []



-- UPDATE:


type Msg
    = NewGame
    | Mark Int
    | NewRandomNumber Int
    | NewEntry (Result Error (List Entry))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomNumber number ->
            ( { model | gameNumber = number }, Cmd.none )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntry result ->
            case result of
                Ok randomEntries ->
                    let
                        _ =
                            Debug.log "It works!" randomEntries
                    in
                    ( { model | entries = randomEntries }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "Oops!" error
                    in
                    ( model, Cmd.none )

        Mark id ->
            let
                updateMark entry =
                    if entry.id == id then
                        { entry | marked = not entry.marked }

                    else
                        entry
            in
            ( { model | entries = List.map updateMark model.entries }, Cmd.none )



-- DECODERS:


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)



-- COMMANDS:


entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


getEntries : Cmd Msg
getEntries =
    get { url = entriesUrl, expect = expectJson NewEntry (Decode.list entryDecoder) }



-- VIEW:


sumPoints : List Entry -> Int
sumPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum


viewScore : Int -> Html Msg
viewScore score =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (String.fromInt score) ]
        ]


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
        , viewScore (sumPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            ]
        , viewFooter
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( initialModel, getEntries )
        , view = viewMain
        , update = update
        , subscriptions = \_ -> Sub.none
        }
