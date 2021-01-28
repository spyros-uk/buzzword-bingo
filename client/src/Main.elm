module Main exposing (..)

import Basics
import Browser exposing (element)
import Entries exposing (Entry, viewEntryList)
import Html exposing (Html, a, div, footer, h1, h2, header, input, li, span, text, ul)
import Html.Attributes exposing (autofocus, class, classList, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..), expectJson, get, jsonBody, post)
import HttpErroHandlers exposing (errorToString)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (Value)
import List
import String
import View.AlertMessage exposing (viewAlertMessage)
import View.Buttons exposing (primaryButton)



-- MODEL:


type GameState
    = EnteringName
    | Playing


type alias Model =
    { playerName : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


type alias GameScore =
    { id : Int, name : String, points : Int }


initialModel : Model
initialModel =
    { playerName = "Anonymous"
    , gameNumber = 1
    , entries = initialEntries
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
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
    | CloseErrorModal
    | ShareScore
    | NewScore (Result Error GameScore)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


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
                    ( { model | alertMessage = Just (errorToString error) }, Cmd.none )

        ShareScore ->
            ( model, shareGameScore model )

        NewScore response ->
            case response of
                Ok score ->
                    ( { model | alertMessage = Just ("Your score: " ++ String.fromInt score.points ++ " was successfully shared!") }, Cmd.none )

                Err error ->
                    ( { model | alertMessage = Just ("Error posting your score: " ++ errorToString error) }, Cmd.none )

        CloseErrorModal ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            let
                updateMark entry =
                    if entry.id == id then
                        { entry | marked = not entry.marked }

                    else
                        entry
            in
            ( { model | entries = List.map updateMark model.entries }, Cmd.none )

        SetNameInput nameOfPlayer ->
            ( { model | nameInput = nameOfPlayer }, Cmd.none )

        SaveName ->
            ( { model
                | playerName = model.nameInput
                , nameInput = ""
                , gameState = Playing
              }
            , Cmd.none
            )

        CancelName ->
            ( { model | nameInput = "", gameState = Playing }, Cmd.none )

        ChangeGameState gameState ->
            ( { model | gameState = gameState }, Cmd.none )



-- ENCODERS/DECODERS:


scoreEncoder : Model -> Value
scoreEncoder model =
    Encode.object
        [ ( "name", Encode.string model.playerName )
        , ( "points", Encode.int (sumPoints model.entries) )
        ]


scoreDecoder : Decoder GameScore
scoreDecoder =
    Decode.map3 GameScore
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "points" Decode.int)


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)



-- COMMANDS:


serverUrl : String -> String
serverUrl resource =
    "http://localhost:3000/" ++ resource


getEntries : Cmd Msg
getEntries =
    get { url = serverUrl "random-entries", expect = expectJson NewEntry (Decode.list entryDecoder) }


shareGameScore : Model -> Cmd Msg
shareGameScore model =
    let
        scoreUrl =
            serverUrl "scores"

        body =
            scoreEncoder model
                |> jsonBody
    in
    post
        { url = scoreUrl
        , body = body
        , expect = expectJson NewScore scoreDecoder
        }



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


viewPlayerInfo : String -> Int -> Html Msg
viewPlayerInfo playerName gameNumber =
    h2
        [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ] [ text playerName ]
        , text (" - Game #" ++ String.fromInt gameNumber)
        ]


viewHeader : String -> Html msg
viewHeader title =
    header [] [ h1 [] [ text title ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org/" ] [ text "Powered by Elm" ] ]


viewPlayerName : Model -> Html Msg
viewPlayerName model =
    let
        isSaveNameDisabled =
            String.isEmpty model.nameInput
    in
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input [ type_ "text", placeholder "Who's playing", autofocus True, onInput SetNameInput, value model.nameInput ] []
                , primaryButton SaveName isSaveNameDisabled "Save"
                , primaryButton CancelName False "Cancel"
                ]

        Playing ->
            text ""


viewMain : Model -> Html Msg
viewMain model =
    let
        shareScoreDisabled =
            if sumPoints model.entries > 0 then
                False

            else
                True
    in
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayerInfo model.playerName model.gameNumber

        --, div [ class "debug" ] [ text (Debug.toString model) ]
        , viewAlertMessage model.alertMessage CloseErrorModal
        , viewPlayerName model
        , viewEntryList model.entries Mark
        , viewScore (sumPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame False "New Game"
            , primaryButton ShareScore shareScoreDisabled "Share Score"
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
