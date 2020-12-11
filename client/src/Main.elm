module Main exposing (..)

import Basics
import Browser exposing (element)
import Html exposing (Html, a, button, div, footer, h1, h2, header, li, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, href, id)
import Html.Events exposing (onClick)
import Http exposing (Error(..), expectJson, get, jsonBody, post)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (Value)
import List
import String



-- MODEL:


type alias Model =
    { playerName : String, gameNumber : Int, entries : List Entry, alertMessage : Maybe String }


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


type alias GameScore =
    { id : Int, name : String, points : Int }


initialModel : Model
initialModel =
    { playerName = "Spyros"
    , gameNumber = 1
    , entries = initialEntries
    , alertMessage = Nothing
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


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus code ->
            case code of
                500 ->
                    "The server had a problem, try again later"

                400 ->
                    "Verify your information and try again"

                401 ->
                    "Unauthorized"

                404 ->
                    "Not found"

                _ ->
                    "Unknown error"

        BadBody alertMessage ->
            alertMessage



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


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage maybeAlertMessage =
    case maybeAlertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick CloseErrorModal ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""


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
        , viewAlertMessage model.alertMessage
        , viewEntryList model.entries
        , viewScore (sumPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick ShareScore, disabled shareScoreDisabled ] [ text "Share Score" ]
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
