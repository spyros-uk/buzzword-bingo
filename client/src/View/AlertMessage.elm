module View.AlertMessage exposing (viewAlertMessage)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


viewAlertMessage : Maybe String -> msg -> Html msg
viewAlertMessage maybeAlertMessage onCloseCommand =
    case maybeAlertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick onCloseCommand ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""
