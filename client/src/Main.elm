module Main exposing (..)

import Basics
import Html
import String


playerInfo playerName gameNumber =
    playerName ++ " - Game #" ++ String.fromInt gameNumber


playerInfoText playerName gameNumber =
    playerInfo playerName gameNumber
        |> String.toUpper
        |> Html.text


main =
    playerInfoText "Spyros" 3
