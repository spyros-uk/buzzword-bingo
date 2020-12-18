module HttpErroHandlers exposing (..)

import Http exposing (Error(..), expectJson, get, jsonBody, post)


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
