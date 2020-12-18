module View.Buttons exposing (primaryButton)

import Html exposing (Html, button, text)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events exposing (onClick)


primaryButton : msg -> Bool -> String -> Html msg
primaryButton onClickCommand isDisabled buttonText =
    button [ type_ "button", class "primary", onClick onClickCommand, disabled isDisabled ] [ text buttonText ]
