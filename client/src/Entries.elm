module Entries exposing (Entry, viewEntryList)

import Html exposing (Html, li, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


viewEntryItem : Entry -> (Int -> msg) -> Html msg
viewEntryItem entry msg =
    li [ classList [ ( "marked", entry.marked ) ], onClick (msg entry.id) ]
        [ span [ class "phrase" ]
            [ text entry.phrase ]
        , span [ class "points" ]
            [ text
                (String.fromInt entry.points)
            ]
        ]


viewEntryList : List Entry -> (Int -> msg) -> Html msg
viewEntryList entries msg =
    entries
        |> List.map (\entry -> viewEntryItem entry msg)
        |> ul []
