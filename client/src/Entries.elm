module Entries exposing (Entry, updateEntries, viewEntryList)

import Html exposing (Html, li, span, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


updateEntries : List Entry -> Int -> List Entry
updateEntries entries id =
    let
        updateMarked entry =
            if entry.id == id then
                { entry | marked = not entry.marked }

            else
                entry
    in
    List.map updateMarked entries


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
