module Main exposing (..)

import Dropdown
import Html exposing (..)
import Html.Attributes exposing (class)


items =
    [ "aa"
    , "bb"
    , "cd"
    , "efg"
    ]


config =
    Dropdown.newConfig identity


type DropdownId
    = Left
    | Right


type alias Model =
    { openDropdownId : Maybe DropdownId
    , leftSelection : String
    , rightSelection : Maybe String
    }


type Msg
    = NoOp
    | SelectionClicked DropdownId
    | SelectLeft (Maybe String)
    | SelectRight (Maybe String)



-- init


init =
    { openDropdownId = Nothing
    , leftSelection = "cd"
    , rightSelection = Nothing
    }



-- update


update msg model =
    case msg of
        NoOp ->
            model

        SelectionClicked dropdownId ->
            if Just dropdownId == model.openDropdownId then
                { model | openDropdownId = Nothing }
            else
                { model | openDropdownId = Just dropdownId }

        SelectLeft maybeString ->
            case maybeString of
                Nothing ->
                    model

                Just string ->
                    { model | leftSelection = string }

        SelectRight maybeString ->
            { model | rightSelection = maybeString }



-- view


view model =
    div
        [ class "root" ]
        [ text "LOOOL"
        , node "style" [] [ text """

          .root {
            background-color: green;
            display: flex;
          }

          .left .ElmDropdown-Root {
            background-color: red;
          }

          .left .ElmDropdown-DownArrow {
            background-color: purple;
          }

          .right .ElmDropdown-Root {
            background-color: blue;
          }

          """ ]
        , div
            [ class "left" ]
            [ Html.map LeftMsg <| Dropdown.view config model.leftDropdown items (Just model.leftSelection) ]
        , div
            [ class "right" ]
            [ Html.map RightMsg <| Dropdown.view config model.rightDropdown items model.rightSelection ]
        ]



-- main


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
