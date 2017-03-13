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


type alias Model =
    { leftSelection : String
    , leftDropdown : Dropdown.Model
    , rightSelection : Maybe String
    , rightDropdown : Dropdown.Model
    }


type Msg
    = NoOp
    | LeftMsg (Dropdown.Msg String)
    | RightMsg (Dropdown.Msg String)



-- init


init =
    { leftSelection = "xx"
    , leftDropdown = Dropdown.init
    , rightSelection = Nothing
    , rightDropdown = Dropdown.init
    }



-- update


update msg model =
    case msg of
        NoOp ->
            model

        LeftMsg msg ->
            let
                ( leftDropdown, outcome ) =
                    Dropdown.update config msg model.leftDropdown

                leftSelection =
                    case outcome of
                        Dropdown.ItemSelected item ->
                            item

                        _ ->
                            model.leftSelection
            in
                { model | leftDropdown = leftDropdown, leftSelection = leftSelection }

        RightMsg msg ->
            let
                ( rightDropdown, outcome ) =
                    Dropdown.update config msg model.rightDropdown

                rightSelection =
                    case outcome of
                        Dropdown.ItemSelected item ->
                            Just item

                        Dropdown.SelectionCleared ->
                            Nothing

                        Dropdown.NoChange ->
                            model.rightSelection
            in
                { model | rightDropdown = rightDropdown, rightSelection = rightSelection }



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
