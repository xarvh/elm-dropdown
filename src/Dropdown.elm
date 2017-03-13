module Dropdown
    exposing
        ( Config
        , newConfig
        , withDownArrow
        , withClearButton
        , withPrompt
        , withItemToLabel
        , Model
        , init
        , Msg
        , Outcome(..)
        , update
        , view
        )

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode


-- Style


namespace s =
    "ElmDropdown-" ++ s


cssRoot =
    namespace "Root"


cssPrompt =
    namespace "Prompt"


cssCurrentSelection =
    namespace "CurrentSelection"


cssDownArrow =
    namespace "DownArrow"


cssMenu =
    namespace "Menu"


cssDropdownItem =
    namespace "Item"


cssDropdownSelected =
    namespace "Selected"



-- Config


type alias Config item msg =
    { downArrow : Html msg
    , itemToLabel : item -> String
    , maybeClearButton : Maybe (Html msg)
    , prompt : String
    , onPickItem : item -> msg
    , onPickNone : msg
    , onToggleMenu : msg
    }


type alias State item =
    { items : List item
    , selection : Maybe item
    , isOpen : Bool
    }


{-
newConfig : (item -> String) -> Config item
newConfig itemToLabel =
    Config <| defaultConfig itemToLabel


withDownArrow : Html (Msg item) -> Config item -> Config item
withDownArrow downArrow (Config privateConfig) =
    Config { privateConfig | downArrow = downArrow }


withClearButton : Maybe (Html (Msg item)) -> Config item -> Config item
withClearButton maybeClearButton (Config privateConfig) =
    Config { privateConfig | maybeClearButton = maybeClearButton }


withPrompt : String -> Config item -> Config item
withPrompt prompt (Config privateConfig) =
    Config { privateConfig | prompt = prompt }


withItemToLabel : (item -> String) -> Config item -> Config item
withItemToLabel itemToLabel (Config privateConfig) =
    Config { privateConfig | itemToLabel = itemToLabel }



-- defaults


defaultConfig : (item -> String) -> PrivateConfig item
defaultConfig itemToLabel =
    { downArrow =
        defaultDownArrow
        -- TODO: provide a default clear button
    , maybeClearButton = Nothing
    , itemToLabel = itemToLabel
    , prompt = ""
    }
-}


defaultDownArrow : Html (Msg item)
defaultDownArrow =
    -- TODO use an actual icon
    span [] [ text "V" ]



-- Update





-- View


keyCodeToItemMsg item key =
    case key of
        -- Enter
        13 ->
            OnClickItem item

        -- ESC
        27 ->
            OnCurrentSelectionLosesFocus

        _ ->
            NoOp


viewItem : Config item -> Maybe item -> item -> Html (Msg item)
viewItem config maybeSelectedItem (prevItem, item, nextItem) =
    let
        isSelected =
            -- TODO: for safety, comparison should be made using itemToLabel?
            Just item == maybeSelectedItem

        classes =
            Html.Attributes.classList
                [ ( cssDropdownItem, True )
                , ( cssDropdownSelected, isSelected )
                ]
    in
        div
            [ classes
--             , Html.Attributes.tabindex -1
              --             , Html.Events.on "blur" <| Json.Decode.succeed OnItemLosesFocus
            , Html.Events.on "keyup" <| Json.Decode.map (keyCodeToItemMsg item) Html.Events.keyCode
            , Html.Events.onClick (OnClickItem item)
            ]
            [ text (privateConfig.itemToLabel item)
            ]


viewSelection privateConfig maybeSelectedItem =
    let
        onClick =
            Html.Events.onClick OnClickCurrentSelection

        currentSelection =
            case maybeSelectedItem of
                Nothing ->
                    span [ class cssPrompt, onClick ] [ text privateConfig.prompt ]

                Just item ->
                    span [ class cssCurrentSelection, onClick ] [ text <| privateConfig.itemToLabel item ]

        clearIcon =
            case privateConfig.maybeClearButton of
                Just button ->
                    span
                        [ Html.Events.onClick OnClickClear ]
                        [ button ]

                Nothing ->
                    span [] []

        downArrow =
            span
                [ class cssDownArrow
                , Html.Events.onClick OnClickDownArrow
                ]
                [ privateConfig.downArrow ]
    in
        div
            []
            [ currentSelection, clearIcon, downArrow ]



viewMenu config state =
  let
      just =
        List.map Just state.items

      prevItems =
        Nothing :: just

      nextItems =
        List.drop 1 just ++ [ Nothing ]

      list =
        List.map3 (,,) prevItems state.items nextItems

      listItems =
         List.map (viewItem config state.maybeSelectedItem) list
  in
      ul [ class cssMenu ] listItems






view : Config item msg -> State item -> Html msg
view config state =
    div
        [ class cssRoot
        , Html.Attributes.tabindex 0
        , Html.Events.on "blur" <| Json.Decode.succeed OnCurrentSelectionLosesFocus
        ]
        [ viewSelection config state.maybeSelectedItem
        , ul
            [ class cssMenu ]
          <|
            if state.isOpen then
            else
                []
        ]
