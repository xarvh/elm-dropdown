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


cssMenu =
    namespace "Menu"


cssDropdownItem =
    namespace "Item"


cssDropdownSelected =
    namespace "Selected"



-- Config


type Config item
    = Config (PrivateConfig item)


type alias PrivateConfig item =
    { downArrow : Html (Msg item)
    , hasClearButton : Bool
    , itemToLabel : item -> String
    , prompt : String
    }


newConfig : (item -> String) -> Config item
newConfig itemToLabel =
    Config <| defaultConfig itemToLabel


withDownArrow : Html (Msg item) -> Config item -> Config item
withDownArrow downArrow (Config privateConfig) =
    Config { privateConfig | downArrow = downArrow }


withClearButton : Bool -> Config item -> Config item
withClearButton hasClearButton (Config privateConfig) =
    Config { privateConfig | hasClearButton = hasClearButton }


withPrompt : String -> Config item -> Config item
withPrompt prompt (Config privateConfig) =
    Config { privateConfig | prompt = prompt }


withItemToLabel : (item -> String) -> Config item -> Config item
withItemToLabel itemToLabel (Config privateConfig) =
    Config { privateConfig | itemToLabel = itemToLabel }



-- defaults


defaultConfig : (item -> String) -> PrivateConfig item
defaultConfig itemToLabel =
    { downArrow = defaultDownArrow
    , hasClearButton = True
    , itemToLabel = itemToLabel
    , prompt = ""
    }


defaultDownArrow : Html (Msg item)
defaultDownArrow =
    -- TODO use an actual icon
    span [] [ text "V" ]



-- Model


type Model
    = Model Bool


init : Model
init =
    Model False



-- Update
{- TODO: it's a pain in the ass to have Msg depend on item, should just use a String -}


type Msg item
    = NoOp
    | OnSelect item
    | OnLoseFocus


type Outcome item
    = NoChange
    | SelectionCleared
    | ItemSelected item


update : Config item -> Msg item -> Model -> ( Model, Outcome item )
update (Config privateConfig) msg (Model isOpen) =
    case msg of
        NoOp ->
            ( Model isOpen, NoChange )

        OnSelect item ->
            ( Model False, ItemSelected item )

        OnLoseFocus ->
            ( Model False, NoChange )



-- View


keyCodeToItemMsg item key =
    case key of
        -- Enter
        13 ->
            OnSelect item

        -- ESC
        27 ->
            OnLoseFocus

        _ ->
            NoOp


viewItem : PrivateConfig item -> Maybe item -> item -> Html (Msg item)
viewItem privateConfig maybeSelectedItem item =
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
            , Html.Events.on "keyup" <| Json.Decode.map (keyCodeToItemMsg item) Html.Events.keyCode
            , Html.Events.on "blur" <| Json.Decode.succeed OnLoseFocus
            , Html.Events.onClick (OnSelect item)
            ]
            [ text (privateConfig.itemToLabel item)
            ]


view : Config item -> Model -> List item -> Maybe item -> Html (Msg item)
view (Config privateConfig) (Model isOpen) items maybeSelectedItem =
    div
        [ class cssRoot ]
        [ div [] []
          --Trigger.view config model items selected
        , div
            [ class cssMenu ]
          <|
            if isOpen then
                List.map (viewItem privateConfig maybeSelectedItem) items
            else
                []
        ]
