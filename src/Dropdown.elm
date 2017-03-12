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
    , itemToLabel : item -> String
    , maybeClearButton : Maybe (Html (Msg item))
    , prompt : String
    }


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
    | OnClickItem item
    | OnClickClear
    | OnClickDownArrow
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

        OnClickClear ->
            ( Model False, SelectionCleared )

        OnClickDownArrow ->
            ( Model (not isOpen), NoChange )

        OnClickItem item ->
            ( Model False, ItemSelected item )

        OnLoseFocus ->
            ( Model False, NoChange )



-- View


keyCodeToItemMsg item key =
    case key of
        -- Enter
        13 ->
            OnClickItem item

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
            , Html.Events.onClick (OnClickItem item)
            ]
            [ text (privateConfig.itemToLabel item)
            ]


viewSelection privateConfig maybeSelectedItem =
    let
        currentSelection =
            case maybeSelectedItem of
                Nothing ->
                    span [ class cssPrompt ] [ text privateConfig.prompt ]

                Just item ->
                    span [ class cssCurrentSelection ] [ text <| privateConfig.itemToLabel item ]

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
                [ Html.Events.onClick OnClickDownArrow ]
                [ privateConfig.downArrow ]
    in
        div
            []
            [ currentSelection, clearIcon, downArrow ]


view : Config item -> Model -> List item -> Maybe item -> Html (Msg item)
view (Config privateConfig) (Model isOpen) items maybeSelectedItem =
    div
        [ class cssRoot
        , Html.Events.on "blur" <| Json.Decode.succeed OnLoseFocus
        ]
        [ viewSelection privateConfig maybeSelectedItem
        , div
            [ class cssMenu ]
          <|
            if isOpen then
                List.map (viewItem privateConfig maybeSelectedItem) items
            else
                []
        ]
