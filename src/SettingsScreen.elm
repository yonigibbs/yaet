module SettingsScreen exposing (Model, Msg, UpdateResult(..), init, subscriptions, update, view)

import Browser.Events
import Element exposing (Element)
import Element.Border
import Element.Font
import Element.Input
import Game
import Json.Decode as JD
import Modal
import Ports
import Settings exposing (EditableSettings, Settings)
import UIHelpers exposing (edges)



-- MODEL


type Model
    = Model ModelData


type Screen
    = SettingsScreen
    | KeySelectionScreen { action : Game.UserAction, key : Maybe String }


type alias ModelData =
    { editableSettings : EditableSettings, screen : Screen, settingsToPersist : Maybe Settings }


init : Settings -> Model
init settings =
    Model { editableSettings = Settings.toEditable settings, screen = SettingsScreen, settingsToPersist = Just settings }



-- UPDATE


type Msg
    = RestoreDefaultSettingsRequested
    | SaveRequested
    | CancelRequested
    | KeySelectionScreenRequested Game.UserAction
    | KeySelected String


type UpdateResult
    = KeepOpen
    | Close (Maybe Settings)


update : Msg -> Model -> ( Model, Cmd Msg, UpdateResult )
update msg ((Model ({ editableSettings, screen, settingsToPersist } as modelData)) as model) =
    let
        ignore =
            ( model, Cmd.none, KeepOpen )
    in
    case ( msg, screen ) of
        ( RestoreDefaultSettingsRequested, SettingsScreen ) ->
            ( Model
                { modelData
                    | editableSettings = Settings.toEditable Settings.default
                    , settingsToPersist = Just Settings.default
                }
            , Cmd.none
            , KeepOpen
            )

        ( RestoreDefaultSettingsRequested, KeySelectionScreen _ ) ->
            ignore

        ( SaveRequested, SettingsScreen ) ->
            case settingsToPersist of
                Just validSettings ->
                    ( model, Settings.toJson validSettings |> Ports.persistSettings, Close <| Just validSettings )

                Nothing ->
                    -- Should never happen - UI shouldn't let user submit the dialog if it's not valid (e.g. some key
                    -- bindings not yet set).
                    ignore

        ( SaveRequested, KeySelectionScreen { action, key } ) ->
            let
                ( newEditableSettings, newSettingsToPersist ) =
                    case key of
                        Just key_ ->
                            Settings.withKeyBinding action key_ editableSettings
                                |> (\newEditableSettings_ -> ( newEditableSettings_, Settings.fromEditable newEditableSettings_ ))

                        Nothing ->
                            ( modelData.editableSettings, modelData.settingsToPersist )
            in
            ( Model
                { modelData
                    | editableSettings = newEditableSettings
                    , screen = SettingsScreen
                    , settingsToPersist = newSettingsToPersist
                }
            , Cmd.none
            , KeepOpen
            )

        ( CancelRequested, SettingsScreen ) ->
            ( model, Cmd.none, Close Nothing )

        ( CancelRequested, KeySelectionScreen _ ) ->
            ( Model { modelData | screen = SettingsScreen }, Cmd.none, KeepOpen )

        ( KeySelectionScreenRequested action, SettingsScreen ) ->
            ( Model { modelData | screen = KeySelectionScreen { action = action, key = Settings.keyBinding action editableSettings } }
            , Cmd.none
            , KeepOpen
            )

        ( KeySelectionScreenRequested _, KeySelectionScreen _ ) ->
            ignore

        ( KeySelected key, KeySelectionScreen keySelectionScreen ) ->
            ( Model { modelData | screen = KeySelectionScreen { keySelectionScreen | key = Just key } }
            , Cmd.none
            , KeepOpen
            )

        ( KeySelected _, _ ) ->
            ignore



-- VIEW


view : Model -> Element Msg
view (Model modelData) =
    case modelData.screen of
        SettingsScreen ->
            settingsView modelData

        KeySelectionScreen { action, key } ->
            keySelectionView action key


settingsView : ModelData -> Element Msg
settingsView ({ editableSettings, settingsToPersist } as modelData) =
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el
            [ Element.centerX, Element.Font.bold, Element.Font.size 24, Element.paddingEach { edges | bottom = 15 } ]
          <|
            Element.text "Settings"
        , keyBindingsTable editableSettings
        ]
        |> Element.el []
        |> Modal.dialog (settingsScreenModalConfig modelData)


settingsScreenModalConfig : { a | settingsToPersist : Maybe Settings } -> Modal.Config Msg
settingsScreenModalConfig { settingsToPersist } =
    Modal.defaultConfig CancelRequested (Maybe.map (always SaveRequested) settingsToPersist)
        |> Modal.withCustomButton "Restore Defaults" (Just RestoreDefaultSettingsRequested)


keySelectionView : Game.UserAction -> Maybe String -> Element Msg
keySelectionView action key =
    let
        caption =
            "Press the key to use to " ++ (Game.userActionDescription action |> String.toLower)

        ( keyDescr, colour ) =
            keyDescriptionAndColour key
    in
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el [ Element.centerX, Element.paddingEach { edges | bottom = 15 }, Element.Font.semiBold, Element.Font.size 16 ] <|
            Element.text caption
        , Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24, Element.Font.color colour ] <| Element.text keyDescr
        ]
        |> Element.el []
        |> Modal.dialog (keySelectionScreenModalConfig key)


keySelectionScreenModalConfig : Maybe String -> Modal.Config Msg
keySelectionScreenModalConfig key =
    Modal.defaultConfig CancelRequested (Maybe.map (always SaveRequested) key)


keyBindingsTable : EditableSettings -> Element Msg
keyBindingsTable settings =
    let
        keyBindings =
            Settings.allKeyBindings settings

        column caption contents =
            { header = Element.el [ Element.Font.size 16, Element.Font.bold, Element.paddingXY 0 4 ] <| Element.text caption
            , width = Element.shrink
            , view = \record -> Element.el [ Element.Font.size 14, Element.Font.semiBold ] <| contents record
            }
    in
    Element.table [ Element.spacingXY 25 5 ]
        { data = keyBindings
        , columns =
            [ column "Action" <|
                \{ action } -> Game.userActionDescription action |> Element.text
            , column "Key" <|
                \{ action, key } ->
                    let
                        ( caption, colour ) =
                            keyDescriptionAndColour key
                    in
                    Element.Input.button
                        [ Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Element.Border.color UIHelpers.mainBackgroundColour
                        , Element.mouseOver [ Element.Border.color UIHelpers.mainForegroundColour ]
                        , Element.Font.color colour
                        ]
                        { onPress = Just <| KeySelectionScreenRequested action, label = Element.text caption }
            ]
        }


keyDescriptionAndColour : Maybe String -> ( String, Element.Color )
keyDescriptionAndColour maybeKey =
    case maybeKey of
        Just key ->
            ( keyDescription key, UIHelpers.mainForegroundColour )

        Nothing ->
            ( "<not set>", Element.rgb255 200 0 0 )


keyDescription : String -> String
keyDescription key =
    case String.toUpper key of
        " " ->
            "Space"

        "ARROWLEFT" ->
            "Left arrow"

        "ARROWRIGHT" ->
            "Right arrow"

        "ARROWDOWN" ->
            "Down arrow"

        "ARROWUP" ->
            "Up arrow"

        upperKey ->
            upperKey


allowedKeys : List String
allowedKeys =
    -- ASCII 33 (exclamation mark) up to 126 (~) are all valid keys, as are the four arrows.
    (List.range 33 126 |> List.map (Char.fromCode >> String.fromChar))
        ++ [ " ", "ArrowLeft", "ArrowRight", "ArrowDown", "ArrowUp" ]


keyBindingDecoder : JD.Decoder Msg
keyBindingDecoder =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if List.member key allowedKeys then
                    Settings.sanitiseKey key |> KeySelected |> JD.succeed

                else
                    JD.fail ""
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model modelData) =
    case modelData.screen of
        SettingsScreen ->
            settingsScreenModalConfig modelData |> Modal.subscriptions

        KeySelectionScreen { key } ->
            Sub.batch
                [ Browser.Events.onKeyDown keyBindingDecoder
                , keySelectionScreenModalConfig key |> Modal.subscriptions
                ]
