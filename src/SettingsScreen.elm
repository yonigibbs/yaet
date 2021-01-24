module SettingsScreen exposing (Model, Msg, UpdateResult(..), init, update, view)

import Element exposing (Element)
import Element.Border
import Element.Font
import Element.Input
import Game
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
    | Cancelled
    | KeySelectionScreenRequested Game.UserAction


type UpdateResult
    = KeepOpen
    | Close (Maybe Settings)


update : Msg -> Model -> ( Model, Cmd Msg, UpdateResult )
update msg ((Model ({ editableSettings, screen } as modelData)) as model) =
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
            -- TODO: implement saving to local storage, and converting the EditableSettings back to Settings
            ( model, Cmd.none, Close Nothing )

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

        ( Cancelled, SettingsScreen ) ->
            ( model, Cmd.none, Close Nothing )

        ( Cancelled, KeySelectionScreen _ ) ->
            ( Model { modelData | screen = SettingsScreen }, Cmd.none, KeepOpen )

        ( KeySelectionScreenRequested action, SettingsScreen ) ->
            ( Model { modelData | screen = KeySelectionScreen { action = action, key = Settings.keyBinding action editableSettings } }
            , Cmd.none
            , KeepOpen
            )

        ( KeySelectionScreenRequested _, KeySelectionScreen _ ) ->
            ignore



-- VIEW


view : Model -> Element Msg
view ((Model { editableSettings, screen, settingsToPersist }) as model) =
    case screen of
        SettingsScreen ->
            settingsView editableSettings settingsToPersist

        KeySelectionScreen { action, key } ->
            keySelectionView action key


settingsView : EditableSettings -> Maybe Settings -> Element Msg
settingsView editableSettings settingsToPersist =
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el
            [ Element.centerX, Element.Font.bold, Element.Font.size 24, Element.paddingEach { edges | bottom = 15 } ]
          <|
            Element.text "Settings"
        , keyBindingsTable editableSettings
        ]
        |> Element.el []
        |> UIHelpers.showModal
            { onSubmit = Maybe.map (always SaveRequested) settingsToPersist
            , onCancel = Cancelled
            , custom = [ ( "Restore Defaults", RestoreDefaultSettingsRequested ) ]
            }


keySelectionView : Game.UserAction -> Maybe String -> Element Msg
keySelectionView action key =
    let
        caption =
            "Press the key to use to " ++ (Game.userActionDescription action |> String.toLower)
    in
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el [ Element.centerX, Element.paddingEach { edges | bottom = 15 }, Element.Font.semiBold, Element.Font.size 16 ] <|
            Element.text caption
        , Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 22 ] <| Element.text (keyDescription key)
        ]
        |> Element.el []
        |> UIHelpers.showModal { onSubmit = Maybe.map (always SaveRequested) key, onCancel = Cancelled, custom = [] }


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
            [ column "Action" <| \{ action } -> Game.userActionDescription action |> Element.text
            , column "Key" <|
                \{ action, key } ->
                    -- TODO: add underlines on hover
                    Element.Input.button
                        [ Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Element.Border.color UIHelpers.mainBackgroundColour
                        , Element.mouseOver [ Element.Border.color UIHelpers.mainForegroundColour ]
                        , Element.focused []
                        ]
                        { onPress = Just <| KeySelectionScreenRequested action, label = Element.text <| keyDescription key }
            ]
        }


keyDescription : Maybe String -> String
keyDescription key =
    case key of
        Nothing ->
            "<not set>"

        Just " " ->
            "Space"

        Just "arrowleft" ->
            "Left arrow"

        Just "arrowright" ->
            "Right arrow"

        Just "arrowdown" ->
            "Down arrow"

        Just "arrowup" ->
            "Up arrow"

        Just k ->
            String.toUpper k
