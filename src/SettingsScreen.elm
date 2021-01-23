module SettingsScreen exposing (Model, Msg, UpdateResult(..), init, update, view)

import Element exposing (Element)
import Element.Font
import Element.Input
import Game
import Settings exposing (Settings)
import UIHelpers exposing (edges)



-- MODEL


type Model
    = Model ModelData


type Screen
    = SettingsScreen
    | KeySelectionScreen { action : Game.UserAction, key : String }


type alias ModelData =
    { settings : Settings, screen : Screen }


init : Settings -> Model
init settings =
    Model { settings = settings, screen = SettingsScreen }



-- UPDATE


type Msg
    = DefaultSettingsRestored
    | SaveRequested
    | Cancelled
    | ChangeKeyBindingRequested Game.UserAction


type UpdateResult
    = KeepOpen
    | Close (Maybe Settings)


update : Msg -> Model -> ( Model, Cmd Msg, UpdateResult )
update msg ((Model ({ settings } as modelData)) as model) =
    case msg of
        DefaultSettingsRestored ->
            ( Model { modelData | settings = Settings.default }, Cmd.none, KeepOpen )

        SaveRequested ->
            -- TODO: implement saving to local storage
            ( model, Cmd.none, Close <| Just settings )

        Cancelled ->
            ( model, Cmd.none, Close Nothing )

        ChangeKeyBindingRequested action ->
            ( Model { modelData | screen = KeySelectionScreen { action = action, key = Settings.keyBinding settings action } }
            , Cmd.none
            , KeepOpen
            )



-- VIEW


view : Model -> Element Msg
view (Model { settings, screen }) =
    case screen of
        SettingsScreen ->
            settingsView settings

        KeySelectionScreen { action, key } ->
            keySelectionView action key


settingsView : Settings -> Element Msg
settingsView settings =
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el
            [ Element.centerX, Element.Font.bold, Element.Font.size 24, Element.paddingEach { edges | bottom = 15 } ]
          <|
            Element.text "Settings"
        , keyBindingsTable settings
        ]
        |> Element.el []
        |> UIHelpers.showModal
            { onSubmit = SaveRequested
            , onCancel = Cancelled
            , custom = [ ( "Restore Defaults", DefaultSettingsRestored ) ]
            }


keySelectionView : Game.UserAction -> String -> Element Msg
keySelectionView action key =
    let
        caption =
            "Press the key to use to " ++ (Game.userActionDescription action |> String.toLower)
    in
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el [ Element.centerX, Element.paddingEach { edges | bottom = 15 } ] <|
            Element.text caption
        ]
        |> Element.el []
        |> UIHelpers.showModal
            { onSubmit = SaveRequested
            , onCancel = Cancelled
            , custom = [ ( "Restore Defaults", DefaultSettingsRestored ) ]
            }


keyBindingsTable : Settings -> Element Msg
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
                    Element.Input.button []
                        { onPress = Just <| ChangeKeyBindingRequested action, label = Element.text <| keyDescription key }
            ]
        }


keyDescription : String -> String
keyDescription key =
    case key of
        " " ->
            "Space"

        "ArrowLeft" ->
            "Left arrow"

        "ArrowRight" ->
            "Right arrow"

        "ArrowDown" ->
            "Down arrow"

        "ArrowUp" ->
            "Up arrow"

        _ ->
            String.toUpper key
