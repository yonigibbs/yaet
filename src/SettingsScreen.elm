module SettingsScreen exposing (Model, Msg, UpdateResult(..), init, update, view)

import Element exposing (Element)
import Element.Font
import Settings exposing (Settings)
import UIHelpers exposing (edges)



-- MODEL


type Model
    = Model ModelData


type Screen
    = SettingsScreen
    | KeySelectionScreen


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


type UpdateResult
    = KeepOpen
    | Close (Maybe Settings)


update : Msg -> Model -> ( Model, Cmd Msg, UpdateResult )
update msg ((Model { settings }) as model) =
    case msg of
        DefaultSettingsRestored ->
            ( model |> withSettings Settings.default, Cmd.none, KeepOpen )

        SaveRequested ->
            -- TODO: implement saving to local storage
            ( model, Cmd.none, Close <| Just settings )

        Cancelled ->
            ( model, Cmd.none, Close Nothing )


withSettings : Settings -> Model -> Model
withSettings settings (Model modelData) =
    Model { modelData | settings = settings }



-- VIEW


view : Model -> Element Msg
view (Model { settings }) =
    Element.column [ Element.Font.color UIHelpers.mainForegroundColour ]
        [ Element.el [ Element.centerX, Element.Font.bold, Element.paddingEach { edges | bottom = 15 } ]
            (Element.el [ Element.Font.size 24 ] <| Element.text "Settings")
        , keyBindingsTable settings
        ]
        |> Element.el []
        |> UIHelpers.showModal
            { onSubmit = SaveRequested
            , onCancel = Cancelled
            , custom = [ ( "Restore Defaults", DefaultSettingsRestored ) ]
            }


keyBindingsTable : Settings -> Element msg
keyBindingsTable settings =
    let
        keyActions =
            Settings.getKeyActions settings

        records =
            [ ( keyActions.moveLeft, "Move left" )
            , ( keyActions.moveRight, "Move right" )
            , ( keyActions.rotateClockwise, "Rotate clockwise" )
            , ( keyActions.rotateAnticlockwise, "Rotate anticlockwise" )
            , ( keyActions.dropOneRow, "Soft drop" )
            , ( keyActions.dropToBottom, "Hard drop" )
            , ( keyActions.hold, "Hold" )
            , ( keyActions.togglePause, "Pause" )
            ]

        column caption contents =
            { header = Element.el [ Element.Font.size 16, Element.Font.bold, Element.paddingXY 0 4 ] <| Element.text caption
            , width = Element.fill
            , view = \record -> Element.el [ Element.Font.size 14, Element.Font.semiBold ] <| contents record
            }
    in
    Element.table [ Element.spacingXY 25 5 ]
        { data = records
        , columns =
            [ column "Action" <| \( _, descr ) -> Element.text descr
            , column "Key" <| \( key, _ ) -> keyDescription key |> Element.text
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
