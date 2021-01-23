module Settings exposing (KeyActions, Settings, default, fromJson, getKeyActions, keyboardDecoder)

{-| Contains all functionality to defining the settings (i.e. user preferences) such as keyboard bindings. Contains
the JSON de/encoders and the types.
-}

import Dict exposing (Dict)
import Game
import Json.Decode as JD
import Json.Encode as JE
import Shape


type Settings
    = Settings { keyBindings : KeyBindings }


fromJson : JE.Value -> Settings
fromJson json =
    -- TODO: read JSON from local storage and decode here
    default


default : Settings
default =
    Settings
        { keyBindings =
            buildKeyBindings
                { moveLeft = "ArrowLeft"
                , moveRight = "ArrowRight"
                , dropOneRow = "ArrowDown"
                , dropToBottom = " "
                , rotateClockwise = "x"
                , rotateAnticlockwise = "z"
                , hold = "c"
                , togglePause = "p"
                }
        }



-- KEY BINDINGS


type alias KeyActions =
    { moveLeft : String
    , moveRight : String
    , dropOneRow : String
    , dropToBottom : String
    , rotateClockwise : String
    , rotateAnticlockwise : String
    , hold : String
    , togglePause : String
    }


{-| The configuration of the keyboard keys, mapping them to their corresponding user actions.
-}
type alias KeyBindings =
    { keyActions : KeyActions, dict : Dict String Game.UserAction }


{-| Builds a `KeyBindings` value from the supplied values.
-}
buildKeyBindings : KeyActions -> KeyBindings
buildKeyBindings ({ moveLeft, moveRight, dropOneRow, dropToBottom, rotateClockwise, rotateAnticlockwise, hold, togglePause } as keyActions) =
    { keyActions = keyActions
    , dict =
        Dict.fromList
            [ ( String.toLower moveLeft, Game.Move Game.Left )
            , ( String.toLower moveRight, Game.Move Game.Right )
            , ( String.toLower dropOneRow, Game.Move Game.Down )
            , ( String.toLower dropToBottom, Game.DropToBottom )
            , ( String.toLower rotateClockwise, Game.Rotate Shape.Clockwise )
            , ( String.toLower rotateAnticlockwise, Game.Rotate Shape.Anticlockwise )
            , ( String.toLower hold, Game.Hold )
            , ( String.toLower togglePause, Game.TogglePause )
            ]
    }


{-| Decodes a key event, succeeding if it's one of the special keys we handle (as defined in the supplied `config`),
otherwise failing.
-}
keyboardDecoder : Settings -> JD.Decoder Game.UserAction
keyboardDecoder (Settings { keyBindings }) =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case Dict.get (String.toLower key) keyBindings.dict of
                    Just action ->
                        JD.succeed action

                    Nothing ->
                        JD.fail "Not a mapped key - ignoring"
            )


getKeyActions : Settings -> KeyActions
getKeyActions (Settings { keyBindings }) =
    keyBindings.keyActions
