module Settings exposing (EditableSettings, Settings, allKeyBindings, default, fromEditable, fromJson, keyBinding, keyboardDecoder, toEditable, withKeyBinding)

{-| Contains all functionality to defining the settings (i.e. user preferences) such as keyboard bindings. Contains
the JSON de/encoders and the types.
-}

import AssocList
import Dict exposing (Dict)
import Game
import Json.Decode as JD
import Json.Encode as JE
import Shape


type Settings
    = Settings { keyBindings : Dict String Game.UserAction }


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


{-| Builds a `KeyBindings` value from the supplied values.
-}
buildKeyBindings :
    { moveLeft : String
    , moveRight : String
    , dropOneRow : String
    , dropToBottom : String
    , rotateClockwise : String
    , rotateAnticlockwise : String
    , hold : String
    , togglePause : String
    }
    -> Dict String Game.UserAction
buildKeyBindings { moveLeft, moveRight, dropOneRow, dropToBottom, rotateClockwise, rotateAnticlockwise, hold, togglePause } =
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


{-| Decodes a key event, succeeding if it's one of the special keys we handle (as defined in the supplied `config`),
otherwise failing.
-}
keyboardDecoder : Settings -> JD.Decoder Game.UserAction
keyboardDecoder (Settings { keyBindings }) =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case Dict.get (String.toLower key) keyBindings of
                    Just action ->
                        JD.succeed action

                    Nothing ->
                        JD.fail "Not a mapped key - ignoring"
            )



-- EDITABLE SETTINGS


type EditableSettings
    = EditableSettings { keyBindings : AssocList.Dict Game.UserAction String }


allActionsOrdered : List Game.UserAction
allActionsOrdered =
    [ Game.Move Game.Left
    , Game.Move Game.Right
    , Game.Rotate Shape.Clockwise
    , Game.Rotate Shape.Anticlockwise
    , Game.Move Game.Down
    , Game.DropToBottom
    , Game.Hold
    , Game.TogglePause
    ]


toEditable : Settings -> EditableSettings
toEditable (Settings { keyBindings }) =
    EditableSettings
        { keyBindings =
            keyBindings
                |> Dict.foldl
                    (\key action acc -> AssocList.insert action key acc)
                    AssocList.empty
        }


fromEditable : EditableSettings -> Maybe Settings
fromEditable (EditableSettings { keyBindings }) =
    if AssocList.size keyBindings == List.length allActionsOrdered then
        keyBindings
            |> AssocList.foldl (\action key acc -> Dict.insert key action acc) Dict.empty
            |> (\fullKeyBindings -> Settings { keyBindings = fullKeyBindings })
            |> Just

    else
        Nothing


allKeyBindings : EditableSettings -> List { action : Game.UserAction, key : Maybe String }
allKeyBindings (EditableSettings { keyBindings }) =
    allActionsOrdered
        |> List.map (\action -> { action = action, key = AssocList.get action keyBindings })


keyBinding : Game.UserAction -> EditableSettings -> Maybe String
keyBinding action (EditableSettings { keyBindings }) =
    AssocList.get action keyBindings


withKeyBinding : Game.UserAction -> String -> EditableSettings -> EditableSettings
withKeyBinding newAction newKey (EditableSettings { keyBindings }) =
    let
        -- Remove any other actions associated with this key.
        newKeyBindings : AssocList.Dict Game.UserAction String
        newKeyBindings =
            keyBindings
                |> AssocList.foldl
                    (\currentAction currentKey acc ->
                        if currentKey == newKey then
                            acc

                        else
                            AssocList.insert currentAction currentKey acc
                    )
                    AssocList.empty
    in
    EditableSettings { keyBindings = AssocList.insert newAction newKey newKeyBindings }
