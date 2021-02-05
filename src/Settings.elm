module Settings exposing
    ( EditableSettings
    , Settings
    , allKeyBindings
    , default
    , fromEditable
    , fromJson
    , keyBinding
    , keyboardDecoder
    , toEditable
    , toJson
    , withKeyBinding
    )

{-| Contains all functionality to defining the settings (i.e. user preferences) such as keyboard bindings. Contains
the JSON de/encoders and the types.
-}

import AssocList
import Dict exposing (Dict)
import Game
import Json.Decode as JD
import Json.Encode as JE
import Shape


{-| The main type exposed from this module. Defines all settings required to run the game (currently just the keyboard
bindings).

This type is exposed as an opaque type. Internally we store keyBindings as a dictionary keyed on the key, as during
gameplay, when performance is most important, we need to most quickly get from a key which the user has pressed to the
corresponding action. This also ensures that no key can be bound to two different actions. When we persist this to JSON,
however, we swap these about: the JSON field name is the action (e.g. "moveLeft"), and the JSON field value is the key
(e.g. "LeftArrow").

-}
type Settings
    = Settings { keyBindings : Dict String Game.UserAction }



-- JSON


{-| Decodes the supplied JSON value into a `Settings` value, falling back to the `default` value on any decoding errors.
-}
fromJson : JE.Value -> Settings
fromJson json =
    JD.decodeValue settingsDecoder json |> Result.withDefault default


{-| Encodes the supplied `Settings` to a JSON value (e.g. to persist in local storage).
-}
toJson : Settings -> JE.Value
toJson (Settings { keyBindings }) =
    keyBindings
        |> Dict.toList
        |> List.map (\( key, action ) -> ( actionToJsonFieldName action, JE.string key ))
        |> JE.object
        |> (\keyBindingsJson -> JE.object [ ( keyBindingsJsonFieldName, keyBindingsJson ) ])


keyBindingsJsonFieldName =
    "keyBindings"


settingsDecoder : JD.Decoder Settings
settingsDecoder =
    JD.field keyBindingsJsonFieldName keyBindingsDecoder
        |> JD.map (\keyBindings -> Settings { keyBindings = keyBindings })


{-| Decodes a JSON value to the internal representation of key bindings, namely a dictionary keyed on a string (the key,
e.g. "LeftArrow"), where the value is the action this key corresponds to.
-}
keyBindingsDecoder : JD.Decoder (Dict String Game.UserAction)
keyBindingsDecoder =
    -- Decode to a list of name/value pairs, where the name is the action (e.g. "moveLeft") and the value is the key (e.g. "LeftArrow")
    JD.keyValuePairs JD.string
        -- Convert this list of name/value pairs to a dictionary keyed on key (notice this was previously the value).
        |> JD.map jsonNameValuePairsToKeyBindingsDict
        |> JD.andThen
            (\dict ->
                -- Now check we have the right number of entries.
                if Dict.size dict == List.length allActionsOrdered then
                    JD.succeed dict

                else
                    JD.fail "Incorrect number of entries in dictionary"
            )


{-| Converts a list of name/value pairs from a JSON value (where the name is the action, e.g. "moveLeft", and the value
is the key, e.g. "LeftArrow") to a dictionary keyed on the key (e.g. "LeftArrow") where the value is the proper
`Game.UserAction` value.
-}
jsonNameValuePairsToKeyBindingsDict : List ( String, String ) -> Dict String Game.UserAction
jsonNameValuePairsToKeyBindingsDict =
    List.foldl
        (\( actionString, key ) dict ->
            case jsonFieldNameToAction actionString of
                Just action ->
                    Dict.insert key action dict

                Nothing ->
                    dict
        )
        Dict.empty


actionToJsonFieldName : Game.UserAction -> String
actionToJsonFieldName action =
    case action of
        Game.Move Game.Left ->
            "moveLeft"

        Game.Move Game.Right ->
            "moveRight"

        Game.Move Game.Down ->
            "softDrop"

        Game.DropToBottom ->
            "hardDrop"

        Game.Rotate Shape.Clockwise ->
            "rotateClockwise"

        Game.Rotate Shape.Anticlockwise ->
            "rotateAnticlockwise"

        Game.Hold ->
            "hold"

        Game.TogglePause ->
            "togglePause"


jsonFieldNameToAction : String -> Maybe Game.UserAction
jsonFieldNameToAction fieldName =
    case fieldName of
        "moveLeft" ->
            Just <| Game.Move Game.Left

        "moveRight" ->
            Just <| Game.Move Game.Right

        "softDrop" ->
            Just <| Game.Move Game.Down

        "hardDrop" ->
            Just <| Game.DropToBottom

        "rotateClockwise" ->
            Just <| Game.Rotate Shape.Clockwise

        "rotateAnticlockwise" ->
            Just <| Game.Rotate Shape.Anticlockwise

        "hold" ->
            Just <| Game.Hold

        "togglePause" ->
            Just <| Game.TogglePause

        _ ->
            Nothing


{-| The default key bindings.
-}
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


{-| Defines the settings when they're in a state of being edited. Similar to the actual `Settings` type, except that the
dictionary is keyed on the action rather than the keyboard key. Also this is allowed to be in an invalid state, e.g.
where some actions don't have a key binding. This is required in case a user changes an action to use a key which is
currently used by another action. In that case, the new binding is used, and the previous action is left temporarily
"unbound", till the user assigns a new binding.
-}
type EditableSettings
    = EditableSettings { keyBindings : AssocList.Dict Game.UserAction String }


{-| All the actions in the game, in the order in which they are shown to the user in the settings screen.
-}
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


{-| Converts the supplied `Settings` to an `EditableSettings` value, for use on the Settings screen.
-}
toEditable : Settings -> EditableSettings
toEditable (Settings { keyBindings }) =
    EditableSettings
        { keyBindings =
            keyBindings
                |> Dict.foldl
                    (\key action acc -> AssocList.insert action key acc)
                    AssocList.empty
        }


{-| Converts the supplied `EditableSettings` to a `Settings` value, if valid (i.e. if all actions have bindings).
-}
fromEditable : EditableSettings -> Maybe Settings
fromEditable (EditableSettings { keyBindings }) =
    keyBindings
        |> AssocList.foldl (\action key acc -> Dict.insert key action acc) Dict.empty
        |> (\newKeyBindings ->
                if Dict.size newKeyBindings == List.length allActionsOrdered then
                    Just <| Settings { keyBindings = newKeyBindings }

                else
                    Nothing
           )


{-| Gets a list of all the current bindings in the supplied `EditableSettings`.
-}
allKeyBindings : EditableSettings -> List { action : Game.UserAction, key : Maybe String }
allKeyBindings (EditableSettings { keyBindings }) =
    allActionsOrdered
        |> List.map (\action -> { action = action, key = AssocList.get action keyBindings })


{-| Gets the current bindings in the supplied `EditableSettings` for the given `action`.
-}
keyBinding : Game.UserAction -> EditableSettings -> Maybe String
keyBinding action (EditableSettings { keyBindings }) =
    AssocList.get action keyBindings


{-| Updates the supplied `EditableSettings` with the supplied binding.
-}
withKeyBinding : Game.UserAction -> String -> EditableSettings -> EditableSettings
withKeyBinding action key (EditableSettings { keyBindings }) =
    let
        lowerKey =
            String.toLower key

        -- Remove any other actions associated with this key.
        newKeyBindings : AssocList.Dict Game.UserAction String
        newKeyBindings =
            keyBindings
                |> AssocList.foldl
                    (\currentAction currentKey acc ->
                        if currentKey == lowerKey then
                            acc

                        else
                            AssocList.insert currentAction currentKey acc
                    )
                    AssocList.empty
    in
    EditableSettings { keyBindings = AssocList.insert action lowerKey newKeyBindings }
