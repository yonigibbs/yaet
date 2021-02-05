module Settings exposing
    ( EditableSettings
    , Settings
    , allKeyBindings
    , default
    , fromEditable
    , fromJson
    , keyBinding
    , keyboardDecoder
    , sanitiseKey
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


type Settings
    = Settings { keyBindings : Dict String Game.UserAction }



-- JSON


fromJson : JE.Value -> Settings
fromJson json =
    JD.decodeValue settingsDecoder json |> Result.withDefault default


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


keyBindingsDecoder : JD.Decoder (Dict String Game.UserAction)
keyBindingsDecoder =
    JD.keyValuePairs JD.string
        |> JD.map jsonNameValuePairsToKeyBindingsDict
        |> JD.andThen
            (\dict ->
                if Dict.size dict == List.length allActionsOrdered then
                    JD.succeed dict

                else
                    JD.fail "Incorrect number of entries in dictionary"
            )


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
        [ ( sanitiseKey moveLeft, Game.Move Game.Left )
        , ( sanitiseKey moveRight, Game.Move Game.Right )
        , ( sanitiseKey dropOneRow, Game.Move Game.Down )
        , ( sanitiseKey dropToBottom, Game.DropToBottom )
        , ( sanitiseKey rotateClockwise, Game.Rotate Shape.Clockwise )
        , ( sanitiseKey rotateAnticlockwise, Game.Rotate Shape.Anticlockwise )
        , ( sanitiseKey hold, Game.Hold )
        , ( sanitiseKey togglePause, Game.TogglePause )
        ]


{-| Decodes a key event, succeeding if it's one of the special keys we handle (as defined in the supplied `config`),
otherwise failing.
-}
keyboardDecoder : Settings -> JD.Decoder Game.UserAction
keyboardDecoder (Settings { keyBindings }) =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case Dict.get (sanitiseKey key) keyBindings of
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


sanitiseKey =
    String.toLower
