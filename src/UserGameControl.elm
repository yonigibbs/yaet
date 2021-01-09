module UserGameControl exposing (KeyboardConfig, Model, Msg, buildKeyboardConfig, init, subscriptions, update)

{-| This module is responsible for handling user-initiated actions. Its model stores which keys are currently being
held down, and it calculates, based on that, which actions to run. For example if a user holds down the left arrow and the
down arrow, this is responsible for deciding that the dropping shape should be moved left and down. BUt if the user then
additionally presses the right arrow, this module decides that the (newer) right arrow press should override the left
arrow one, and move the shape right instead.

It's also responsible for handling how quickly to respond to key presses. An initial keypress is responded to immediately,
and if the key is held down then it should be executed repeatedly at a given interval till the key is released. However
there should be an initial delay after the key is first held down, before it's then acted on again (after which there
should be no further delay). In order to handle this, we use the concept of a "keyboard frame". This is similar to an
animation frame: it's a repeated timer event that occurs while a key is being pressed, at a regular interval, which,
every time it occurs, means that the keyboard press should be processed again.

-}

import Browser.Events
import Dict exposing (Dict)
import Game
import Json.Decode as JD
import Shape
import Time



-- MODEL


{-| A request for an action to be executed. Created when a user holds down a keyboard key, for example.

  - `action`: The action being requested (e.g. move shape left).
  - `delayTillRepeat`: Defines how many "keyboard frames" (see comments on this module) the system should wait before
    repeating this action. When a new `ActionRequest` is created (e.g. when a user initially presses down a key) this
    is set to some value (e.g. 4), and the action is executed immediately. The system then uses `Time.every` to initiate
    a message every so often (e.g. 50ms): every time one of these messages fires, if the key is still being held down,
    we decrease the value in `delayTillRepeat`. Once it's at zero, we stop decreasing it, and instead start repeatedly
    executing the action. This means that when the user holds down a key the action is executed immediately, then there's
    a short pause, before it starts being executed repeatedly.

-}
type alias ActionRequest =
    { action : Game.UserAction, delayTillRepeat : Int }


{-| The data associated with the model, which is exposed as an opaque type

  - `keyboardConfig`: A mapping of keyboard keys to the actions they should invoke.
  - `requests`: A list of requests for actions to be executed, generally corresponding to all the keys currently being
    held down. This list is in order, with the most recent presses first, so that they override older presses (e.g. if
    user holds down left arrow then right arrow, we ignore the left arrow and move the shape right).

-}
type alias ModelData =
    { keyboardConfig : KeyboardConfig, requests : List ActionRequest }


type Model
    = Model ModelData


init : Model
init =
    Model
        -- TODO: make keyboard bindings configurable
        { keyboardConfig =
            buildKeyboardConfig
                { moveLeft = "ArrowLeft"
                , moveRight = "ArrowRight"
                , dropOneRow = "ArrowDown"
                , dropToBottom = " "
                , rotateClockwise = "x"
                , rotateAnticlockwise = "z"
                , hold = "c"
                , togglePause = "p"
                }
        , requests = []
        }



-- UPDATE


type Msg
    = ActionRequestStarted Game.UserAction -- An action is being requested, e.g. the user has just pressed a key.
    | ActionRequestStopped Game.UserAction -- An action is no longer being requested, e.g. the user has released a key.
    | KeyboardFrame -- See comments on this module.


{-| Handles a message in this module. Returns an updated `Model`, along with a list of the actions that should be executed.
-}
update : Model -> Msg -> ( Model, List Game.UserAction )
update ((Model modelData) as model) msg =
    case msg of
        ActionRequestStarted action ->
            if List.any (\request -> request.action == action) modelData.requests then
                -- This action was already requested (e.g. key already pressed) - ignore this request
                ( model, [] )

            else
                -- Append the new request at the _beginning_ of the list, and request that this action be executed immediately.
                ( model |> addNewRequest action, [ action ] )

        ActionRequestStopped action ->
            -- Remove this request from the list, and don't request that any action be run immediately.
            ( model |> removeRequest action, [] )

        KeyboardFrame ->
            handleKeyboardFrame model


addNewRequest : Game.UserAction -> Model -> Model
addNewRequest action (Model modelData) =
    Model { modelData | requests = { action = action, delayTillRepeat = 4 } :: modelData.requests }


removeRequest : Game.UserAction -> Model -> Model
removeRequest action (Model modelData) =
    let
        requests =
            modelData.requests |> List.filter (\listItem -> listItem.action /= action)
    in
    Model { modelData | requests = requests }


{-| Handles a keyboard frame (see comments on this module). For every action currently being requested, this checks if the
action is still quite new (e.g. the user only recently pressed the key): if it is (i.e. if its `delayTillRepeat` value is
greater than zero), it decrements that value, otherwise it treats it as an action ready to run. It then calculates which
actions to run (e.g. handling conflicting actions like if the user presses both the left and right keys) and returns this
in the second value in the returned tuple.
-}
handleKeyboardFrame : Model -> ( Model, List Game.UserAction )
handleKeyboardFrame (Model modelData) =
    let
        requests : List ActionRequest
        requests =
            modelData.requests
                |> List.map
                    (\action ->
                        if action.delayTillRepeat > 0 then
                            { action | delayTillRepeat = action.delayTillRepeat - 1 }

                        else
                            action
                    )

        actionsToExecute : List Game.UserAction
        actionsToExecute =
            requests |> removeNonRepeatableActions |> removeInactive |> removeConflicts |> List.map .action
    in
    ( Model { modelData | requests = requests }, actionsToExecute )


{-| Some actions are "repeatable" (e.g. if the user holds down the left arrow we want to repeatedly move the shape left).
Others, e.g. rotating a shape, aren't. This function removes the non-repeatable ones.
-}
removeNonRepeatableActions : List ActionRequest -> List ActionRequest
removeNonRepeatableActions =
    List.filter
        (\{ action } ->
            case action of
                Game.DropToBottom ->
                    False

                Game.Rotate _ ->
                    False

                Game.Hold ->
                    False

                Game.TogglePause ->
                    False

                Game.Move _ ->
                    True
        )


removeInactive : List ActionRequest -> List ActionRequest
removeInactive =
    List.filter (\request -> request.delayTillRepeat == 0)


removeConflicts : List ActionRequest -> List ActionRequest
removeConflicts requests =
    requests
        |> List.foldl
            (\request { exclude, keep } ->
                if List.member request.action exclude then
                    { exclude = exclude, keep = keep }

                else
                    { exclude = exclude ++ conflictsOf request.action, keep = request :: keep }
            )
            { exclude = [], keep = [] }
        |> (\{ exclude, keep } -> keep)


{-| Gets all possible actions that conflict with the supplied `action`, e.g. right and left keys conflict.
-}
conflictsOf : Game.UserAction -> List Game.UserAction
conflictsOf action =
    case action of
        Game.Move Game.Left ->
            [ Game.Move Game.Right ]

        Game.Move Game.Right ->
            [ Game.Move Game.Left ]

        Game.Move Game.Down ->
            []

        Game.DropToBottom ->
            [ Game.Move Game.Left
            , Game.Move Game.Right
            , Game.Move Game.Down
            , Game.Rotate Shape.Clockwise
            , Game.Rotate Shape.Anticlockwise
            ]

        Game.Rotate Shape.Clockwise ->
            [ Game.Rotate Shape.Anticlockwise ]

        Game.Rotate Shape.Anticlockwise ->
            [ Game.Rotate Shape.Clockwise ]

        Game.Hold ->
            [ Game.Move Game.Left
            , Game.Move Game.Right
            , Game.Move Game.Down
            , Game.Rotate Shape.Clockwise
            , Game.Rotate Shape.Anticlockwise
            , Game.DropToBottom
            ]

        Game.TogglePause ->
            [ Game.Move Game.Left
            , Game.Move Game.Right
            , Game.Move Game.Down
            , Game.Rotate Shape.Clockwise
            , Game.Rotate Shape.Anticlockwise
            , Game.DropToBottom
            , Game.Hold
            ]



-- KEYBOARD


{-| The configuration of the keyboard keys, mapping them to their corresponding user actions.
-}
type KeyboardConfig
    = KeyboardConfig (Dict String Game.UserAction)


{-| Builds a `KeyboardConfig` from the supplied values.
-}
buildKeyboardConfig :
    { moveLeft : String
    , moveRight : String
    , dropOneRow : String
    , dropToBottom : String
    , rotateClockwise : String
    , rotateAnticlockwise : String
    , hold : String
    , togglePause : String
    }
    -> KeyboardConfig
buildKeyboardConfig { moveLeft, moveRight, dropOneRow, dropToBottom, rotateClockwise, rotateAnticlockwise, hold, togglePause } =
    KeyboardConfig <|
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
keyboardDecoder : KeyboardConfig -> JD.Decoder Game.UserAction
keyboardDecoder (KeyboardConfig config) =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case Dict.get (String.toLower key) config of
                    Just action ->
                        JD.succeed action

                    Nothing ->
                        JD.fail "Not a mapped key - ignoring"
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model { requests, keyboardConfig }) =
    let
        keyboardFrameSub =
            case requests of
                [] ->
                    Sub.none

                _ ->
                    -- TODO: is this delay the right value here?
                    Time.every 50 <| always KeyboardFrame
    in
    Sub.batch
        [ keyboardDecoder keyboardConfig |> JD.map ActionRequestStarted |> Browser.Events.onKeyDown
        , keyboardDecoder keyboardConfig |> JD.map ActionRequestStopped |> Browser.Events.onKeyUp
        , keyboardFrameSub
        ]