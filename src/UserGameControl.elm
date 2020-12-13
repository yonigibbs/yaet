module UserGameControl exposing (KeyboardConfig, Model, Msg, buildKeyboardConfig, init, subscriptions, update)

import Browser.Events
import Dict exposing (Dict)
import Game
import Json.Decode as JD
import Shape
import Time



-- MODEL


type alias ActionRequest =
    { action : Game.UserAction, delayTillRepeat : Int }


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
                , dropImmediately = "Space"
                , rotateClockwise = "x"
                , rotateAnticlockwise = "z"
                }
        , requests = []
        }



-- UPDATE


type Msg
    = ActionRequestStarted Game.UserAction
    | ActionRequestStopped Game.UserAction
    | KeyboardFrame


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
    -- TODO: is 3 the right value here?
    Model { modelData | requests = { action = action, delayTillRepeat = 3 } :: modelData.requests }


removeRequest : Game.UserAction -> Model -> Model
removeRequest action (Model modelData) =
    let
        requests =
            modelData.requests |> List.filter (\listItem -> listItem.action /= action)
    in
    Model { modelData | requests = requests }


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


buildUpdateResult : Model -> List ActionRequest -> ( Model, List Game.UserAction )
buildUpdateResult (Model modelData) requests =
    let
        activeActions =
            requests |> removeConflicts |> removeInactive |> List.map .action
    in
    ( Model { modelData | requests = requests |> removeNonRepeatableActions }, activeActions )


removeNonRepeatableActions : List ActionRequest -> List ActionRequest
removeNonRepeatableActions =
    -- TODO: should rotate action be non-repeatable as well?
    List.filter
        (\{ action } ->
            case action of
                Game.DropToBottom ->
                    False

                _ ->
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



-- KEYBOARD


type KeyboardConfig
    = Config (Dict String Game.UserAction)


buildKeyboardConfig :
    { moveLeft : String
    , moveRight : String
    , dropOneRow : String
    , dropImmediately : String
    , rotateClockwise : String
    , rotateAnticlockwise : String
    }
    -> KeyboardConfig
buildKeyboardConfig { moveLeft, moveRight, dropOneRow, dropImmediately, rotateClockwise, rotateAnticlockwise } =
    Config <|
        Dict.fromList
            [ ( moveLeft, Game.Move Game.Left )
            , ( moveRight, Game.Move Game.Right )
            , ( dropOneRow, Game.Move Game.Down )
            , ( dropImmediately, Game.DropToBottom )
            , ( rotateClockwise, Game.Rotate Shape.Clockwise )
            , ( rotateAnticlockwise, Game.Rotate Shape.Anticlockwise )
            ]


{-| Decodes a key event, succeeding if it's one of the special keys we handle (see `Key` type), otherwise failing.
-}
keyboardDecoder : KeyboardConfig -> JD.Decoder Game.UserAction
keyboardDecoder (Config config) =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case Dict.get key config of
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
