module HighlightAnimation exposing
    ( Id
    , Model
    , Msg
    , Type(..)
    , UpdateResult(..)
    , animatedBlocks
    , animatedColour
    , highlightAnimationType
    , initialId
    , nextAnimationId
    , startNewAnimation
    , subscriptions
    , update
    , withBlocks
    )

import Block
import Color exposing (Color)
import Task
import Time


type Type
    = ShapeLanding
    | LineRemoval


type Id
    = Id Int


initialId : Id
initialId =
    Id 0


nextAnimationId : Id -> Id
nextAnimationId (Id id) =
    Id <| id + 1


type Model
    = Model
        { id : Id
        , animationType : Type
        , totalTimeMs : Int
        , blocks : List ( Block.Coord, Block.Colour )
        , progress : Progress
        }


type Progress
    = Pending
    | Running { start : Time.Posix, percentComplete : Float }


type Msg
    = StartTimeAvailable { id : Id, start : Time.Posix }
    | Frame { id : Id, time : Time.Posix }


type UpdateResult
    = IgnoreMsg
    | Continue ( Model, Cmd Msg )
    | Complete


update : Msg -> Model -> UpdateResult
update msg ((Model modelData) as model) =
    case msg of
        StartTimeAvailable { id, start } ->
            if id == modelData.id then
                Continue ( Model { modelData | progress = Running { start = start, percentComplete = 0 } }, Cmd.none )

            else
                IgnoreMsg

        Frame { id, time } ->
            if id == modelData.id then
                handleAnimationFrame time model

            else
                IgnoreMsg


handleAnimationFrame : Time.Posix -> Model -> UpdateResult
handleAnimationFrame time (Model modelData) =
    case modelData.progress of
        Pending ->
            IgnoreMsg

        Running progress ->
            let
                newPercentComplete =
                    toFloat (Time.posixToMillis time - Time.posixToMillis progress.start) / toFloat modelData.totalTimeMs * 100
            in
            if newPercentComplete < 100 then
                Continue
                    ( Model { modelData | progress = Running { progress | percentComplete = newPercentComplete } }
                    , Cmd.none
                    )

            else
                Complete


totalTimeMs : Type -> Int -> Int
totalTimeMs animationType timerDropDelay =
    case animationType of
        ShapeLanding ->
            timerDropDelay

        LineRemoval ->
            150


highlightAnimationType : Model -> Type
highlightAnimationType (Model { animationType }) =
    animationType


startNewAnimation : Id -> Type -> Int -> List ( Block.Coord, Block.Colour ) -> ( Model, Cmd Msg )
startNewAnimation id animationType timerDropDelay blocks =
    ( Model
        { id = id
        , animationType = animationType
        , totalTimeMs = totalTimeMs animationType timerDropDelay
        , blocks = blocks
        , progress = Pending
        }
    , Task.perform (\now -> StartTimeAvailable { id = id, start = now }) Time.now
    )


withBlocks : List ( Block.Coord, Block.Colour ) -> Model -> Model
withBlocks blocks (Model model) =
    Model { model | blocks = blocks }


animatedBlocks : Model -> List ( Block.Coord, Block.Colour )
animatedBlocks (Model { blocks }) =
    blocks


animatedColour : Model -> Color -> Color
animatedColour (Model { animationType, progress }) colour =
    let
        percentComplete =
            case progress of
                Pending ->
                    0

                Running running ->
                    running.percentComplete

        calcColourPart part =
            case animationType of
                ShapeLanding ->
                    if percentComplete < 50 then
                        -- Dim the colour by reducing it towards 0, but never quite that far.
                        part - (0.9 * part * percentComplete / 50)

                    else
                        -- Brighten the colour back up from near 0 towards its initial value
                        part - (0.9 * (part * (100 - percentComplete) / 50))

                LineRemoval ->
                    -- Brighten towards one for the full length of the animation, to make it "flash"
                    part + ((1 - part) * percentComplete / 100)
    in
    Color.toRgba colour
        |> (\{ red, green, blue, alpha } ->
                { red = calcColourPart red
                , green = calcColourPart green
                , blue = calcColourPart blue
                , alpha = alpha
                }
           )
        |> Color.fromRgba


subscriptions : Model -> Sub Msg
subscriptions (Model { id, progress }) =
    case progress of
        Pending ->
            Sub.none

        Running { start } ->
            -- TODO: is 50ms the right value here?
            Time.every 50 (\now -> Frame { id = id, time = now })
