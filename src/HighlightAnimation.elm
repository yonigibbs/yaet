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

{-| This module controls animation of blocks, used when a dropping shape is about to land (which is animated by fading
it out then back in) and when one or more rows are about to disappear (which is animated by "flashing" the row(s)
briefly.

This module has its own model/msg/update/subscriptions/etc., which the main module has to "map" in the usual way.

-}

import BlockColour exposing (BlockColour)
import Color exposing (Color)
import Coord exposing (Coord)
import Task
import Time



-- ANIMATION ID


{-| A unique identifier of a given animation process. The animation model contains its current ID, and any messages also
have this ID in their data. Then, when the (timer) event fires and is handled, we can check that the ID in the message
matches what's in the model: if it doesn't it means that since the timer event was requested the model has been updated
and either there's no longer any animation required, or there's a new animation, so the message can be ignored.

This is an opaque type, and internally is managed as an incrementing integer. (It's highly unlikely that the upper
limits of an Int will ever be hit, but if that does occur we can just go back to 0 at a certain point.)

-}
type Id
    = Id Int


{-| The initial ID to use for the next animation.
-}
initialId : Id
initialId =
    Id 0


{-| The next animation ID to use after the supplied one.
-}
nextAnimationId : Id -> Id
nextAnimationId (Id id) =
    Id <| id + 1



-- MODEL TYPES


{-| The type of animation:

  - `ShapeLanding`: a shape is about to land so is animated by fading it out then back in.
  - `RowRemoval`: one or more rows are about to be removed, so are animated by "flashing" them briefly.

-}
type Type
    = ShapeLanding
    | RowRemoval


{-| The model containing the information about an animation:

  - `id`: The unique ID of this animation (see the `Id` type for more info).
  - `animationType`: The type of animation being executed (see the `Type` type for more info).
  - `totalTimeMs`: The total time, in milliseconds, which the animation should run for.
  - `blocks`: The blocks to be animated.
  - `progress`: The progress of the animation (see the `Progress` type for more info).

-}
type Model
    = Model
        { id : Id
        , animationType : Type
        , totalTimeMs : Int
        , blocks : List ( Coord, BlockColour )
        , progress : Progress
        }


{-| Defines the progress status of the animation:

  - `Pending`: The animation is in the process of being started. In order to start it needs to know its start time (so
    that it can be ended at the right time). Getting time is an async action so while we're waiting for the current time
    the animation is considered to be in this state.
  - `Running`: The animation is currently running. It now knows its `start` time, and also its `percentComplete` (a
    numeric value between 0 and 100).

-}
type Progress
    = Pending
    | Running { start : Time.Posix, percentComplete : Float }



-- UPDATE


{-| The messages handled by this module:

  - `StartTimeAvailable`: The time at which the animation is starting is now available. See the `Progress` type for more
    info.
  - `Frame`: An animation frame time has elapsed (i.e. the animation can increment its `percentComplete` then be re-rendered).

-}
type Msg
    = StartTimeAvailable { id : Id, start : Time.Posix }
    | Frame { id : Id, time : Time.Posix }


{-| The result of the `update` function:

  - `IgnoreMsg`: The message was ignored. Typically this is because a message has arrived for an animation which is no
    longer the current one.
  - `Continue`: The animation has progressed but is still in progress. The updated model, and next command to run, are
    returned in this variant's data.
  - `Complete`: The animation is now complete. The main module can now discard this animation.

-}
type UpdateResult
    = IgnoreMsg
    | Continue ( Model, Cmd Msg )
    | Complete


{-| Checks if the message relates to the animation in the supplied model and, if so, updates the model, and returns the
relevant result (see `UpdateResult` for more info).
-}
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


{-| Handles an animation frame time having elapsed. Calculates the new progress and either increments the `percentComplete`
or, if this is now 100%, reports that this animation is now complete.
-}
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


{-| Starts a new animation with the supplied data.
-}
startNewAnimation : Id -> Type -> Int -> List ( Coord, BlockColour ) -> ( Model, Cmd Msg )
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


{-| Returns an updated copy of the supplied model, with the supplied blocks in it.
-}
withBlocks : List ( Coord, BlockColour ) -> Model -> Model
withBlocks blocks (Model model) =
    Model { model | blocks = blocks }



-- INFORMATION ABOUT THE MODEL


{-| Calculates the total time use for an animation of the given type.
-}
totalTimeMs : Type -> Int -> Int
totalTimeMs animationType timerDropDelay =
    case animationType of
        ShapeLanding ->
            timerDropDelay

        RowRemoval ->
            150


{-| Gets the type of animation which the supplied model has.
-}
highlightAnimationType : Model -> Type
highlightAnimationType (Model { animationType }) =
    animationType


{-| Gets the blocks contained in the supplied model.
-}
animatedBlocks : Model -> List ( Coord, BlockColour )
animatedBlocks (Model { blocks }) =
    blocks


{-| Calculates the colour to use when rendering the supplied colour using the supplied animation. Returns a lighter or
darker version of that colour based on the type and progress of the animation.
-}
animatedColour : Model -> Color -> Color
animatedColour (Model { animationType, progress }) colour =
    let
        percentComplete =
            case progress of
                Pending ->
                    0

                Running running ->
                    running.percentComplete

        calcColourPart =
            case animationType of
                ShapeLanding ->
                    if percentComplete < 50 then
                        -- Dim the colour by reducing it towards 0, but never quite that far.
                        \part -> part - (0.9 * part * percentComplete / 50)

                    else
                        -- Brighten the colour back up from near 0 towards its initial value
                        \part -> part - (0.9 * (part * (100 - percentComplete) / 50))

                RowRemoval ->
                    -- Brighten towards one for the full length of the animation, to make it "flash"
                    \part -> part + ((1 - part) * percentComplete / 100)
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



-- SUBSCRIPTIONS


{-| Gets the subscriptions required for the animation to run. Sets up a timer subscription which is used to update the
animation every 50 ms.
-}
subscriptions : Model -> Sub Msg
subscriptions (Model { id, progress }) =
    case progress of
        Pending ->
            Sub.none

        Running { start } ->
            -- TODO: is 50ms the right value here?
            Time.every 50 (\now -> Frame { id = id, time = now })
