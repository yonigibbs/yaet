module HighlightAnimation exposing
    ( Id
    , Model
    , Msg
    , Type(..)
    , UpdateResult(..)
    , animatedBlocks
    , animatedColour
    , animatedOpacity
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
import Browser.Events
import Color exposing (Color)
import Coord exposing (Coord)



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
        , totalTimeMs : Float
        , blocks : List ( Coord, BlockColour )
        , elapsedTimeMs : Float
        }



-- UPDATE


{-| The messages handled by this module:

  - `Frame`: An animation frame time has elapsed (i.e. the animation can increment its `percentComplete` then be re-rendered).

-}
type Msg
    = Frame { id : Id, timeSinceLastFrameMs : Float }


{-| The result of the `update` function:

  - `IgnoreMsg`: The message was ignored. Typically this is because a message has arrived for an animation which is no
    longer the current one.
  - `Continue`: The animation has progressed but is still in progress. The updated model is returned in this variant's data.
  - `Complete`: The animation is now complete. The main module can now discard this animation.

-}
type UpdateResult
    = IgnoreMsg
    | Continue Model
    | Complete


{-| Checks if the message relates to the animation in the supplied model and, if so, updates the model, and returns the
relevant result (see `UpdateResult` for more info).
-}
update : Msg -> Model -> UpdateResult
update (Frame { id, timeSinceLastFrameMs }) ((Model modelData) as model) =
    if id == modelData.id then
        handleAnimationFrame timeSinceLastFrameMs model

    else
        IgnoreMsg


{-| Handles an animation frame time having elapsed. Calculates the new progress and either increments the `percentComplete`
or, if this is now 100%, reports that this animation is now complete.
-}
handleAnimationFrame : Float -> Model -> UpdateResult
handleAnimationFrame timeSinceLastFrameMs (Model modelData) =
    let
        newElapsedTimeMs =
            modelData.elapsedTimeMs + timeSinceLastFrameMs
    in
    if newElapsedTimeMs < modelData.totalTimeMs then
        Continue <| Model { modelData | elapsedTimeMs = newElapsedTimeMs }

    else
        Complete


{-| Starts a new animation with the supplied data.
-}
startNewAnimation : Id -> Type -> Int -> List ( Coord, BlockColour ) -> Model
startNewAnimation id animationType totalTimeMs blocks =
    Model
        { id = id
        , animationType = animationType
        , totalTimeMs = toFloat totalTimeMs
        , blocks = blocks
        , elapsedTimeMs = 0
        }


{-| Returns an updated copy of the supplied model, with the supplied blocks in it.
-}
withBlocks : List ( Coord, BlockColour ) -> Model -> Model
withBlocks blocks (Model model) =
    Model { model | blocks = blocks }



-- INFORMATION ABOUT THE MODEL


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


{-| Calculates the opacity to use when rendering a highlighted cell with the given animation model.
-}
animatedOpacity : Model -> Float
animatedOpacity (Model { animationType, elapsedTimeMs, totalTimeMs }) =
    case animationType of
        ShapeLanding ->
            let
                percentComplete =
                    100 * elapsedTimeMs / totalTimeMs
            in
            if percentComplete < 50 then
                -- Reduce the opacity to nearly (but not quite) 0
                1 - (0.9 * percentComplete / 50)

            else
                -- Increase the opacity back towards 1
                1 - (0.9 * ((100 - percentComplete) / 50))

        RowRemoval ->
            -- We don't change the opacity in this animation type
            1


{-| Calculates the colour to use when rendering the supplied colour using the supplied animation. Returns a lighter or
darker version of that colour based on the type and progress of the animation.
-}
animatedColour : Model -> Color -> Color
animatedColour (Model { animationType, elapsedTimeMs, totalTimeMs }) colour =
    case animationType of
        ShapeLanding ->
            -- We don't change the colour in this animation type
            colour

        RowRemoval ->
            -- Brighten towards one for the full length of the animation, to make it "flash"
            let
                calcColourPart part =
                    part + ((1 - part) * elapsedTimeMs / totalTimeMs)
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


{-| Gets the subscriptions required for the animation to run.
-}
subscriptions : Model -> Sub Msg
subscriptions (Model { id }) =
    Browser.Events.onAnimationFrameDelta
        (\timeSinceLastFrameMs -> Frame { id = id, timeSinceLastFrameMs = timeSinceLastFrameMs })
