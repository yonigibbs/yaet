module GameOver exposing (Model, Msg, UpdateResult(..), getHighScores, init, subscriptions, update, view)

{-| This module handles all functionality related to when a game is over. Shows the board as it was when the game ended,
animating a "Game Over" message on top of it, then fading the game out.
-}

import BoardView
import Browser.Events
import Coord exposing (Coord)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Game exposing (Game)
import HighScores exposing (HighScores)
import Settings exposing (Settings)
import Shape
import UIHelpers
import UserGame



-- MODEL


{-| The model of the current animation (there are three stages to the animation: see `Model`).
-}
type alias AnimationModel =
    { totalTimeMs : Float, elapsedTimeMs : Float }


{-| Describes the current stage of the animation.
-}
type Animation
    = EnteringGameOverMessage AnimationModel
    | ShowingGameOverMessage AnimationModel
    | FadingOut AnimationModel


{-| The data associated with the model (which is an opaque type).
-}
type alias ModelData =
    { blocks : List ( Coord, Shape.BlockColour ), animation : Animation, highScores : HighScores }


type Model
    = Model ModelData


init : HighScores -> Game shapeBuffer -> Model
init highScores game =
    Model
        { blocks = (Game.blocks game).normal
        , animation = EnteringGameOverMessage { totalTimeMs = 1000, elapsedTimeMs = 0 }
        , highScores = highScores
        }



-- UPDATE


type Msg
    = AnimationFrame Float


{-| The value returned from the `update` function. Either `Continue`, meaning the game over animation is still in action,
or `Done`, meaning it's finished and the calling module should now return the user to the welcome screen.
-}
type UpdateResult
    = Continue Model
    | Done


{-| Updates this module's model based on the supplied message. Returns an `UpdateResult` which informs the calling module
of how to proceed (see `UpdateResult` for more info).
-}
update : Msg -> Model -> UpdateResult
update (AnimationFrame timeSinceLastFrameMs) (Model model) =
    case model.animation of
        EnteringGameOverMessage animationModel ->
            progressAnimation model
                timeSinceLastFrameMs
                animationModel
                EnteringGameOverMessage
                (Just <| ShowingGameOverMessage { totalTimeMs = 1000, elapsedTimeMs = 0 })

        ShowingGameOverMessage animationModel ->
            progressAnimation model
                timeSinceLastFrameMs
                animationModel
                ShowingGameOverMessage
                (Just <| FadingOut { totalTimeMs = 500, elapsedTimeMs = 0 })

        FadingOut animationModel ->
            progressAnimation model timeSinceLastFrameMs animationModel FadingOut Nothing


{-| Progresses the animation after an animation frame. Each animation knows the total time it should run for so this will
either continue the current animation if not enough time has elapsed yet (using `ifAnimationContinuing`) or will use
`ifAnimationOver` to decide how to proceed. If that parameter is a `Nothing` then `Done` is returned, meaning this
whole module is now finished and the user should be returned to the Welcome screen. Otherwise (i.e. if `ifAnimationOver`
is `Just` some `Animation`, then that `Animation` is used (i.e. the next stage in the overall animation proceeds).
-}
progressAnimation : ModelData -> Float -> AnimationModel -> (AnimationModel -> Animation) -> Maybe Animation -> UpdateResult
progressAnimation modelData timeSinceLastFrameMs animationModel ifAnimationContinuing ifAnimationOver =
    let
        newElapsedTimeMs =
            animationModel.elapsedTimeMs + timeSinceLastFrameMs
    in
    if newElapsedTimeMs < animationModel.totalTimeMs then
        { animationModel | elapsedTimeMs = newElapsedTimeMs }
            |> ifAnimationContinuing
            |> (\continuingAnimation -> Model { modelData | animation = continuingAnimation })
            |> Continue

    else
        ifAnimationOver
            |> Maybe.map (\nextAnimation -> Continue <| Model { modelData | animation = nextAnimation })
            |> Maybe.withDefault Done



-- VIEW


view : Model -> Element msg
view (Model modelData) =
    let
        { messageOpacity, messageGlow, entireOpacity } =
            calcViewInfo modelData

        boardView =
            BoardView.view UserGame.boardViewConfig False (modelData.blocks |> BoardView.withOpacity 1) [] Nothing

        gameOverOverlay =
            Element.row
                [ Element.Border.width 2
                , Element.centerX
                , Element.centerY
                , Element.Background.color UIHelpers.mainForegroundColour
                , Element.padding 20
                , Element.Border.rounded 20
                , Element.Border.glow (Element.rgb255 200 200 200) messageGlow
                , Element.Font.size 32
                , Element.Font.extraBold
                , Element.Font.color UIHelpers.mainBackgroundColour
                , Element.Font.family [ Element.Font.typeface "Courier New" ]
                , Element.alpha messageOpacity
                ]
                [ Element.text "Game Over" ]
    in
    Element.el [ Element.inFront gameOverOverlay, Element.alpha entireOpacity ] boardView


calcViewInfo : ModelData -> { messageOpacity : Float, messageGlow : Float, entireOpacity : Float }
calcViewInfo { animation } =
    let
        defaultMessageGlow =
            5

        calcPercentComplete { totalTimeMs, elapsedTimeMs } =
            100 * elapsedTimeMs / totalTimeMs
    in
    case animation of
        EnteringGameOverMessage animationModel ->
            let
                percentComplete =
                    calcPercentComplete animationModel
            in
            { messageOpacity = percentComplete / 100
            , messageGlow = defaultMessageGlow * percentComplete / 100
            , entireOpacity = 1
            }

        ShowingGameOverMessage animationModel ->
            let
                percentComplete =
                    calcPercentComplete animationModel

                extraMessageGlow =
                    if percentComplete < 50 then
                        percentComplete / 50 * defaultMessageGlow

                    else
                        ((100 - percentComplete) / 50) * defaultMessageGlow
            in
            { messageOpacity = 1
            , messageGlow = defaultMessageGlow + extraMessageGlow
            , entireOpacity = 1
            }

        FadingOut animationModel ->
            let
                percentComplete =
                    calcPercentComplete animationModel
            in
            { messageOpacity = 1
            , messageGlow = defaultMessageGlow - (defaultMessageGlow * percentComplete / 100)
            , entireOpacity = 1 - (percentComplete / 100)
            }



-- HIGH SCORES


getHighScores : Model -> HighScores
getHighScores (Model { highScores }) =
    highScores



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Browser.Events.onAnimationFrameDelta AnimationFrame
