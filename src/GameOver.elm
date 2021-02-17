module GameOver exposing (Model, Msg, UpdateResult(..), getHighScores, init, subscriptions, update, view)

{-| This module handles all functionality related to when a game is over. Shows the board as it was when the game ended,
animating a "Game Over" message on top of it, then fading the game out.

Also responsible for asking the user to fill in their high scores, if a new high score was achieved.

-}

import BoardView
import Browser.Events
import Coord exposing (Coord)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Game exposing (Game)
import HighScores exposing (HighScores, NewHighScoreModel)
import Scoring
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


type ScreenState
    = Animating Animation
    | HandlingNewHighScore NewHighScoreModel


{-| The data associated with the model (which is an opaque type).
-}
type alias ModelData =
    { blocks : List ( Coord, Shape.BlockColour ), state : ScreenState, score : Int, highScores : HighScores }


type Model
    = Model ModelData


init : HighScores -> Game shapeBuffer -> Model
init highScores game =
    Model
        { blocks = (Game.blocks game).normal
        , state = Animating <| EnteringGameOverMessage { totalTimeMs = 1000, elapsedTimeMs = 0 }
        , score = game |> Game.getScoring |> Scoring.getPoints
        , highScores = highScores
        }



-- UPDATE


type Msg
    = AnimationFrame Float
    | GotNewHighScoreDialogMsg HighScores.NewHighScoreMsg


{-| The value returned from the `update` function. Either `Continue`, meaning the game over animation is still in action,
or `Done`, meaning it's finished and the calling module should now return the user to the welcome screen.
-}
type UpdateResult
    = Continue ( Model, Cmd Msg )
    | Done ( Model, Cmd Msg )


{-| Updates this module's model based on the supplied message. Returns an `UpdateResult` which informs the calling module
of how to proceed (see `UpdateResult` for more info).
-}
update : Msg -> Model -> UpdateResult
update msg ((Model modelData) as model) =
    let
        ignore =
            Continue ( model, Cmd.none )
    in
    case ( msg, modelData.state ) of
        ( AnimationFrame timeSinceLastFrameMs, Animating (EnteringGameOverMessage animationModel) ) ->
            progressAnimation model
                timeSinceLastFrameMs
                animationModel
                EnteringGameOverMessage
                (Just ( Animating <| ShowingGameOverMessage { totalTimeMs = 1000, elapsedTimeMs = 0 }, Cmd.none ))

        ( AnimationFrame timeSinceLastFrameMs, Animating (ShowingGameOverMessage animationModel) ) ->
            let
                ifAnimationOver =
                    case HighScores.initNewHighScoreDialog modelData.score modelData.highScores of
                        Just ( subModel, subCmd ) ->
                            Just ( HandlingNewHighScore subModel, Cmd.map GotNewHighScoreDialogMsg subCmd )

                        Nothing ->
                            -- Score wasn't high enough to be a new high score - when animation ends just fade out then
                            -- return to Welcome screen
                            Just ( Animating <| FadingOut { totalTimeMs = 500, elapsedTimeMs = 0 }, Cmd.none )
            in
            progressAnimation model timeSinceLastFrameMs animationModel ShowingGameOverMessage ifAnimationOver

        ( AnimationFrame timeSinceLastFrameMs, Animating (FadingOut animationModel) ) ->
            progressAnimation model timeSinceLastFrameMs animationModel FadingOut Nothing

        ( AnimationFrame _, HandlingNewHighScore _ ) ->
            ignore

        ( GotNewHighScoreDialogMsg subMsg, _ ) ->
            handleNewHighScoreDialogMsg subMsg model


handleNewHighScoreDialogMsg : HighScores.NewHighScoreMsg -> Model -> UpdateResult
handleNewHighScoreDialogMsg msg ((Model modelData) as model) =
    case modelData.state of
        Animating _ ->
            Continue ( model, Cmd.none )

        HandlingNewHighScore newHighScoreModel ->
            case HighScores.updateNewHighScoreDialog msg newHighScoreModel of
                HighScores.KeepOpen nextNewHighScoreModel ->
                    Continue <| ( Model { modelData | state = HandlingNewHighScore nextNewHighScoreModel }, Cmd.none )

                HighScores.Close (Just ( newHighScores, subCmd )) ->
                    Done
                        ( Model { modelData | highScores = newHighScores }
                        , Cmd.map GotNewHighScoreDialogMsg subCmd
                        )

                HighScores.Close Nothing ->
                    Done ( model, Cmd.none )


{-| Progresses the animation after an animation frame. Each animation knows the total time it should run for so this will
either continue the current animation if not enough time has elapsed yet (using `ifAnimationContinuing`) or will use
`ifAnimationOver` to decide how to proceed. If that parameter is a `Nothing` then `Done` is returned, meaning this
whole module is now finished and the user should be returned to the Welcome screen. Otherwise (i.e. if `ifAnimationOver`
is `Just` some `Animation`, then that `Animation` is used (i.e. the next stage in the overall animation proceeds).
-}
progressAnimation : Model -> Float -> AnimationModel -> (AnimationModel -> Animation) -> Maybe ( ScreenState, Cmd Msg ) -> UpdateResult
progressAnimation ((Model modelData) as model) timeSinceLastFrameMs animationModel ifAnimationContinuing ifAnimationOver =
    let
        newElapsedTimeMs =
            animationModel.elapsedTimeMs + timeSinceLastFrameMs
    in
    if newElapsedTimeMs < animationModel.totalTimeMs then
        { animationModel | elapsedTimeMs = newElapsedTimeMs }
            |> ifAnimationContinuing
            |> (\continuingAnimation -> Continue ( Model { modelData | state = Animating continuingAnimation }, Cmd.none ))

    else
        ifAnimationOver
            |> Maybe.map (\( nextState, nextCmd ) -> Continue ( Model { modelData | state = nextState }, nextCmd ))
            |> Maybe.withDefault (Done ( model, Cmd.none ))



-- VIEW


view : Model -> Element Msg
view (Model modelData) =
    let
        boardView =
            BoardView.view UserGame.boardViewConfig False (modelData.blocks |> BoardView.withOpacity 1) [] Nothing

        ( overlay, opacity ) =
            case modelData.state of
                Animating animation ->
                    gameOverOverlay animation

                HandlingNewHighScore newHighScoreModel ->
                    ( HighScores.newHighScoreView newHighScoreModel |> Element.map GotNewHighScoreDialogMsg, 1.0 )
    in
    Element.el [ Element.inFront overlay, Element.alpha opacity ] boardView


gameOverOverlay : Animation -> ( Element msg, Float )
gameOverOverlay animation =
    let
        { messageOpacity, messageGlow, entireOpacity } =
            calcGameOverMsgViewInfo animation
    in
    ( Element.row
        [ Element.centerX
        , Element.centerY
        , Element.Border.width 2
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
    , entireOpacity
    )


calcGameOverMsgViewInfo : Animation -> { messageOpacity : Float, messageGlow : Float, entireOpacity : Float }
calcGameOverMsgViewInfo animation =
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


subscriptions : Model -> Sub Msg
subscriptions (Model { state }) =
    case state of
        Animating _ ->
            Browser.Events.onAnimationFrameDelta AnimationFrame

        HandlingNewHighScore newHighScoreModel ->
            HighScores.newHighScoreDialogSubscriptions newHighScoreModel |> Sub.map GotNewHighScoreDialogMsg
