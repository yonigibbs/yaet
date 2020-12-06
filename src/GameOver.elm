module GameOver exposing (Model, Msg, UpdateResult(..), init, subscriptions, update, view)

import BlockColour exposing (BlockColour)
import BoardView
import Browser.Events
import Coord exposing (Coord)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Game exposing (Game)
import Task
import Time
import UIHelpers
import UserGame



-- MODEL


type alias AnimationModel =
    { blocks : List ( Coord, BlockColour ), totalTimeMs : Float, elapsedTimeMs : Float }


type Model
    = EnteringGameOverMessage AnimationModel
    | ShowingGameOverMessage AnimationModel
    | FadingOut AnimationModel


init : Game -> Model
init game =
    EnteringGameOverMessage { blocks = (Game.blocks game).normal, totalTimeMs = 1000, elapsedTimeMs = 0 }



-- UPDATE


type Msg
    = AnimationFrame Float


type UpdateResult
    = Continue Model
    | Done


update : Msg -> Model -> UpdateResult
update msg model =
    case ( msg, model ) of
        ( AnimationFrame timeSinceLastFrameMs, EnteringGameOverMessage animationModel ) ->
            progressAnimation timeSinceLastFrameMs
                animationModel
                EnteringGameOverMessage
                (Continue <| ShowingGameOverMessage { blocks = animationModel.blocks, totalTimeMs = 1000, elapsedTimeMs = 0 })

        ( AnimationFrame timeSinceLastFrameMs, ShowingGameOverMessage animationModel ) ->
            progressAnimation timeSinceLastFrameMs
                animationModel
                ShowingGameOverMessage
                (Continue <| FadingOut { blocks = animationModel.blocks, totalTimeMs = 500, elapsedTimeMs = 0 })

        ( AnimationFrame timeSinceLastFrameMs, FadingOut animationModel ) ->
            progressAnimation timeSinceLastFrameMs animationModel FadingOut Done


progressAnimation : Float -> AnimationModel -> (AnimationModel -> Model) -> UpdateResult -> UpdateResult
progressAnimation timeSinceLastFrameMs ({ totalTimeMs, elapsedTimeMs } as animationModel) ifAnimationRunning ifAnimationOver =
    let
        newElapsedTimeMs =
            elapsedTimeMs + timeSinceLastFrameMs
    in
    if newElapsedTimeMs < totalTimeMs then
        { animationModel | elapsedTimeMs = newElapsedTimeMs } |> ifAnimationRunning |> Continue

    else
        ifAnimationOver



-- VIEW


view : Model -> Element msg
view model =
    let
        { blocks, messageOpacity, messageGlow, entireOpacity } =
            calcViewInfo model

        boardView =
            BoardView.view UserGame.boardViewConfig blocks Nothing

        gameOverOverlay =
            Element.row
                [ Element.Border.width 2
                , Element.centerX
                , Element.centerY
                , Element.Background.color UIHelpers.buttonBorderColor
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


calcViewInfo : Model -> { blocks : List ( Coord, BlockColour ), messageOpacity : Float, messageGlow : Float, entireOpacity : Float }
calcViewInfo model =
    let
        defaultMessageGlow =
            5

        calcPercentComplete { totalTimeMs, elapsedTimeMs } =
            100 * elapsedTimeMs / totalTimeMs
    in
    case model of
        EnteringGameOverMessage animationModel ->
            let
                percentComplete =
                    calcPercentComplete animationModel
            in
            { blocks = animationModel.blocks
            , messageOpacity = percentComplete / 100
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
            { blocks = animationModel.blocks
            , messageOpacity = 1
            , messageGlow = defaultMessageGlow + extraMessageGlow
            , entireOpacity = 1
            }

        FadingOut animationModel ->
            let
                percentComplete =
                    calcPercentComplete animationModel
            in
            { blocks = animationModel.blocks
            , messageOpacity = 1
            , messageGlow = defaultMessageGlow - (defaultMessageGlow * percentComplete / 100)
            , entireOpacity = 1 - (percentComplete / 100)
            }



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Browser.Events.onAnimationFrameDelta AnimationFrame
