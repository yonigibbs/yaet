module GameOver exposing (Model, Msg, UpdateResult(..), init, subscriptions, update, view)

import BlockColour exposing (BlockColour)
import BoardView
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
    { blocks : List ( Coord, BlockColour ), startTime : Time.Posix, percentComplete : Float }


type Model
    = Initialising { blocks : List ( Coord, BlockColour ) }
    | EnteringGameOverMessage AnimationModel
    | ShowingGameOverMessage AnimationModel
    | FadingOut AnimationModel


init : Game -> ( Model, Cmd Msg )
init game =
    ( Initialising { blocks = (Game.blocks game).normal }
    , Time.now |> Task.perform GotStartTime
    )



-- UPDATE


type Msg
    = GotStartTime Time.Posix
    | ProgressAnimationRequested Time.Posix


type UpdateResult
    = Continue Model
    | Done


update : Msg -> Model -> UpdateResult
update msg model =
    case ( msg, model ) of
        ( GotStartTime startTime, Initialising { blocks } ) ->
            Continue <| EnteringGameOverMessage { blocks = blocks, startTime = startTime, percentComplete = 0 }

        ( GotStartTime _, _ ) ->
            Continue model

        ( ProgressAnimationRequested time, EnteringGameOverMessage animationModel ) ->
            progressAnimation time animationModel EnteringGameOverMessage (Just ShowingGameOverMessage)

        ( ProgressAnimationRequested time, ShowingGameOverMessage animationModel ) ->
            progressAnimation time animationModel ShowingGameOverMessage (Just FadingOut)

        ( ProgressAnimationRequested time, FadingOut animationModel ) ->
            progressAnimation time animationModel FadingOut Nothing

        ( ProgressAnimationRequested _, Initialising _ ) ->
            Continue model


progressAnimation : Time.Posix -> AnimationModel -> (AnimationModel -> Model) -> Maybe (AnimationModel -> Model) -> UpdateResult
progressAnimation time currentAnimationModel ifContinue ifDone =
    let
        percentComplete =
            toFloat (Time.posixToMillis time - Time.posixToMillis currentAnimationModel.startTime) * 100 / dropGameOverMessageTotalTimeMs
    in
    if percentComplete < 100 then
        { currentAnimationModel | percentComplete = percentComplete } |> ifContinue |> Continue

    else
        ifDone
            |> Maybe.map (\nextStage -> { currentAnimationModel | startTime = time, percentComplete = 0 } |> nextStage |> Continue)
            |> Maybe.withDefault Done



-- ANIMATION TIMINGS


dropGameOverMessageTotalTimeMs =
    1000


showGameOverMessageTotalTimeMs =
    2000


fadeOutTotalTimeMs =
    2000



-- VIEW


view : Model -> Element msg
view model =
    let
        { blocks_, messageOpacity, extraMessageGlow, entireOpacity } =
            case model of
                Initialising { blocks } ->
                    { blocks_ = blocks
                    , messageOpacity = 0
                    , extraMessageGlow = 0
                    , entireOpacity = 1
                    }

                EnteringGameOverMessage { blocks, percentComplete } ->
                    { blocks_ = blocks
                    , messageOpacity = percentComplete / 100
                    , extraMessageGlow = 0
                    , entireOpacity = 1
                    }

                ShowingGameOverMessage { blocks, percentComplete } ->
                    let
                        messageGlow =
                            if percentComplete < 50 then
                                percentComplete / 50 * 10

                            else
                                ((100 - percentComplete) / 50) * 10
                    in
                    { blocks_ = blocks
                    , messageOpacity = 1
                    , extraMessageGlow = messageGlow
                    , entireOpacity = 1
                    }

                FadingOut { blocks, percentComplete } ->
                    { blocks_ = blocks
                    , messageOpacity = 1
                    , extraMessageGlow = 0
                    , entireOpacity = 1 - (percentComplete / 100)
                    }

        boardView =
            BoardView.view UserGame.boardViewConfig blocks_ Nothing

        gameOverOverlay =
            Element.row
                [ Element.Border.width 2
                , Element.centerX
                , Element.centerY
                , Element.Background.color UIHelpers.buttonBorderColor
                , Element.padding 20
                , Element.Border.rounded 20
                , Element.Border.glow (Element.rgb255 200 200 200) (3 + extraMessageGlow)
                , Element.Font.size 32
                , Element.Font.extraBold
                , Element.Font.color UIHelpers.mainBackgroundColour
                , Element.Font.family [ Element.Font.typeface "Courier New" ]
                , Element.alpha messageOpacity
                ]
                [ Element.text "Game Over" ]
    in
    Element.el [ Element.inFront gameOverOverlay, Element.alpha entireOpacity ] boardView



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialising _ ->
            Sub.none

        _ ->
            -- TODO: is 50ms right here?
            Time.every 50 ProgressAnimationRequested
