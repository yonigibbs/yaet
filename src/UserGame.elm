module UserGame exposing (Model, Msg, UpdateResult(..), boardViewConfig, init, subscriptions, update, view)

{-| This module contains functionality related to a game being played by the user. The actual game logic itself is all
in the `Game` module, but that module itself is, in a sense, "inert" - it doesn't do anything by itself, instead requiring
a "controlling" module which will act on it (e.g. tell it to move a shape in some direction, generate a random shape
for it, etc). This module does that when the game is being controlled normally by the user (as opposed to, for example,
in a unit test). This module provides that control, and also the view for rendering the game screen to the user (with
the board and other related items such as a Pause button, etc.
-}

import BlockColour exposing (BlockColour)
import BoardView
import Coord exposing (Coord)
import Element exposing (Element)
import Game exposing (Game)
import GameBoard
import HighlightAnimation
import Process
import Random
import RandomShapeGenerator
import Shape exposing (Shape)
import Task
import Time
import UserGameControl



-- MODEL


type Model
    = Initialising
    | Playing PlayingModel


{-| The data associated with a game currently being played.

  - `game`: The current game, passed into the `Game` module whenever events occur such as the user moving a block, etc.
  - `timerDropDelay`: How long, in ms, before the currently dropping shape should be automatically dropped down a row.
    As the game continues this decrements to make the game speed up.
  - `normalBlocks`: The normal blocks which are to be rendered with no special effects (animation). These are calculated
    by calling `Game.blocks` (which calculates them based on the supplied `game`), so arguably it's a redundant duplication
    to store them in the model here, but this is done for performance reasons. `Game.blocks` has to do a bit of
    calculation to convert its own internal representation of the game's state to that required for rendering it, and we
    don't want that calculation to run multiple times a second when rendering animations, so we store it here. It's
    important that whenever `game` is updated with potentially new blocks, this field is correspondingly updated.
  - `highlightAnimation`: If any blocks are currently highlighted (e.g. because a shape is about to land) this contains
    the animation used to provide that highlighting. As for `normalBlocks`, this information can be calculated from the
    game, but for performance reasons is stored in the model.
  - `nextAnimationId`: The unique ID to use for the next animation. See the `Id` type in the `HighlightAnimation` module
    for more info on this.

-}
type alias PlayingModel =
    { game : Game
    , timerDropDelay : Int
    , timerDropMessageId : Int
    , normalBlocks : List ( Coord, BlockColour )
    , highlightAnimation : Maybe HighlightAnimation.Model
    , nextAnimationId : HighlightAnimation.Id
    , gameControl : UserGameControl.Model
    }


{-| Initialises the game - this involves generating the random shapes required by the game to start.
-}
init : ( Model, Cmd Msg )
init =
    ( Initialising, Random.generate Initialised initialGameDataGenerator )



-- UPDATE


type Msg
    = Initialised Game.InitialisationInfo -- Game has been initialised
    | RandomShapeGenerated Shape -- Random shape was asked for (to add to the buffer) and is now ready
    | GotGameControlMsg UserGameControl.Msg -- User requested some actions, e.g. clicked arrow to move or rotate currently dropping shape.
    | TimerDropDelayElapsed Int -- Currently dropping shape should drop one row
    | HighlightAnimationMsg HighlightAnimation.Msg -- A message from the `HighlightAnimation` module as occurred: this is passed to that module to handle.


{-| Data returned from the `update` function detailing anything the calling module needs to know, e.g. whether the game
is now over or not.
-}
type UpdateResult
    = Continue Model (Cmd Msg)
    | GameOver Game


update : Msg -> Model -> UpdateResult
update msg model =
    case ( model, msg ) of
        ( Initialising, Initialised initialisationInfo ) ->
            startNewGame initialisationInfo |> continueFromModelCmdTuple

        ( _, Initialised _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, RandomShapeGenerated shape ) ->
            Continue (Playing { playingModel | game = Game.shapeGenerated playingModel.game shape }) Cmd.none

        ( _, RandomShapeGenerated _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, GotGameControlMsg gameControlMsg ) ->
            handleGameControlMsg playingModel gameControlMsg

        ( _, GotGameControlMsg _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, TimerDropDelayElapsed id ) ->
            if playingModel.timerDropMessageId == id then
                Game.timerDrop playingModel.game |> handleMoveResult playingModel True

            else
                Continue model Cmd.none

        ( _, TimerDropDelayElapsed _ ) ->
            Continue model Cmd.none

        ( _, HighlightAnimationMsg highlightAnimationMsg ) ->
            handleAnimationMsg model highlightAnimationMsg


handleGameControlMsg : PlayingModel -> UserGameControl.Msg -> UpdateResult
handleGameControlMsg playingModel gameControlMsg =
    let
        ( nextGameControlModel, actionsToExecute ) =
            UserGameControl.update playingModel.gameControl gameControlMsg

        moveResult =
            Game.executeUserActions playingModel.game actionsToExecute

        startNewTimerDropDelay =
            case moveResult of
                Game.Continue { shapeDropped } ->
                    -- If a shape was dropped then reset the timer drop delay
                    shapeDropped

                _ ->
                    False

        newTimerDropSubscriptionId =
            if startNewTimerDropDelay then
                playingModel.timerDropMessageId + 1

            else
                playingModel.timerDropMessageId
    in
    moveResult
        |> handleMoveResult
            { playingModel
                | gameControl = nextGameControlModel
                , timerDropMessageId = newTimerDropSubscriptionId
            }
            startNewTimerDropDelay


{-| Starts a new game after it's been initialised.
-}
startNewGame : Game.InitialisationInfo -> ( Model, Cmd Msg )
startNewGame initialisationInfo =
    let
        newGame =
            Game.new initialisationInfo

        playingModel =
            { game = newGame
            , timerDropDelay = 1000 -- TODO: hard-coded 1000 here - configurable? right value?
            , timerDropMessageId = 0
            , normalBlocks = (Game.blocks newGame).normal
            , highlightAnimation = Nothing -- We know initially there is nothing highlighted.
            , nextAnimationId = HighlightAnimation.initialId
            , gameControl = UserGameControl.init
            }
    in
    ( Playing playingModel, timerDropDelayCmd playingModel )


timerDropDelayCmd : PlayingModel -> Cmd Msg
timerDropDelayCmd { timerDropDelay, timerDropMessageId } =
    Process.sleep (toFloat timerDropDelay)
        |> Task.perform (always <| TimerDropDelayElapsed timerDropMessageId)


{-| Handles the result of a movement in the game, namely updates the model with the new game and, if required, initiates
the asynchronous generation of a new random shape (which is then added to the game's model later).
-}
handleMoveResult : PlayingModel -> Bool -> Game.MoveResult -> UpdateResult
handleMoveResult currentPlayingModel startNewTimerDropDelay moveResult =
    let
        allCmds : List (Cmd Msg) -> Cmd Msg
        allCmds cmds =
            if startNewTimerDropDelay then
                Cmd.batch <| timerDropDelayCmd currentPlayingModel :: cmds

            else
                Cmd.batch cmds
    in
    case moveResult of
        Game.NoChange ->
            Continue (Playing currentPlayingModel) (allCmds [])

        Game.Continue { game, newShapeRequested } ->
            let
                nextBlocks =
                    Game.blocks game

                generateRandomShapeCmd =
                    if newShapeRequested then
                        generateRandomShape

                    else
                        Cmd.none
            in
            case nextBlocks.highlighted of
                [] ->
                    -- No animation required as there are no highlighted blocks
                    Continue
                        (Playing
                            { currentPlayingModel | game = game, normalBlocks = nextBlocks.normal, highlightAnimation = Nothing }
                        )
                        (allCmds [ generateRandomShapeCmd ])

                _ ->
                    -- There are some blocks we need to animate, but this animation might already be in progress (e.g.
                    -- if a block is on the bottom row but the user moves it left/right - it can just continue its
                    -- current animation).
                    case currentPlayingModel.highlightAnimation of
                        Nothing ->
                            ( startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.ShapeLanding
                            , allCmds [ generateRandomShapeCmd ]
                            )
                                |> continueFromModelCmdTuple

                        Just currentAnimation ->
                            if HighlightAnimation.highlightAnimationType currentAnimation == HighlightAnimation.ShapeLanding then
                                -- Just continue this current animation, but update the blocks on it
                                Continue
                                    (Playing
                                        { currentPlayingModel
                                            | game = game
                                            , normalBlocks = nextBlocks.normal
                                            , highlightAnimation =
                                                Just <| HighlightAnimation.withBlocks nextBlocks.highlighted currentAnimation
                                        }
                                    )
                                    (allCmds [ generateRandomShapeCmd ])

                            else
                                ( startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.ShapeLanding
                                , allCmds [ generateRandomShapeCmd ]
                                )
                                    |> continueFromModelCmdTuple

        Game.RowBeingRemoved { game } ->
            let
                nextBlocks =
                    Game.blocks game
            in
            ( startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.RowRemoval
            , allCmds [ generateRandomShape ]
            )
                |> continueFromModelCmdTuple

        Game.GameOver { game } ->
            GameOver game


continueFromModelCmdTuple : ( Model, Cmd Msg ) -> UpdateResult
continueFromModelCmdTuple ( model, cmd ) =
    Continue model cmd


{-| Starts a new animation.
-}
startNewAnimation : PlayingModel -> Game -> Game.GameBlockInfo -> HighlightAnimation.Type -> Model
startNewAnimation currentPlayingModel game blocks animationType =
    let
        animationModel =
            HighlightAnimation.startNewAnimation
                currentPlayingModel.nextAnimationId
                animationType
                (totalAnimationTimeForType animationType currentPlayingModel)
                blocks.highlighted
    in
    Playing
        { currentPlayingModel
            | game = game
            , normalBlocks = blocks.normal
            , nextAnimationId = HighlightAnimation.nextAnimationId currentPlayingModel.nextAnimationId
            , highlightAnimation = Just animationModel
        }


{-| Calculates the total time use for an animation of the given type.
-}
totalAnimationTimeForType : HighlightAnimation.Type -> { a | timerDropDelay : Int } -> Int
totalAnimationTimeForType animationType { timerDropDelay } =
    case animationType of
        HighlightAnimation.ShapeLanding ->
            timerDropDelay

        HighlightAnimation.RowRemoval ->
            150


{-| Handles a message from the `HighlightAnimation` module. Passes the message to that module to handle then based on the
result from that function updates this module's model.
-}
handleAnimationMsg : Model -> HighlightAnimation.Msg -> UpdateResult
handleAnimationMsg model msg =
    case model of
        Playing playingModel ->
            case playingModel.highlightAnimation of
                Just currentAnimation ->
                    case HighlightAnimation.update msg currentAnimation of
                        HighlightAnimation.IgnoreMsg ->
                            Continue model Cmd.none

                        HighlightAnimation.Continue nextAnimationModel ->
                            Continue (Playing { playingModel | highlightAnimation = Just nextAnimationModel }) Cmd.none

                        HighlightAnimation.Complete ->
                            case HighlightAnimation.highlightAnimationType currentAnimation of
                                HighlightAnimation.ShapeLanding ->
                                    -- We've finished animating a shape landing: stop animating. Whenever the timerDrop
                                    -- subscription fires next the game will move on.
                                    Continue (Playing { playingModel | highlightAnimation = Nothing }) Cmd.none

                                HighlightAnimation.RowRemoval ->
                                    -- When we've finished animating rows about to be removed, call
                                    -- `onRowRemovalAnimationComplete` which will remove those rows from the board and
                                    -- return the game to its regular state.
                                    let
                                        nextGame =
                                            Game.onRowRemovalAnimationComplete playingModel.game
                                    in
                                    Continue
                                        (Playing
                                            { playingModel
                                                | game = nextGame
                                                , timerDropDelay = max (playingModel.timerDropDelay - 10) 100
                                                , normalBlocks = (Game.blocks nextGame).normal
                                                , highlightAnimation = Nothing
                                            }
                                        )
                                        Cmd.none

                Nothing ->
                    Continue model Cmd.none

        _ ->
            Continue model Cmd.none



-- RANDOM SHAPE GENERATION


generateRandomShape : Cmd Msg
generateRandomShape =
    Random.generate RandomShapeGenerated RandomShapeGenerator.generator


{-| Generator of the random data required to start a new game.
-}
initialGameDataGenerator : Random.Generator Game.InitialisationInfo
initialGameDataGenerator =
    Random.map3 Game.InitialisationInfo RandomShapeGenerator.generator RandomShapeGenerator.generator shapeBufferGenerator


{-| Generator of a list of random shapes (of length 5). See the `Game` module's comments for details of the random shape
buffer.
-}
shapeBufferGenerator : Random.Generator (List Shape)
shapeBufferGenerator =
    Random.map5
        (\s1 s2 s3 s4 s5 -> [ s1, s2, s3, s4, s5 ])
        RandomShapeGenerator.generator
        RandomShapeGenerator.generator
        RandomShapeGenerator.generator
        RandomShapeGenerator.generator
        RandomShapeGenerator.generator



-- VIEW


view : Model -> Element msg
view model =
    let
        boardView =
            case model of
                Initialising ->
                    BoardView.view boardViewConfig [] Nothing

                Playing { normalBlocks, highlightAnimation } ->
                    BoardView.view boardViewConfig
                        (BoardView.withOpacity 1 normalBlocks)
                        highlightAnimation
    in
    Element.column [] [ boardView ]


{-| The configuration required to render the game.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 30, rowCount = GameBoard.rowCount, colCount = GameBoard.colCount, borderStyle = BoardView.Solid }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialising ->
            Sub.none

        Playing { game, gameControl, timerDropDelay, timerDropMessageId, highlightAnimation } ->
            let
                animationSubscription =
                    highlightAnimation
                        |> Maybe.map
                            (\animation -> [ HighlightAnimation.subscriptions animation |> Sub.map HighlightAnimationMsg ])
                        |> Maybe.withDefault []
            in
            Sub.batch <|
                [ UserGameControl.subscriptions gameControl |> Sub.map GotGameControlMsg ]
                    ++ animationSubscription
