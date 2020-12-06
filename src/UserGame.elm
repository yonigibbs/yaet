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
import Browser.Events
import Coord exposing (Coord)
import Element exposing (Element)
import Game exposing (Game)
import GameBoard
import HighlightAnimation
import Keyboard
import Random
import RandomShapeGenerator
import Shape exposing (Shape)
import Time



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
    , normalBlocks : List ( Coord, BlockColour )
    , highlightAnimation : Maybe HighlightAnimation.Model
    , nextAnimationId : HighlightAnimation.Id
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
    | MoveShapeRequested Game.MoveDirection -- User clicked arrow to move currently dropping shape in given direction
    | RotateShapeRequested Shape.RotationDirection -- User pressed key to rotate currently dropping shape in given direction
    | TimerDropDelayElapsed -- Currently dropping shape should drop one row
    | HighlightAnimationMsg HighlightAnimation.Msg -- A message from the `HighlightAnimation` module as occurred: this is passed to that module to handle.


{-| Mapping of keyboard buttons to corresponding messages.
-}
keyMessages : Keyboard.KeyMessages Msg
keyMessages =
    { moveLeft = MoveShapeRequested Game.Left
    , moveRight = MoveShapeRequested Game.Right
    , dropOneRow = MoveShapeRequested Game.Down
    , rotateClockwise = RotateShapeRequested Shape.Clockwise
    , rotateAnticlockwise = RotateShapeRequested Shape.Anticlockwise
    }


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
            Continue (startNewGame initialisationInfo) Cmd.none

        ( _, Initialised _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, RandomShapeGenerated shape ) ->
            Continue (Playing { playingModel | game = Game.shapeGenerated playingModel.game shape }) Cmd.none

        ( _, RandomShapeGenerated _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, MoveShapeRequested direction ) ->
            Game.moveShape direction playingModel.game |> handleMoveResult playingModel

        ( _, MoveShapeRequested _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, RotateShapeRequested direction ) ->
            Game.rotateShape direction playingModel.game |> handleMoveResult playingModel

        ( _, RotateShapeRequested _ ) ->
            Continue model Cmd.none

        ( Playing playingModel, TimerDropDelayElapsed ) ->
            Game.timerDrop playingModel.game |> handleMoveResult playingModel

        ( _, TimerDropDelayElapsed ) ->
            Continue model Cmd.none

        ( _, HighlightAnimationMsg highlightAnimationMsg ) ->
            handleAnimationMsg model highlightAnimationMsg


{-| Starts a new game after it's been initialised.
-}
startNewGame : Game.InitialisationInfo -> Model
startNewGame initialisationInfo =
    let
        newGame =
            Game.new initialisationInfo
    in
    Playing
        { game = newGame
        , timerDropDelay = 1000 -- TODO: hard-coded 1000 here - configurable? right value?
        , normalBlocks = (Game.blocks newGame).normal
        , highlightAnimation = Nothing -- We know initially there is nothing highlighted.
        , nextAnimationId = HighlightAnimation.initialId
        }


{-| Handles the result of a movement in the game, namely updates the model with the new game and, if required, initiates
the asynchronous generation of a new random shape (which is then added to the game's model later).
-}
handleMoveResult : PlayingModel -> Game.MoveResult -> UpdateResult
handleMoveResult currentPlayingModel moveResult =
    case moveResult of
        Game.NoChange ->
            Continue (Playing currentPlayingModel) Cmd.none

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
                        generateRandomShapeCmd

                _ ->
                    -- There are some blocks we need to animate, but this animation might already be in progress (e.g.
                    -- if a block is on the bottom row but the user moves it left/right - it can just continue its
                    -- current animation).
                    case currentPlayingModel.highlightAnimation of
                        Nothing ->
                            ( startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.ShapeLanding, generateRandomShapeCmd )
                                |> continueFromModelCmdTuple

                        Just currentAnimation ->
                            if HighlightAnimation.highlightAnimationType currentAnimation == HighlightAnimation.ShapeLanding then
                                -- Just continue this current animation, but update the blocks on it
                                Continue
                                    (Playing
                                        { currentPlayingModel
                                            | game = game
                                            , normalBlocks = nextBlocks.normal
                                            , highlightAnimation = Just <| HighlightAnimation.withBlocks nextBlocks.highlighted currentAnimation
                                        }
                                    )
                                    generateRandomShapeCmd

                            else
                                ( startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.ShapeLanding, generateRandomShapeCmd )
                                    |> continueFromModelCmdTuple

        Game.RowBeingRemoved { game } ->
            let
                nextBlocks =
                    Game.blocks game
            in
            ( startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.RowRemoval, generateRandomShape )
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
                                    -- When we've finished animating a shape landing, just call `timerDrop`, which will
                                    -- "land" that shape and move the game forward (adding a new dropping shape, etc).
                                    Game.timerDrop playingModel.game |> handleMoveResult playingModel

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

        Playing { game, timerDropDelay, highlightAnimation } ->
            let
                subscription =
                    case highlightAnimation of
                        Just animation ->
                            -- We're in the process of animating something so don't need the `timerDrop` event to occur:
                            -- instead we use the end of the animation to know how to move the game on.
                            Sub.map HighlightAnimationMsg <| HighlightAnimation.subscriptions animation

                        _ ->
                            Time.every (toFloat timerDropDelay) <| always TimerDropDelayElapsed
            in
            Sub.batch
                [ Browser.Events.onKeyDown <| Keyboard.keyEventDecoder keyMessages
                , subscription
                ]
