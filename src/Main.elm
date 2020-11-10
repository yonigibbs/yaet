module Main exposing (main)

{-| This is the main module of the application. It renders the main UI (the instructions etc), delegating the rendering
of the actual game to `GameView`. It's responsible for passing user/timer events to the `Game` module for it to update
the game accordingly.
-}

import Array exposing (Array)
import BlockColour exposing (BlockColour)
import Browser
import Browser.Events
import Coord exposing (Coord)
import Game exposing (Game)
import GameView
import HighlightAnimation
import Html exposing (Html)
import Html.Events
import Keyboard
import Random
import Shape exposing (Shape)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Instructions, Cmd.none )



-- MODEL


type Model
    = Instructions -- No game being played - showing the user the instructions
    | Initialising -- Game being initialised (random shapes being generated)
    | Playing PlayingModel -- Game is currently being played
    | Ended -- Game has ended (TODO: add data here for rendering end state of game?)


{-| When the game is in the `Playing` state, this is the data associated with it.

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



-- UPDATE


type Msg
    = StartGameRequested -- User asked to start the game
    | Initialised Game.InitialisationInfo -- Game has been initialised
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Instructions, StartGameRequested ) ->
            initialiseGame

        ( Ended, StartGameRequested ) ->
            initialiseGame

        ( _, StartGameRequested ) ->
            ( model, Cmd.none )

        ( Initialising, Initialised initialisationInfo ) ->
            startNewGame initialisationInfo

        ( _, Initialised _ ) ->
            ( model, Cmd.none )

        ( Playing playingModel, RandomShapeGenerated shape ) ->
            ( Playing { playingModel | game = Game.shapeGenerated playingModel.game shape }, Cmd.none )

        ( _, RandomShapeGenerated _ ) ->
            ( model, Cmd.none )

        ( Playing playingModel, MoveShapeRequested direction ) ->
            Game.moveShape direction playingModel.game |> handleMoveResult playingModel

        ( _, MoveShapeRequested _ ) ->
            ( model, Cmd.none )

        ( Playing playingModel, RotateShapeRequested direction ) ->
            Game.rotateShape playingModel.game direction |> handleMoveResult playingModel

        ( _, RotateShapeRequested _ ) ->
            ( model, Cmd.none )

        ( Playing playingModel, TimerDropDelayElapsed ) ->
            Game.timerDrop playingModel.game |> handleMoveResult playingModel

        ( _, TimerDropDelayElapsed ) ->
            ( model, Cmd.none )

        ( _, HighlightAnimationMsg highlightAnimationMsg ) ->
            handleAnimationMsg model highlightAnimationMsg


{-| Initialises the game - this involves generating the random shapes required by the game to start.
-}
initialiseGame : ( Model, Cmd Msg )
initialiseGame =
    ( Initialising, Random.generate Initialised initialGameDataGenerator )


{-| Starts a new game after it's been initialised.
-}
startNewGame : Game.InitialisationInfo -> ( Model, Cmd msg )
startNewGame initialisationInfo =
    let
        newGame =
            Game.new initialisationInfo
    in
    ( Playing
        { game = newGame
        , timerDropDelay = 1000 -- TODO: hard-coded 1000 here - configurable? right value?
        , normalBlocks = (Game.blocks newGame).normal
        , highlightAnimation = Nothing -- We know initially there is nothing highlighted.
        , nextAnimationId = HighlightAnimation.initialId
        }
    , Cmd.none
    )


{-| Handles the result of a movement in the game, namely updates the model with the new game and, if required, initiates
the asynchronous generation of a new random shape (which is then added to the game's model later).
-}
handleMoveResult : PlayingModel -> Game.MoveResult -> ( Model, Cmd Msg )
handleMoveResult currentPlayingModel moveResult =
    case moveResult of
        Game.NoChange ->
            ( Playing currentPlayingModel, Cmd.none )

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
                    ( Playing
                        { currentPlayingModel | game = game, normalBlocks = nextBlocks.normal, highlightAnimation = Nothing }
                    , generateRandomShapeCmd
                    )

                _ ->
                    -- There are some blocks we need to animate, but this animation might already be in progress (e.g.
                    -- if a block is on the bottom row but the user moves it left/right - it can just continue its
                    -- current animation).
                    case currentPlayingModel.highlightAnimation of
                        Nothing ->
                            startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.ShapeLanding [ generateRandomShapeCmd ]

                        Just currentAnimation ->
                            if HighlightAnimation.highlightAnimationType currentAnimation == HighlightAnimation.ShapeLanding then
                                -- Just continue this current animation, but update the blocks on it
                                ( Playing
                                    { currentPlayingModel
                                        | game = game
                                        , normalBlocks = nextBlocks.normal
                                        , highlightAnimation = Just <| HighlightAnimation.withBlocks nextBlocks.highlighted currentAnimation
                                    }
                                , generateRandomShapeCmd
                                )

                            else
                                startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.ShapeLanding [ generateRandomShapeCmd ]

        Game.RowBeingRemoved { game } ->
            let
                nextBlocks =
                    Game.blocks game
            in
            startNewAnimation currentPlayingModel game nextBlocks HighlightAnimation.RowRemoval [ generateRandomShape ]

        Game.EndGame ->
            Debug.todo "Implement game end"


{-| Starts a new animation.
-}
startNewAnimation : PlayingModel -> Game -> Game.GameBlockInfo -> HighlightAnimation.Type -> List (Cmd Msg) -> ( Model, Cmd Msg )
startNewAnimation currentPlayingModel game blocks animationType extraCmds =
    let
        ( animationModel, animationMsg ) =
            HighlightAnimation.startNewAnimation
                currentPlayingModel.nextAnimationId
                animationType
                currentPlayingModel.timerDropDelay
                blocks.highlighted
    in
    ( Playing
        { currentPlayingModel
            | game = game
            , normalBlocks = blocks.normal
            , nextAnimationId = HighlightAnimation.nextAnimationId currentPlayingModel.nextAnimationId
            , highlightAnimation = Just animationModel
        }
    , Cmd.batch <| [ Cmd.map HighlightAnimationMsg animationMsg ] ++ extraCmds
    )


{-| Handles a message from the `HighlightAnimation` module. Passes the message to that module to handle then based on the
result from that function updates this module's model.
-}
handleAnimationMsg : Model -> HighlightAnimation.Msg -> ( Model, Cmd Msg )
handleAnimationMsg model msg =
    case model of
        Playing playingModel ->
            case playingModel.highlightAnimation of
                Just currentAnimation ->
                    case HighlightAnimation.update msg currentAnimation of
                        HighlightAnimation.IgnoreMsg ->
                            ( model, Cmd.none )

                        HighlightAnimation.Continue ( nextAnimationModel, nextAnimationCmd ) ->
                            ( Playing { playingModel | highlightAnimation = Just nextAnimationModel }
                            , Cmd.map HighlightAnimationMsg nextAnimationCmd
                            )

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
                                    ( Playing
                                        { playingModel
                                            | game = nextGame
                                            , timerDropDelay = max (playingModel.timerDropDelay - 10) 100
                                            , normalBlocks = (Game.blocks nextGame).normal
                                            , highlightAnimation = Nothing
                                        }
                                    , Cmd.none
                                    )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- RANDOM SHAPE GENERATION


{-| All the possible colours, in an array so that one can be randomly chosen from it.
-}
allColours : Array BlockColour
allColours =
    Array.fromList [ BlockColour.Blue, BlockColour.Red, BlockColour.Orange, BlockColour.Yellow, BlockColour.Purple, BlockColour.Green ]


{-| Functions for generating each of the possible colours, in an array so that one can be randomly chosen from it.
-}
allShapeBuilders : Array Shape.ShapeBuilder
allShapeBuilders =
    let
        ( first, rest ) =
            Shape.builders
    in
    Array.fromList (first :: rest)


randomColourGenerator : Random.Generator BlockColour
randomColourGenerator =
    Random.int 0 (Array.length allColours - 1)
        |> Random.map (\index -> Array.get index allColours |> Maybe.withDefault BlockColour.Blue)


randomShapeBuilderGenerator : Random.Generator Shape.ShapeBuilder
randomShapeBuilderGenerator =
    Random.int 0 (Array.length allShapeBuilders - 1)
        |> Random.map (\index -> Array.get index allShapeBuilders |> Maybe.withDefault (Tuple.first Shape.builders))


{-| Generator of random shapes. Requires a random colour and a random `ShapeBuilder` function. The latter is called,
receiving the result of the former, to eventually get a `Shape`.
-}
randomShapeGenerator : Random.Generator Shape
randomShapeGenerator =
    Random.pair randomColourGenerator randomShapeBuilderGenerator
        |> Random.map (\( colour, shapeBuilder ) -> shapeBuilder colour)


{-| Generator of a list of random shapes (of length 5). See the `Game` module's comments for details of the random shape
buffer.
-}
shapeBufferGenerator : Random.Generator (List Shape)
shapeBufferGenerator =
    Random.map5
        (\s1 s2 s3 s4 s5 -> [ s1, s2, s3, s4, s5 ])
        randomShapeGenerator
        randomShapeGenerator
        randomShapeGenerator
        randomShapeGenerator
        randomShapeGenerator


{-| Generator of the random data required to start a new game.
-}
initialGameDataGenerator : Random.Generator Game.InitialisationInfo
initialGameDataGenerator =
    Random.map3 Game.InitialisationInfo randomShapeGenerator randomShapeGenerator shapeBufferGenerator


generateRandomShape : Cmd Msg
generateRandomShape =
    Random.generate RandomShapeGenerated randomShapeGenerator



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Instructions ->
            Html.div [] [ Html.text "TODO: Instructions", Html.button [ Html.Events.onClick StartGameRequested ] [ Html.text "Start game" ] ]

        Initialising ->
            Html.text "TODO: Initialising"

        Playing { normalBlocks, highlightAnimation } ->
            GameView.view normalBlocks highlightAnimation

        Ended ->
            Html.text "TODO: Game ended"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Instructions ->
            Sub.none

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
            -- TODO: in some cases we don't want timerDropDelay, e.g. when animating?
            Sub.batch <|
                [ Browser.Events.onKeyDown <| Keyboard.keyEventDecoder keyMessages
                , subscription
                ]

        Ended ->
            Sub.none
