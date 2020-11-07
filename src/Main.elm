module Main exposing (main)

import Array exposing (Array)
import Block
import Browser
import Browser.Events
import Game exposing (Game)
import GameRender
import Html exposing (Html)
import Html.Events
import Keyboard
import Random
import Shape exposing (Shape)
import Task
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



-- TODO: put animation-related stuff in its own module?


type HighlightAnimationType
    = ShapeLanding
    | LineRemoval


type HighlightAnimation
    = NotAnimating
    | Animating
        { id : Int
        , animationType : HighlightAnimationType
        , totalTimeMs : Int
        , blocks : List ( Block.Coord, Block.Colour )

        -- TODO: would this be better as an AnimationStatus type maybe?
        , progress : Maybe { start : Time.Posix, percentComplete : Float }
        }


type alias PlayingModel =
    { game : Game
    , timerDropDelay : Int -- How long, in ms, before the currently dropping shape should be automatically dropped down a row.
    , normalBlocks : List ( Block.Coord, Block.Colour )
    , highlightAnimation : HighlightAnimation
    , nextAnimationId : Int
    }



-- UPDATE


type Msg
    = StartGameRequested -- User asked to start the game
    | Initialised Game.InitialisationInfo -- Game has been initialised
    | RandomShapeGenerated Shape -- Random shape was asked for (to add to the buffer) and is now ready
    | MoveShapeRequested Game.MoveDirection -- User clicked arrow to move currently dropping shape in given direction
    | RotateShapeRequested Shape.RotationDirection -- User pressed key to rotate currently dropping shape in given direction
    | TimerDropDelayElapsed -- Currently dropping shape should drop one row
    | HighlightAnimationStartTimeAvailable { id : Int, start : Time.Posix }
    | HighlightAnimationFrame { id : Int, time : Time.Posix }



-- New highlighting animation ready to start (as we have current time)


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
            let
                newGame =
                    Game.new initialisationInfo
            in
            -- TODO: hard-coded 1000 here - configurable? right value?
            ( Playing
                { game = newGame
                , timerDropDelay = 1000
                , normalBlocks = (Game.blocks newGame).normal
                , highlightAnimation = NotAnimating
                , nextAnimationId = 0
                }
            , Cmd.none
            )

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

        ( Playing playingModel, HighlightAnimationStartTimeAvailable request ) ->
            -- TODO: put in separate function and refactor a bit - helper functions etc
            case playingModel.highlightAnimation of
                Animating currentAnimation ->
                    if request.id == currentAnimation.id then
                        ( Playing
                            { playingModel
                                | highlightAnimation =
                                    Animating
                                        { currentAnimation | progress = Just { percentComplete = 0, start = request.start } }
                            }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( _, HighlightAnimationStartTimeAvailable _ ) ->
            ( model, Cmd.none )

        ( Playing playingModel, HighlightAnimationFrame frame ) ->
            -- TODO: put in separate function
            case playingModel.highlightAnimation of
                Animating currentAnimation ->
                    if currentAnimation.id == frame.id then
                        case currentAnimation.progress of
                            Just progress ->
                                let
                                    newPercentComplete =
                                        toFloat (Time.posixToMillis frame.time - Time.posixToMillis progress.start)
                                            / toFloat currentAnimation.totalTimeMs
                                            * 100

                                    newHighlightAnimation =
                                        if newPercentComplete < 100 then
                                            Animating
                                                { currentAnimation
                                                    | progress =
                                                        Just
                                                            { progress
                                                                | percentComplete =
                                                                    newPercentComplete
                                                            }
                                                }

                                        else
                                            NotAnimating
                                in
                                ( Playing { playingModel | highlightAnimation = newHighlightAnimation }
                                , Cmd.none
                                )

                            Nothing ->
                                ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( _, HighlightAnimationFrame _ ) ->
            ( model, Cmd.none )


{-| Initialises the game - this involves generating the random shapes required by the game to start.
-}
initialiseGame : ( Model, Cmd Msg )
initialiseGame =
    ( Initialising, Random.generate Initialised initialGameDataGenerator )


{-| Handles the result of a movement in the game, namely updates the model with the new game and, if required, initiates
the asynchronous generation of a new random shape (which is then added to the game's model later).
-}
handleMoveResult : PlayingModel -> Game.MoveResult -> ( Model, Cmd Msg )
handleMoveResult currentPlayingModel moveResult =
    -- TODO: refactor and split out
    case moveResult of
        Game.NoChange ->
            ( Playing currentPlayingModel, Cmd.none )

        Game.Continue nextGame ->
            let
                nextBlocks =
                    Game.blocks nextGame

                startNewAnimation =
                    ( Playing
                        { currentPlayingModel
                            | game = nextGame
                            , normalBlocks = nextBlocks.normal
                            , nextAnimationId = currentPlayingModel.nextAnimationId + 1
                            , highlightAnimation =
                                Animating
                                    { id = currentPlayingModel.nextAnimationId
                                    , animationType = ShapeLanding
                                    , totalTimeMs = currentPlayingModel.timerDropDelay
                                    , blocks = nextBlocks.highlighted
                                    , progress = Nothing
                                    }
                        }
                    , Task.perform
                        (\now ->
                            HighlightAnimationStartTimeAvailable { id = currentPlayingModel.nextAnimationId, start = now }
                        )
                        Time.now
                    )
            in
            case nextBlocks.highlighted of
                [] ->
                    -- No animation required as there are no highlighted blocks
                    ( Playing
                        { currentPlayingModel | game = nextGame, normalBlocks = nextBlocks.normal, highlightAnimation = NotAnimating }
                    , Cmd.none
                    )

                _ ->
                    -- There are some blocks we need to animate, but this animation might already be in progress (e.g.
                    -- if a block is on the bottom row but the user moves it left/right - it can just continue its
                    -- current animation).
                    case currentPlayingModel.highlightAnimation of
                        NotAnimating ->
                            startNewAnimation

                        Animating currentAnimation ->
                            if currentAnimation.animationType == ShapeLanding then
                                -- Just continue this current animation, but update the blocks on it
                                ( Playing
                                    { currentPlayingModel
                                        | game = nextGame
                                        , normalBlocks = nextBlocks.normal
                                        , highlightAnimation = Animating { currentAnimation | blocks = nextBlocks.highlighted }
                                    }
                                , Cmd.none
                                )

                            else
                                startNewAnimation

        Game.LineBeingRemoved nextGame ->
            let
                nextBlocks =
                    Game.blocks nextGame
            in
            ( Playing
                { currentPlayingModel
                    | game = nextGame
                    , normalBlocks = nextBlocks.normal
                    , nextAnimationId = currentPlayingModel.nextAnimationId + 1
                    , highlightAnimation =
                        Animating
                            { id = currentPlayingModel.nextAnimationId
                            , animationType = LineRemoval
                            , totalTimeMs = 500 -- TODO: hard-coded magic constant
                            , blocks = nextBlocks.highlighted
                            , progress = Nothing
                            }
                }
            , Cmd.batch
                [ Task.perform
                    (\now ->
                        HighlightAnimationStartTimeAvailable { id = currentPlayingModel.nextAnimationId, start = now }
                    )
                    Time.now
                , generateRandomShape
                ]
            )

        Game.EndGame ->
            Debug.todo "Implement game end"



-- RANDOM SHAPE GENERATION


{-| All the possible colours, in an array so that one can be randomly chosen from it.
-}
allColours : Array Block.Colour
allColours =
    Array.fromList [ Block.Blue, Block.Red, Block.Orange, Block.Yellow, Block.Purple, Block.Green ]


{-| Functions for generating each of the possible colours, in an array so that one can be randomly chosen from it.
-}
allShapeBuilders : Array Shape.ShapeBuilder
allShapeBuilders =
    let
        ( first, rest ) =
            Shape.builders
    in
    Array.fromList (first :: rest)


randomColourGenerator : Random.Generator Block.Colour
randomColourGenerator =
    Random.int 0 (Array.length allColours - 1)
        |> Random.map (\index -> Array.get index allColours |> Maybe.withDefault Block.Blue)


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
            let
                requestHighlightInfo =
                    case highlightAnimation of
                        Animating { blocks, progress } ->
                            Just
                                { animationPercentComplete = Maybe.map .percentComplete progress |> Maybe.withDefault 0
                                , blocks = blocks
                                }

                        _ ->
                            Nothing
            in
            GameRender.render { normalBlocks = normalBlocks, highlighted = requestHighlightInfo }

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
                animationFrameSub =
                    case highlightAnimation of
                        Animating { id } ->
                            -- TODO: is 50ms the right value here?
                            [ Time.every 50
                                (\now ->
                                    HighlightAnimationFrame { id = id, time = now }
                                )
                            ]

                        _ ->
                            []
            in
            Sub.batch <|
                [ Time.every (toFloat timerDropDelay) <| always TimerDropDelayElapsed
                , Browser.Events.onKeyDown <| Keyboard.keyEventDecoder keyMessages
                ]
                    ++ animationFrameSub

        Ended ->
            Sub.none
