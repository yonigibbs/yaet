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


type HighlightAnimation
    = NoHighlightAnimation
    | HighlightAnimationPending { info : Game.HighlightedBlockInfo }
    | HighlightAnimationInProgress { info : Game.HighlightedBlockInfo, start : Time.Posix }


type alias PlayingModel =
    { game : Game, normalBlocks : List ( Block.Coord, Block.Colour ), highlightAnimation : HighlightAnimation }



-- UPDATE


type Msg
    = StartGameRequested -- User asked to start the game
    | Initialised Game.InitialisationInfo -- Game has been initialised
    | RandomShapeGenerated Shape -- Random shape was asked for (to add to the buffer) and is now ready
    | MoveShapeRequested Game.MoveDirection -- User clicked arrow to move currently dropping shape in given direction
    | RotateShapeRequested Shape.RotationDirection -- User pressed key to rotate currently dropping shape in given direction
    | TimerDropDelayElapsed -- Currently dropping shape should drop one row
    | NewAnimationRequested { highlightInfo : Game.HighlightedBlockInfo, start : Time.Posix }



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
            ( Playing { game = newGame, normalBlocks = (Game.blocks newGame).normal, highlightAnimation = NoHighlightAnimation }
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

        ( Playing playingModel, NewAnimationRequested request ) ->
            -- TODO: put in separate function
            case playingModel.highlightAnimation of
                HighlightAnimationPending pendingAnimation ->
                    -- TODO: is this logic right? Can we assume that if it's the same animation type then the message we
                    -- received is definitely for the current model, or could something have moved on since then?
                    if isSameHighlightInfo request.highlightInfo pendingAnimation.info then
                        ( Playing
                            { playingModel
                                | highlightAnimation =
                                    HighlightAnimationInProgress { info = request.highlightInfo, start = request.start }
                            }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( _, NewAnimationRequested _ ) ->
            ( model, Cmd.none )


{-| Initialises the game - this involves generating the random shapes required by the game to start.
-}
initialiseGame : ( Model, Cmd Msg )
initialiseGame =
    ( Initialising, Random.generate Initialised initialGameDataGenerator )


isSameHighlightInfo : Game.HighlightedBlockInfo -> Game.HighlightedBlockInfo -> Bool
isSameHighlightInfo info1 info2 =
    info1.highlightType == info2.highlightType


{-| Handles the result of a movement in the game, namely updates the model with the new game and, if required, initiates
the asynchronous generation of a new random shape (which is then added to the game's model later).
-}
handleMoveResult : PlayingModel -> Game.MoveResult -> ( Model, Cmd Msg )
handleMoveResult currentPlayingModel moveResult =
    case moveResult of
        Game.NoChange ->
            ( Playing currentPlayingModel, Cmd.none )

        Game.Continue continueResult ->
            let
                generateRandomShapeCmd : List (Cmd Msg)
                generateRandomShapeCmd =
                    if continueResult.newShapeRequested then
                        [ generateRandomShape ]

                    else
                        []

                nextBlocks =
                    Game.blocks continueResult.game

                buildStartAnimationCmd : Game.HighlightedBlockInfo -> List (Cmd Msg)
                buildStartAnimationCmd newHighlightInfo =
                    [ Task.perform
                        (\now -> NewAnimationRequested { highlightInfo = newHighlightInfo, start = now })
                        Time.now
                    ]

                maybeCurrentHighlight =
                    case currentPlayingModel.highlightAnimation of
                        NoHighlightAnimation ->
                            Nothing

                        HighlightAnimationPending animation ->
                            Just animation.info

                        HighlightAnimationInProgress animation ->
                            Just animation.info

                -- TODO: refactor all this - bit of a mess with duplication
                ( nextModelHighlightAnimation, startAnimationCmd ) =
                    case ( nextBlocks.highlighted, currentPlayingModel.highlightAnimation ) of
                        ( Just nextHighlightInfo, HighlightAnimationInProgress currentHighlightAnimation ) ->
                            -- Game told us there is some form of highlighting to be done, but we're already animating
                            -- some type of highlighting: check whether this is a new one (in which case we need to
                            -- reset all animation-related info).
                            if isSameHighlightInfo nextHighlightInfo currentHighlightAnimation.info then
                                -- The current type of animation should continue, potentially with new blocks (e.g. if
                                -- a block is as far down as it can go and the user quickly moves it before it's
                                -- considered as fully landed).
                                ( HighlightAnimationInProgress { currentHighlightAnimation | info = nextHighlightInfo }, [] )

                            else
                                -- New animation required - set the animation in the model to be pending (as we can't
                                -- start the animation till we have the current time), and initiate a task to get the
                                -- current time, after which the highlighting animation can start.
                                ( HighlightAnimationPending { info = nextHighlightInfo }
                                , buildStartAnimationCmd nextHighlightInfo
                                )

                        ( Just nextHighlightInfo, HighlightAnimationPending currentHighlightAnimation ) ->
                            -- Game told us there is some form of highlighting to be done, but we're pending animating
                            -- some type of highlighting: check whether this is a new one (in which case we need to
                            -- reset all animation-related info).
                            if isSameHighlightInfo nextHighlightInfo currentHighlightAnimation.info then
                                -- The current type of animation should continue, potentially with new blocks (e.g. if
                                -- a block is as far down as it can go and the user quickly moves it before it's
                                -- considered as fully landed).
                                ( HighlightAnimationPending { currentHighlightAnimation | info = nextHighlightInfo }, [] )

                            else
                                -- New animation required - set the animation in the model to be pending (as we can't
                                -- start the animation till we have the current time), and initiate a task to get the
                                -- current time, after which the highlighting animation can start.
                                ( HighlightAnimationPending { info = nextHighlightInfo }
                                , buildStartAnimationCmd nextHighlightInfo
                                )

                        ( Just nextHighlightInfo, NoHighlightAnimation ) ->
                            -- We aren't currently animating, but we now need to start.
                            ( HighlightAnimationPending { info = nextHighlightInfo }
                            , buildStartAnimationCmd nextHighlightInfo
                            )

                        ( Nothing, _ ) ->
                            -- No highlighting required
                            ( NoHighlightAnimation, [] )
            in
            ( Playing
                { currentPlayingModel
                    | game = continueResult.game
                    , normalBlocks = nextBlocks.normal
                    , highlightAnimation = nextModelHighlightAnimation
                }
            , List.concat [ generateRandomShapeCmd, startAnimationCmd ] |> Cmd.batch
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
                        HighlightAnimationInProgress { info } ->
                            Just { animationPercentComplete = 0, blocks = info.blocks }

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

        Playing { game } ->
            Sub.batch
                [ Time.every (Game.timerDropDelay game |> toFloat) <| always TimerDropDelayElapsed
                , Browser.Events.onKeyDown <| Keyboard.keyEventDecoder keyMessages
                ]

        Ended ->
            Sub.none
