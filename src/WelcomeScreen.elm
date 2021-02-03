module WelcomeScreen exposing (Model, Msg, UpdateResult(..), getSettings, init, subscriptions, update, view)

{-| This module contains all functionality related to the welcome screen. Manages the animated board and functionality
available from the Welcome screen, e.g. the Settings.
-}

import Array
import BoardView
import Coord exposing (Coord)
import DroppingShape exposing (DroppingShape)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import HighlightAnimation
import Random
import Random.Array
import Settings exposing (Settings)
import SettingsScreen
import Shape exposing (Shape)
import Task
import Time
import UIHelpers exposing (edges)



-- MODEL


{-| Represents a letter in the word "TETRIS", as a shape on a board, much like a normal Tetris shape.
-}
type alias Letter =
    { blocks : List Coord, colour : Shape.BlockColour, gridCoord : Coord }


{-| The data associated with the `DroppingLetters` variant of `AnimatedBoard`. Defines the data required to show the
welcome screen at the stage where the letters of Tetris are dropping onto the board one by one.

  - `landed`: the letters which have already landed.
  - `dropping`: the letter which is currently dropping down.
  - `next`: the letters which have yet to start dropping.
  - `randomSeed`: the seed to use to generate random colours. Also passed through to subsequent stages of the welcome
    screen so they have access to a random seed when they need randomness. This is initialised based on the current
    system time, using the `Initialised` message.

-}
type alias DroppingLettersData =
    { landed : List Letter
    , dropping : Letter
    , next : List Letter
    , randomSeed : Random.Seed
    }


{-| The data associated with the `PulsingLetters` variant of `AnimatedBoard`. Defines the data required to show the
welcome screen at the stage where the letters of Tetris have already landed and are now being pulsed (faded out then back
in briefly). This stage doesn't actually use the `randomSeed` value but stores it so it can be passed to the subsequent
stage (`DroppingRandomShapes`) which does need it.
-}
type alias PulsingLettersData =
    { letters : List Letter
    , animation : HighlightAnimation.Model
    , randomSeed : Random.Seed
    }


{-| The data associated with the `DroppingRandomShapes` variant of `AnimatedBoard`. Defines the data required to show the
welcome screen at the stage where the letters of Tetris have landed and been pulsed, and now random shapes drop down
"behind" those letters.

  - `letters`: the letters of Tetris, which are rendered stationary.
  - `droppingShapes`: the shapes currently dropping down.
  - `shapeBuffer`: the buffer of shapes used to get a new shape whenever required.
  - `randomSeed`: the seed to use to generate random colours and starting positions for the dropping shapes. Also used
    to initialise the shape buffer.

-}
type alias DroppingRandomShapesData =
    { letters : List Letter
    , droppingShapes : List DroppingShape
    , shapeBuffer : Shape.Bag
    , randomSeed : Random.Seed
    }


{-| The model of this module, exposed as an opaque type.
-}
type Model
    = Model ModelData


type alias ModelData =
    { animatedBoard : AnimatedBoard, settings : Settings, settingsScreen : Maybe SettingsScreen.Model }


{-| The state of the animated board on the Welcome screen. Defines the three stages of the animation, along with an
`Initialising` state, used purely to get the current time to use a random seed:

  - `DroppingLetters`: The letters of the word "Tetris" are dropping onto the board, one by one.
  - `PulsingLetters`: The letters of the word "Tetris" are being "pulsed" (faded out then back in).
  - `DroppingRandomShapes`: Random shapes are dropping from the top of the board down till they disappear, behind the
    letters of Tetris.

-}
type AnimatedBoard
    = Initialising
    | DroppingLetters DroppingLettersData
    | PulsingLetters PulsingLettersData
    | DroppingRandomShapes DroppingRandomShapesData


init : Settings -> ( Model, Cmd Msg )
init settings =
    ( Model { animatedBoard = Initialising, settings = settings, settingsScreen = Nothing }
    , Time.now |> Task.perform (Time.posixToMillis >> Random.initialSeed >> Initialised)
    )



-- UPDATE


type Msg
    = Initialised Random.Seed -- Ready to start the board animation (the supplied value is used as a random seed for various aspects)
    | LetterDropAnimationFrame -- A letter should be dropped another row (or a new letter added)
    | GotHighlightAnimationMsg HighlightAnimation.Msg -- A pulsing animation frame has occurred
    | ShapeDropDelayElapsed -- The delay between each time the dropping shapes are lowered a row has elapsed
    | ShowSettingsRequested -- The user has requested to see the Settings modal
    | GotSettingsScreenMsg SettingsScreen.Msg -- The user has clicked Save on the settings modal
    | StartGameRequested


type UpdateResult
    = Stay
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg, UpdateResult )
update msg ((Model { animatedBoard }) as model) =
    case ( msg, animatedBoard ) of
        ( Initialised randomSeed, Initialising ) ->
            ( model |> withAnimatedBoard (initDroppingLetters randomSeed |> DroppingLetters)
            , Cmd.none
            , Stay
            )

        ( Initialised _, _ ) ->
            ( model, Cmd.none, Stay )

        ( LetterDropAnimationFrame, DroppingLetters data ) ->
            ( model |> withAnimatedBoard (onLetterDropAnimationFrame data)
            , Cmd.none
            , Stay
            )

        ( LetterDropAnimationFrame, _ ) ->
            ( model, Cmd.none, Stay )

        ( GotHighlightAnimationMsg highlightAnimationMsg, PulsingLetters data ) ->
            ( model |> withAnimatedBoard (onPulsingLettersAnimationFrame animatedBoard highlightAnimationMsg data)
            , Cmd.none
            , Stay
            )

        ( GotHighlightAnimationMsg _, _ ) ->
            ( model, Cmd.none, Stay )

        ( ShapeDropDelayElapsed, DroppingRandomShapes data ) ->
            ( model |> withAnimatedBoard (handleShapeDropDelayElapsed data |> DroppingRandomShapes)
            , Cmd.none
            , Stay
            )

        ( ShapeDropDelayElapsed, _ ) ->
            ( model, Cmd.none, Stay )

        ( ShowSettingsRequested, _ ) ->
            ( showSettingsScreen model
            , Cmd.none
            , Stay
            )

        ( GotSettingsScreenMsg subMsg, _ ) ->
            handleSettingsScreenMsg subMsg model

        ( StartGameRequested, _ ) ->
            ( model, Cmd.none, StartGame )


withAnimatedBoard : AnimatedBoard -> Model -> Model
withAnimatedBoard animatedBoard (Model modelData) =
    Model { modelData | animatedBoard = animatedBoard }


showSettingsScreen : Model -> Model
showSettingsScreen (Model ({ settings } as modelData)) =
    Model { modelData | settingsScreen = Just <| SettingsScreen.init settings }


handleSettingsScreenMsg : SettingsScreen.Msg -> Model -> ( Model, Cmd Msg, UpdateResult )
handleSettingsScreenMsg settingsMsg ((Model modelData) as model) =
    case modelData.settingsScreen of
        Just settingsScreen ->
            let
                ( settingsModel, settingsCmd, settingsUpdateResult ) =
                    SettingsScreen.update settingsMsg settingsScreen

                nextModelData =
                    case settingsUpdateResult of
                        SettingsScreen.KeepOpen ->
                            { modelData | settingsScreen = Just settingsModel }

                        SettingsScreen.Close maybeNewSettings ->
                            -- Close settings screen, possibly updating the settings
                            { modelData | settings = maybeNewSettings |> Maybe.withDefault modelData.settings, settingsScreen = Nothing }
            in
            ( Model nextModelData, Cmd.map GotSettingsScreenMsg settingsCmd, Stay )

        Nothing ->
            ( model, Cmd.none, Stay )


{-| Gets the initial state of the `DroppingLetters` state of the screen. Gets all the letters ready to drop, along with
a random colour for each (using the supplied seed).
-}
initDroppingLetters : Random.Seed -> DroppingLettersData
initDroppingLetters randomSeed =
    let
        -- We use the seed to get the random colours, and then keep a hold of that same seed which is used later for a
        -- different purpose (to initialise the shape buffer for the dropping shapes, for example).
        randomColours =
            Random.step (Shape.allColours |> Array.fromList |> Random.Array.shuffle) randomSeed |> Tuple.first

        -- We know we'll never request an index out of bounds of the array, but to convince the compiler we fall back
        -- to a default (blue) which will never be used.
        getRandomColour index =
            Array.get index randomColours |> Maybe.withDefault Shape.Blue
    in
    { landed = []
    , dropping = { blocks = tBlocks, colour = getRandomColour 0, gridCoord = ( 25, boardViewConfig.rowCount ) }
    , next =
        [ { blocks = eBlocks, colour = getRandomColour 1, gridCoord = ( 31, boardViewConfig.rowCount ) }
        , { blocks = tBlocks, colour = getRandomColour 2, gridCoord = ( 36, boardViewConfig.rowCount ) }
        , { blocks = rBlocks, colour = getRandomColour 3, gridCoord = ( 42, boardViewConfig.rowCount ) }
        , { blocks = iBlocks, colour = getRandomColour 4, gridCoord = ( 47, boardViewConfig.rowCount ) }
        , { blocks = sBlocks, colour = getRandomColour 5, gridCoord = ( 49, boardViewConfig.rowCount ) }
        ]
    , randomSeed = randomSeed
    }


{-| Called when the animation for dropping letters has progressed a frame. Either drops the current letter down one, adds
a new letter to be dropped, or progresses to the next stage once all letters have landed (i.e. to the `PulsingLetters`
stage).
-}
onLetterDropAnimationFrame : DroppingLettersData -> AnimatedBoard
onLetterDropAnimationFrame ({ landed, dropping, next, randomSeed } as data) =
    let
        ( gridX, gridY ) =
            dropping.gridCoord
    in
    if gridY == 4 then
        -- The currently dropping letter has reached the bottom - start the next letter
        let
            newLandedLetters =
                dropping :: landed
        in
        case next of
            nextLetter :: restLetters ->
                -- We have more letters to drop
                DroppingLetters { data | landed = newLandedLetters, dropping = nextLetter, next = restLetters }

            [] ->
                -- All letters now landed
                PulsingLetters
                    { letters = newLandedLetters
                    , animation =
                        HighlightAnimation.startNewAnimation HighlightAnimation.initialId
                            HighlightAnimation.ShapeLanding
                            1000
                            (lettersToBoardBlocks newLandedLetters)
                    , randomSeed = randomSeed
                    }

    else
        -- The currently dropping letter can drop one more row
        DroppingLetters { data | dropping = { dropping | gridCoord = ( gridX, gridY - 1 ) } }


{-| Called when the animation for pulsing letters has progressed a frame. Delegates the work to the `HighlightAnimation`
module then, based on its result, either continues the current animation or progresses to the next stage (i.e. the
`DroppingRandomShapes` stage).
-}
onPulsingLettersAnimationFrame : AnimatedBoard -> HighlightAnimation.Msg -> PulsingLettersData -> AnimatedBoard
onPulsingLettersAnimationFrame animatedBoard msg data =
    case HighlightAnimation.update msg data.animation of
        HighlightAnimation.IgnoreMsg ->
            animatedBoard

        HighlightAnimation.Continue animation ->
            PulsingLetters { data | animation = animation }

        HighlightAnimation.Complete ->
            initDroppingRandomShapes data.randomSeed data.letters |> DroppingRandomShapes


{-| Initialises the data required when entering the DroppingRandomShapes state. Generates shape buffer used to get
random shapes, and gets an initial shape from it which is then initialised for dropping.
-}
initDroppingRandomShapes : Random.Seed -> List Letter -> DroppingRandomShapesData
initDroppingRandomShapes randomSeed letters =
    let
        ( initialShape, shapeBuffer ) =
            Shape.createShapeBag randomSeed |> Shape.next

        ( startInfo, shapeStartInfoSeed ) =
            Random.step randomShapeStartInfoGenerator randomSeed
    in
    { letters = letters
    , droppingShapes = [ initDroppingShape startInfo initialShape ]
    , shapeBuffer = shapeBuffer
    , randomSeed = shapeStartInfoSeed
    }



-- DROPPING SHAPES


{-| Initialises the supplied `Shape` ready to be a `DroppingShape`, using the supplied `xCoord` and `turns`.
-}
initDroppingShape : { xCoord : Int, turns : Int } -> Shape -> DroppingShape
initDroppingShape { xCoord, turns } shape =
    { shape = rotateXTimes turns shape, gridCoord = ( xCoord, boardViewConfig.rowCount ) }


{-| Handles the case when the dropping shapes should be dropped one row. Moves all shapes down one row, possibly removing
some if they've now dropped off the bottom of the board, and also potentially building a command to generate a new
random shape.
-}
handleShapeDropDelayElapsed : DroppingRandomShapesData -> DroppingRandomShapesData
handleShapeDropDelayElapsed ({ droppingShapes, randomSeed } as data) =
    let
        -- Decides what to do with this dropping shape - either lowers it by one row (returning it in a `Just`) or
        -- returns `Nothing`, indicating this shape should be removed.
        processDroppingShape : DroppingShape -> Maybe DroppingShape
        processDroppingShape droppingShape =
            let
                ( x, y ) =
                    droppingShape.gridCoord
            in
            if y < (-1 * (Shape.data droppingShape.shape).gridSize) then
                -- The shape is now definitely below the grid, so remove it
                Nothing

            else
                Just { shape = droppingShape.shape, gridCoord = ( x, y - 1 ) }

        newData =
            { data | droppingShapes = List.map processDroppingShape droppingShapes |> List.filterMap identity }
    in
    -- Whenever a shape is on row 9 add a new shape at the top
    if droppingShapes |> List.map .gridCoord |> List.map Tuple.second |> List.any ((==) 9) then
        withNewRandomDroppingShape newData

    else
        newData


{-| Gets a new copy of the supplied `DroppingRandomShapesData`, with a new random dropping shape (and an updated shape
buffer and random seed for next time they're used).
-}
withNewRandomDroppingShape : DroppingRandomShapesData -> DroppingRandomShapesData
withNewRandomDroppingShape ({ randomSeed, shapeBuffer, droppingShapes } as data) =
    let
        ( startInfo, nextRandomSeed ) =
            Random.step randomShapeStartInfoGenerator randomSeed

        ( nextShape, nextShapeBuffer ) =
            Shape.next shapeBuffer |> Tuple.mapFirst (initDroppingShape startInfo)
    in
    { data | droppingShapes = nextShape :: droppingShapes, shapeBuffer = nextShapeBuffer, randomSeed = nextRandomSeed }


{-| Generates a random position (along the x-axis) and rotation for a new shape about to be dropped down the screen.
-}
randomShapeStartInfoGenerator : Random.Generator { xCoord : Int, turns : Int }
randomShapeStartInfoGenerator =
    Random.map2 (\xCoord turns -> { xCoord = xCoord, turns = turns })
        (Random.int 20 55)
        (Random.int 0 3)


{-| Rotates the given shape the given number of turns.
-}
rotateXTimes : Int -> Shape -> Shape
rotateXTimes turns shape =
    List.range 1 turns |> List.foldl (\_ shape_ -> Shape.rotate Shape.Clockwise shape_) shape



-- VIEW: COMMON


view : Model -> Element Msg
view (Model { animatedBoard, settings, settingsScreen }) =
    let
        ( letters_, maybeAnimation, droppingShapes_ ) =
            case animatedBoard of
                Initialising ->
                    ( [], Nothing, [] )

                DroppingLetters { landed, dropping } ->
                    ( dropping :: landed, Nothing, [] )

                PulsingLetters { animation } ->
                    ( [], Just animation, [] )

                DroppingRandomShapes { letters, droppingShapes } ->
                    ( letters, Nothing, droppingShapes )

        letterBlocks =
            lettersToBoardBlocks letters_ |> BoardView.withOpacity 1

        droppingShapeBlocks =
            droppingShapes_
                |> List.concatMap droppingShapeToBoardBlocks
                |> BoardView.withOpacity 0.5

        modalAttr =
            case settingsScreen of
                Just settingsScreenModel ->
                    [ SettingsScreen.view settingsScreenModel
                        |> Element.map GotSettingsScreenMsg
                        |> Element.inFront
                    ]

                Nothing ->
                    []
    in
    Element.column
        ([ Element.spacingXY 0 25
         , Element.height Element.fill
         , Element.width Element.fill
         ]
            ++ modalAttr
        )
        [ Element.el [ Element.centerX ] <| BoardView.view boardViewConfig False (droppingShapeBlocks ++ letterBlocks) [] maybeAnimation
        , Element.row [ Element.centerX, Element.spacingXY 20 0 ]
            [ button "Start Game" StartGameRequested
            , button "Settings" ShowSettingsRequested
            ]
        ]


button : String -> msg -> Element msg
button caption msg =
    Element.Input.button
        [ Element.Background.color UIHelpers.mainBackgroundColour
        , Element.Font.color UIHelpers.mainForegroundColour
        , Element.Border.color UIHelpers.mainForegroundColour
        , Element.Border.width 2
        , Element.Border.rounded 20
        , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]
        , Element.focused []
        ]
        { onPress = Just msg
        , label = Element.el [ Element.paddingEach { top = 5, right = 7, bottom = 7, left = 7 } ] (Element.text caption)
        }



-- VIEW: BOARD


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15
    , rowCount = 15
    , colCount = 80
    , borderStyle = BoardView.Fade UIHelpers.mainBackgroundColour
    , showGridLines = True
    }


{-| Converts the list of letters to the list of blocks to use to render them on the board.
-}
lettersToBoardBlocks : List Letter -> List ( Coord, Shape.BlockColour )
lettersToBoardBlocks letters =
    letters
        |> List.concatMap
            (\{ blocks, colour, gridCoord } ->
                blocks
                    |> DroppingShape.calcBoardCoords gridCoord
                    |> BoardView.withColour colour
            )


{-| Converts supplied `DroppingShape` to the list of blocks to use to render it on the board.
-}
droppingShapeToBoardBlocks : DroppingShape -> List ( Coord, Shape.BlockColour )
droppingShapeToBoardBlocks droppingShape =
    let
        { colour } =
            Shape.data droppingShape.shape
    in
    DroppingShape.calcShapeBlocksBoardCoords droppingShape |> BoardView.withColour colour



-- SETTINGS


getSettings : Model -> Settings
getSettings (Model { settings }) =
    settings



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model { animatedBoard, settingsScreen }) =
    Sub.batch [ animationSubscriptions animatedBoard, settingsScreenSubscription settingsScreen ]


settingsScreenSubscription : Maybe SettingsScreen.Model -> Sub Msg
settingsScreenSubscription maybeSettingScreen =
    maybeSettingScreen
        |> Maybe.map SettingsScreen.subscriptions
        |> Maybe.withDefault Sub.none
        |> Sub.map GotSettingsScreenMsg


animationSubscriptions : AnimatedBoard -> Sub Msg
animationSubscriptions animatedBoard =
    case animatedBoard of
        Initialising ->
            Sub.none

        DroppingLetters _ ->
            Time.every 50 <| always LetterDropAnimationFrame

        PulsingLetters { animation } ->
            HighlightAnimation.subscriptions animation |> Sub.map GotHighlightAnimationMsg

        DroppingRandomShapes _ ->
            Time.every (toFloat 250) <| always ShapeDropDelayElapsed



-- LETTERS


tBlocks : List Coord
tBlocks =
    [ ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 4, 6 ), ( 2, 5 ), ( 2, 4 ), ( 2, 3 ), ( 2, 2 ), ( 2, 1 ), ( 2, 0 ) ]


eBlocks : List Coord
eBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 1, 3 ), ( 2, 3 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]


rBlocks : List Coord
rBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 5 ), ( 3, 4 ), ( 2, 3 ), ( 1, 3 ), ( 1, 2 ), ( 2, 1 ), ( 3, 0 ) ]


iBlocks : List Coord
iBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ) ]


sBlocks : List Coord
sBlocks =
    [ ( 0, 1 ), ( 1, 0 ), ( 2, 0 ), ( 3, 1 ), ( 3, 2 ), ( 2, 3 ), ( 1, 3 ), ( 0, 4 ), ( 0, 5 ), ( 1, 6 ), ( 2, 6 ), ( 3, 5 ) ]
