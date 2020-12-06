module WelcomeScreen exposing (Model, Msg, init, subscriptions, update, view)

{-| This module contains all functionality related to the welcome screen. Manages the animations shown here.
-}

import BlockColour exposing (BlockColour)
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
import RandomShapeGenerator
import Shape exposing (Shape)
import Time
import UIHelpers



-- MODEL


{-| Represents a letter in the word "TETRIS", as a shape on a board, much like a normal Tetris shape.
-}
type alias Letter =
    { blocks : List Coord, colour : BlockColour, gridCoord : Coord }


{-| The model of this module, exposed as an opaque type. Defines the three stages of the animation shows on the Welcome
screen:

  - `DroppingLetters`: The letters of the word "Tetris" are dropping onto the board, one by one.
  - `PulsingLetters`: The letters of the word "Tetris" are being "pulsed" (faded out then back in).
  - `DroppingRandomShapes`: Random shapes are dropping from the top of the board down till they disappear, behind the
    letters of Tetris.

-}
type Model
    = DroppingLetters { dropped : List Letter, dropping : Letter, next : List Letter }
    | PulsingLetters { letters : List Letter, animation : HighlightAnimation.Model }
    | DroppingRandomShapes { letters : List Letter, droppingShapes : List DroppingShape }


init : Model
init =
    DroppingLetters
        { dropped = []
        , dropping = { blocks = tBlocks, colour = BlockColour.Blue, gridCoord = ( 25, boardViewConfig.rowCount ) }
        , next =
            [ { blocks = eBlocks, colour = BlockColour.Red, gridCoord = ( 31, boardViewConfig.rowCount ) }
            , { blocks = tBlocks, colour = BlockColour.Orange, gridCoord = ( 36, boardViewConfig.rowCount ) }
            , { blocks = rBlocks, colour = BlockColour.Yellow, gridCoord = ( 42, boardViewConfig.rowCount ) }
            , { blocks = iBlocks, colour = BlockColour.Purple, gridCoord = ( 47, boardViewConfig.rowCount ) }
            , { blocks = sBlocks, colour = BlockColour.Green, gridCoord = ( 49, boardViewConfig.rowCount ) }
            ]
        }



-- UPDATE


type Msg
    = LetterDropAnimationFrame -- A letter should be dropped another row (or a new letter added)
    | GotHighlightAnimationMsg HighlightAnimation.Msg -- A pulsing animation frame has occurred
    | RandomShapeGenerated { shape : Shape, xCoord : Int } -- Random shape generated and should be added
    | ShapeDropDelayElapsed -- The delay between each time the dropping shapes are lowered a row has elapsed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LetterDropAnimationFrame, DroppingLetters data ) ->
            ( onLetterDropAnimationFrame data, Cmd.none )

        ( LetterDropAnimationFrame, _ ) ->
            ( model, Cmd.none )

        ( GotHighlightAnimationMsg highlightAnimationMsg, PulsingLetters data ) ->
            onPulsingLettersAnimationFrame model highlightAnimationMsg data

        ( GotHighlightAnimationMsg _, _ ) ->
            ( model, Cmd.none )

        ( RandomShapeGenerated { shape, xCoord }, DroppingRandomShapes data ) ->
            -- Add the new dropping shape into the list.
            ( DroppingRandomShapes
                { data
                    | droppingShapes = { shape = shape, gridCoord = ( xCoord, boardViewConfig.rowCount ) } :: data.droppingShapes
                }
            , Cmd.none
            )

        ( RandomShapeGenerated _, _ ) ->
            ( model, Cmd.none )

        ( ShapeDropDelayElapsed, DroppingRandomShapes data ) ->
            handleShapeDropDelayElapsed data

        ( ShapeDropDelayElapsed, _ ) ->
            ( model, Cmd.none )


{-| Called when the animation for dropping letters has progressed a frame. Either drops the current letter down one, adds
a new letter to be dropped, or progresses to the next stage once all letters have dropped (i.e. to the `PulsingLetters`
stage).
-}
onLetterDropAnimationFrame : { dropped : List Letter, dropping : Letter, next : List Letter } -> Model
onLetterDropAnimationFrame { dropped, dropping, next } =
    let
        ( gridX, gridY ) =
            dropping.gridCoord
    in
    if gridY == 4 then
        -- The currently dropping letter has reached the bottom - start the next letter
        case next of
            nextLetter :: restLetters ->
                -- We have more letters to drop
                DroppingLetters { dropped = dropping :: dropped, dropping = nextLetter, next = restLetters }

            [] ->
                -- All letters now dropped
                let
                    letters =
                        dropping :: dropped
                in
                PulsingLetters
                    { letters = letters
                    , animation =
                        HighlightAnimation.startNewAnimation HighlightAnimation.initialId
                            HighlightAnimation.ShapeLanding
                            1000
                            (lettersToBoardBlocks letters)
                    }

    else
        -- The currently dropping letter can drop one more row
        DroppingLetters { dropped = dropped, dropping = { dropping | gridCoord = ( gridX, gridY - 1 ) }, next = next }


{-| Called when the animation for pulsing letters has progressed a frame. Delegates the work to the `HighlightAnimation`
module then, based on its result, either continues the current animation or progresses to the next stage (i.e. the
`DroppingRandomShapes` stage).
-}
onPulsingLettersAnimationFrame : Model -> HighlightAnimation.Msg -> { letters : List Letter, animation : HighlightAnimation.Model } -> ( Model, Cmd Msg )
onPulsingLettersAnimationFrame model msg pulsingLettersData =
    case HighlightAnimation.update msg pulsingLettersData.animation of
        HighlightAnimation.IgnoreMsg ->
            ( model, Cmd.none )

        HighlightAnimation.Continue nextAnimationModel ->
            ( PulsingLetters { pulsingLettersData | animation = nextAnimationModel }, Cmd.none )

        HighlightAnimation.Complete ->
            ( DroppingRandomShapes { letters = pulsingLettersData.letters, droppingShapes = [] }, generateRandomShape )


{-| Handles the case when the dropping shapes should be dropped one row. Moves all shapes down one row, possibly removing
some if they've now dropped off the bottom of the board, and also potentially building a command to generate a new
random shape.
-}
handleShapeDropDelayElapsed : { letters : List Letter, droppingShapes : List DroppingShape } -> ( Model, Cmd Msg )
handleShapeDropDelayElapsed ({ droppingShapes } as data) =
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

        nextDroppingShapes =
            List.map processDroppingShape droppingShapes |> List.filterMap identity

        -- Whenever a shape is on row 9 add a new shape at the top
        needNewShape =
            droppingShapes |> List.any (\{ gridCoord } -> Tuple.second gridCoord == 9)

        cmd =
            if needNewShape then
                generateRandomShape

            else
                Cmd.none
    in
    ( DroppingRandomShapes { data | droppingShapes = nextDroppingShapes }, cmd )


{-| Generates a random dropping shape, i.e. one of the shapes in the game, rotated a random number of times (between 0 and
3), with a random x-coordinate, ready to start dropping down the board.
-}
generateRandomShape : Cmd Msg
generateRandomShape =
    Random.map3 (\shape xCoord turns -> { shape = rotateXTimes turns shape, xCoord = xCoord })
        RandomShapeGenerator.generator
        (Random.int 20 60)
        (Random.int 0 3)
        |> Random.generate RandomShapeGenerated


{-| Rotates the given shape the given number of turns.
-}
rotateXTimes : Int -> Shape -> Shape
rotateXTimes turns shape =
    List.range 1 turns |> List.foldl (\_ shape_ -> Shape.rotate Shape.Clockwise shape_) shape



-- VIEW


view : Model -> msg -> Element msg
view model startGameMsg =
    let
        ( letters_, maybeAnimation, droppingShapes_ ) =
            case model of
                DroppingLetters { dropped, dropping } ->
                    ( dropping :: dropped, Nothing, [] )

                PulsingLetters { animation } ->
                    ( [], Just animation, [] )

                DroppingRandomShapes { letters, droppingShapes } ->
                    ( letters, Nothing, droppingShapes )

        letterBlocks =
            lettersToBoardBlocks letters_ |> BoardView.withOpacity 1

        droppingShapeBlocks =
            droppingShapes_
                |> List.concatMap droppingShapeToBoardBlocks
                |> BoardView.withOpacity 0.25
    in
    Element.column [ Element.spacingXY 0 25 ]
        [ BoardView.view boardViewConfig (droppingShapeBlocks ++ letterBlocks) maybeAnimation
        , Element.row [ Element.centerX ] [ button "Start Game" startGameMsg ]
        ]


button : String -> msg -> Element msg
button caption msg =
    Element.Input.button
        [ Element.Background.color UIHelpers.mainBackgroundColour
        , Element.Font.color UIHelpers.buttonBorderColor
        , Element.Border.color UIHelpers.buttonBorderColor
        , Element.Border.width 2
        , Element.Border.rounded 20
        , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]
        ]
        { onPress = Just msg
        , label = Element.el [ Element.paddingEach { top = 5, right = 7, bottom = 7, left = 7 } ] (Element.text caption)
        }


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15, rowCount = 15, colCount = 80, borderStyle = BoardView.Fade UIHelpers.mainBackgroundColour }


{-| Converts the list of letters to the list of blocks to use to render them on the board.
-}
lettersToBoardBlocks : List Letter -> List ( Coord, BlockColour )
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
droppingShapeToBoardBlocks : DroppingShape -> List ( Coord, BlockColour )
droppingShapeToBoardBlocks droppingShape =
    let
        { colour } =
            Shape.data droppingShape.shape
    in
    DroppingShape.calcShapeBlocksBoardCoords droppingShape |> BoardView.withColour colour



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        DroppingLetters _ ->
            Time.every 50 <| always LetterDropAnimationFrame

        PulsingLetters { animation } ->
            HighlightAnimation.subscriptions animation |> Sub.map GotHighlightAnimationMsg

        DroppingRandomShapes _ ->
            Time.every (toFloat 250) <| always ShapeDropDelayElapsed



-- LETTERS


tBlocks : List Coord
tBlocks =
    [ ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 4, 6 ), ( 2, 6 ), ( 2, 5 ), ( 2, 4 ), ( 2, 3 ), ( 2, 2 ), ( 2, 1 ), ( 2, 0 ) ]


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
