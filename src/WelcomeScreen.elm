module WelcomeScreen exposing (Model, Msg, init, subscriptions, update, view)

import BlockColour exposing (BlockColour)
import BoardView
import Coord exposing (Coord)
import Element exposing (Element)
import HighlightAnimation
import Random
import RandomShapeGenerator
import Shape exposing (Shape)
import Time
import UIHelpers



-- MODEL


type alias Letter =
    { blocks : List Coord, colour : BlockColour, gridCoord : Coord }



-- TODO: same as DroppingShape alias in Game. Put somewhere common and reuse definition here?


type alias DroppingShape =
    { shape : Shape, gridCoord : Coord }


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
    = ProgressLetterDropAnimationRequested
    | GotHighlightAnimationMsg HighlightAnimation.Msg
    | RandomShapeGenerated { shape : Shape, xCoord : Int }
    | ShapeDropDelayElapsed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ProgressLetterDropAnimationRequested, DroppingLetters data ) ->
            ( progressLetterDropAnimation data, Cmd.none )

        ( ProgressLetterDropAnimationRequested, _ ) ->
            ( model, Cmd.none )

        ( GotHighlightAnimationMsg highlightAnimationMsg, PulsingLetters data ) ->
            progressPulsingLettersAnimation model highlightAnimationMsg data

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


progressLetterDropAnimation : { dropped : List Letter, dropping : Letter, next : List Letter } -> Model
progressLetterDropAnimation { dropped, dropping, next } =
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


progressPulsingLettersAnimation : Model -> HighlightAnimation.Msg -> { letters : List Letter, animation : HighlightAnimation.Model } -> ( Model, Cmd Msg )
progressPulsingLettersAnimation model msg pulsingLettersData =
    case HighlightAnimation.update msg pulsingLettersData.animation of
        HighlightAnimation.IgnoreMsg ->
            ( model, Cmd.none )

        HighlightAnimation.Continue nextAnimationModel ->
            ( PulsingLetters { pulsingLettersData | animation = nextAnimationModel }, Cmd.none )

        HighlightAnimation.Complete ->
            ( DroppingRandomShapes { letters = pulsingLettersData.letters, droppingShapes = [] }, generateRandomShape )


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


generateRandomShape : Cmd Msg
generateRandomShape =
    Random.map3 (\shape xCoord turns -> { shape = rotateXTimes turns shape, xCoord = xCoord })
        RandomShapeGenerator.generator
        (Random.int 20 60)
        (Random.int 0 3)
        |> Random.generate RandomShapeGenerated


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
        , Element.row [ Element.centerX ] [ UIHelpers.button "Start Game" startGameMsg ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15, rowCount = 15, colCount = 80, borderStyle = BoardView.Fade UIHelpers.mainBackgroundColour }


lettersToBoardBlocks : List Letter -> List ( Coord, BlockColour )
lettersToBoardBlocks =
    List.map letterToBoardBlocks >> List.concat


letterToBoardBlocks : Letter -> List ( Coord, BlockColour )
letterToBoardBlocks { blocks, colour, gridCoord } =
    let
        ( gridX, gridY ) =
            gridCoord
    in
    -- TODO: this is v similar to Game.calcShapeBlocksBoardCoords - poss create shared module for stuff related to drawing blocks on the board?
    -- There's more in this module that's similar, e.g. dropping a shape one row, etc. Could also go into a shared module.
    blocks
        |> List.map (\( x, y ) -> ( x + gridX, y + gridY ))
        |> List.map (\coord -> ( coord, colour ))


droppingShapeToBoardBlocks : DroppingShape -> List ( Coord, BlockColour )
droppingShapeToBoardBlocks { shape, gridCoord } =
    -- TODO: is this just a duplicate of Game.calcShapeBlocksBoardCoords? Put somewhere common.
    let
        ( shapeX, shapeY ) =
            gridCoord

        { blocks, colour } =
            Shape.data shape
    in
    blocks
        |> List.map (\( x, y ) -> ( ( x + shapeX, y + shapeY ), colour ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        DroppingLetters _ ->
            Time.every 50 <| always ProgressLetterDropAnimationRequested

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
