module Game exposing (Direction(..), Game, InitialisationInfo, blocks, moveShape, new, rotateShape, shapeGenerated, timerDrop, timerDropDelay)

{-| This module controls the game currently being played. It's responsible for moving the dropping shape, adding a new
dropping shape when it's landed, etc. It doesn't actually subscribe to timer/keyboard events etc. - instead, it exposes
functions which the main module calls when those subscriptions fire. Similarly, it doesn't have any of its own messages
(e.g. for generating a random next shape): instead, it receives an initial shape when a game is started, and later
returns a flag to the main module whenever `timerDrop` is called to indicate that a new shape is required. This keeps
this module simpler and more self-contained and, more importantly, easy to unit test, as it doesn't directly rely on any
randomness, so can be controlled in a completely predictable fashion in tests.
-}

import Block exposing (BlockColour)
import Board exposing (Board)
import Shape exposing (Shape)


{-| The currently dropping shape, namely the `Shape` itself, and its coordinates. The latter are the coordinates on the
board of the bottom left corner of the grid which contains the shape (see comments on the `Shape` module itself for more
info).
-}
type alias DroppingShape =
    { shape : Shape, coord : Block.Coord }


{-| A game in the process of being played.
-}
type Game
    = Game Model


{-| Information about the currently playing game.
-}
type alias Model =
    { board : Board
    , droppingShape : DroppingShape
    , nextShape : Shape
    , timerDropDelay : Int
    , shapeBuffer : List Shape
    }


type Direction
    = Down
    | Left
    | Right


{-| The default period to wait before the next timer drop kicks in (i.e. the dropping shape is dropped one more row in
the board.
-}
defaultInitialDropDelay =
    1000


type alias InitialisationInfo =
    { initialShape : Shape, nextShape : Shape, shapeBuffer : List Shape }


{-| Starts a new game with the supplied shape as the initially dropping shape.
-}
new : InitialisationInfo -> Game
new { initialShape, nextShape, shapeBuffer } =
    Game
        { board = Board.emptyBoard
        , droppingShape = initDroppingShape initialShape
        , nextShape = nextShape
        , timerDropDelay = defaultInitialDropDelay
        , shapeBuffer = shapeBuffer
        }


initDroppingShape : Shape -> DroppingShape
initDroppingShape shape =
    { shape = shape, coord = calcInitialBoardCoord shape }


timerDrop : Game -> ( Game, Bool )
timerDrop ((Game ({ droppingShape } as model)) as game) =
    let
        proposedDroppingShape =
            { droppingShape | coord = nextCoord Down droppingShape.coord }
    in
    if isValidPosition model.board proposedDroppingShape then
        -- It's valid for the currently dropping shape to go down by one row, so just do that. No need to ask for a new
        -- shape to add to the buffer just now as we aren't yet using up the currently dropping one.
        ( Game { model | droppingShape = proposedDroppingShape }, False )

    else
        -- It's not valid for the currently dropping shape to go down by one row, so it must have landed.
        handleDroppingShapeLanded game


moveShape : Direction -> Game -> ( Game, Bool )
moveShape direction ((Game ({ droppingShape } as model)) as game) =
    let
        proposedDroppingShape =
            { droppingShape | coord = nextCoord direction droppingShape.coord }
    in
    case ( isValidPosition model.board proposedDroppingShape, direction ) of
        ( True, _ ) ->
            ( Game { model | droppingShape = proposedDroppingShape }, False )

        ( False, Down ) ->
            handleDroppingShapeLanded game

        ( False, _ ) ->
            ( game, False )


{-| Handles the case when the dropping shape has landed: appends its blocks to the board and takes the next item off the
buffer to be the new "next" shape.
-}
handleDroppingShapeLanded : Game -> ( Game, Bool )
handleDroppingShapeLanded ((Game model) as game) =
    -- We can only do this we have a shape in the buffer, as we need to take a shape out the buffer and make it the new
    -- "next" shape.
    case model.shapeBuffer of
        firstInBuffer :: restOfBuffer ->
            -- We have one, so we can use it:
            let
                { colour } =
                    Shape.data model.droppingShape.shape

                -- TODO: don't immediately remove completed rows as currently implemented - add some sort of quick
                -- animation (fading? flashing?) to show the ones being removed.
                ( nextBoard, _ ) =
                    Board.append model.board colour (calcShapeBlocksBoardCoords model.droppingShape)
                        |> Board.removeCompletedRows
            in
            ( Game
                { model
                    | board = nextBoard
                    , droppingShape = initDroppingShape model.nextShape
                    , nextShape = firstInBuffer
                    , shapeBuffer = restOfBuffer
                }
            , True
            )

        _ ->
            -- Nothing in the buffer so we can't accept this
            ( game, False )


rotateShape : Game -> Shape.RotationDirection -> Game
rotateShape ((Game ({ droppingShape } as model)) as game) direction =
    let
        proposedDroppingShape =
            { droppingShape | shape = Shape.rotate direction droppingShape.shape }
    in
    if isValidPosition model.board proposedDroppingShape then
        Game { model | droppingShape = proposedDroppingShape }

    else
        game


nextCoord : Direction -> Block.Coord -> Block.Coord
nextCoord direction ( x, y ) =
    case direction of
        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


{-| Calculates whether the supplied dropping shape is valid to be at its specified coordinates, for the given board.
-}
isValidPosition : Board -> DroppingShape -> Bool
isValidPosition board droppingShape =
    calcShapeBlocksBoardCoords droppingShape |> Board.areCellsAvailable board


{-| Gets all the blocks that are currently in the game, including landed blocks and ones that are part of the dropping
shape. Used for rendering, as the distinction between the dropping blocks and the landed blocks there is irrelevant.

Returns a list of tuples, where the first value in the tuple is the block's coordinates, and the second value is its
colour.

-}
blocks : Game -> List ( Block.Coord, BlockColour )
blocks (Game game) =
    let
        { colour } =
            Shape.data game.droppingShape.shape

        droppingShapeBlocks =
            calcShapeBlocksBoardCoords game.droppingShape |> List.map (\coord -> ( coord, colour ))
    in
    Board.occupiedCells game.board ++ droppingShapeBlocks


{-| Called when a random shape has been generated and is ready to add to the buffer.
-}
shapeGenerated : Game -> Shape -> Game
shapeGenerated (Game model) shape =
    Game { model | shapeBuffer = model.shapeBuffer ++ [ shape ] }


{-| Calculates the coordinates of the blocks of the supplied dropping shape on board. The dropping shape's blocks'
coordinates are relative to the coordinates of the shape itself.
-}
calcShapeBlocksBoardCoords : DroppingShape -> List Block.Coord
calcShapeBlocksBoardCoords droppingShape =
    let
        ( shapeX, shapeY ) =
            droppingShape.coord
    in
    Shape.data droppingShape.shape
        |> .blocks
        |> List.map (\( x, y ) -> ( x + shapeX, y + shapeY ))


{-| Calculates the coordinates on the board that the supplied shape should first be put at.
-}
calcInitialBoardCoord : Shape -> Block.Coord
calcInitialBoardCoord shape =
    let
        shapeGridSize =
            Shape.data shape |> .gridSize

        x =
            ((toFloat Board.xCellCount / 2) - (toFloat shapeGridSize / 2)) |> floor

        y =
            Board.yCellCount - shapeGridSize
    in
    ( x, y )


timerDropDelay : Game -> Int
timerDropDelay (Game model) =
    model.timerDropDelay
