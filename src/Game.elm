module Game exposing (Game, blocks, new, nextShapeGenerated)

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


{-| The state of the dropping shape. When `Initialising`, we are waiting for a random shape to be generated; when
`Dropping` the shape is currently dropping.
-}
type DroppingShape
    = Initialising
    | Dropping DroppingShapeInfo


{-| Data about the currently dropping shape, namely the `Shape` itself, and its coordinates. The latter are the
coordinates on the board of the bottom left corner of the grid which contains the shape (see comments on the `Shape`
module itself for more info).
-}
type alias DroppingShapeInfo =
    { shape : Shape, coord : Block.Coord }


{-| A game in the process of being played.
-}
type Game
    = Game GameInfo


{-| Information about the currently playing game.
-}
type alias GameInfo =
    { board : Board, droppingShape : DroppingShape, nextTimerDropDelay : Int }


{-| The default period to wait before the next timer drop kicks in (i.e. the dropping shape is dropped one more row in
the board.
-}
defaultInitialDropDelay =
    1000


{-| Starts a new game with the supplied shape as the initially dropping shape.
-}
new : Shape -> Game
new shape =
    Game
        { board = Board.emptyBoard
        , droppingShape = Dropping { shape = shape, coord = calcInitialBoardCoord shape }
        , nextTimerDropDelay = defaultInitialDropDelay
        }


{-| Gets all the blocks that are currently in the game, including landed blocks and ones that are part of the dropping
shape. Used for rendering, as the distinction between the dropping blocks and the landed blocks there is irrelevant.

Returns a list of tuples, where the first value in the tuple is the block's coordinates, and the second value is its
colour.

-}
blocks : Game -> List ( Block.Coord, BlockColour )
blocks (Game game) =
    let
        droppingShapeBlocks =
            case game.droppingShape of
                Dropping droppingShapeInfo ->
                    let
                        { colour } =
                            Shape.data droppingShapeInfo.shape
                    in
                    calcShapeBlocksBoardCoords droppingShapeInfo |> List.map (\coord -> ( coord, colour ))

                Initialising ->
                    []
    in
    Board.occupiedCells game.board ++ droppingShapeBlocks


{-| Called when the next random shape has been generated and is ready to add to the game.
-}
nextShapeGenerated : Game -> Shape -> Game
nextShapeGenerated game shape =
    Debug.todo "Implement nextShapeGenerated"


{-| Calculates the coordinates of the blocks of the supplied dropping shape on board. The dropping shape's blocks'
coordinates are relative to the coordinates of the shape itself.
-}
calcShapeBlocksBoardCoords : DroppingShapeInfo -> List Block.Coord
calcShapeBlocksBoardCoords droppingShapeInfo =
    let
        ( shapeX, shapeY ) =
            droppingShapeInfo.coord
    in
    Shape.data droppingShapeInfo.shape
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
            Board.yCellCount - shapeGridSize + 1
    in
    ( x, y )
