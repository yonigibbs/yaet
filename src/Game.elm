module Game exposing
    ( Game
    , InitialisationInfo
    , MoveDirection(..)
    , blocks
    , moveShape
    , new
    , rotateShape
    , shapeGenerated
    , timerDrop
    , timerDropDelay
    )

{-| This module controls the game currently being played. It's responsible for moving the dropping shape, adding a new
dropping shape when it's landed, etc. It doesn't actually subscribe to timer/keyboard events etc. - instead, it exposes
functions which the main module calls when those subscriptions fire (e.g. `moveShape`).

Similarly, it doesn't have any of its own messages (e.g. for generating a random next shape): instead, it receives the
shapes required to initialise the model, and a buffer of shapes to use next (e.g. whenever a shape lands and a new
dropping shape is required). Whenever this happens, it takes the next item from the buffer. Then, to refill the buffer,
it responds to the calling module (`Main`) with a flag telling it to generate a new random shape, then add it to the
buffer by calling `shapeGenerated`. The main module, therefore, is responsible for making sure there are always enough
shapes in the buffer so that the game will never get into a situation where it needs a shape but the buffer doesn't
contain one. (Realistically as long as there is even one that should be fine, as the time it takes to generate the next
random shape should be negligible. But for safety it might be better to have a few more.)

The above design decisions help keep this module simpler and more self-contained and, more importantly, easy to unit
test, as it doesn't directly rely on any randomness, so can be controlled in a completely predictable fashion in tests.

-}

import Block
import Board exposing (Board)
import Shape exposing (Shape)



-- MODEL TYPES


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
    { board : Board -- The current board (only landed blocks, not the currently dropping shape).
    , droppingShape : DroppingShape -- The currently dropping shape.
    , nextShape : Shape -- The next shape to use as the dropping shape once the current one lands.
    , timerDropDelay : Int -- How long, in ms, before the currently dropping shape should be automatically dropped down a row.
    , shapeBuffer : List Shape -- Buffer of available random shapes. See comments on this module for details.
    }


{-| The direction in which a shape can be moved by the user (or automatically, in the case of `Down`).
-}
type MoveDirection
    = Down
    | Left
    | Right



-- INITIALISATION


{-| The information required to create a new game. See comments on this module for information about what the shape
buffer is.
-}
type alias InitialisationInfo =
    { initialShape : Shape, nextShape : Shape, shapeBuffer : List Shape }


{-| The default period to wait before the next timer drop kicks in (i.e. the dropping shape is dropped one more row in
the board.
-}
defaultInitialDropDelay =
    1000


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


{-| Creates a `DroppingShape` out of the supplied `Shape` by calculating its initial coordinates on the board.
-}
initDroppingShape : Shape -> DroppingShape
initDroppingShape shape =
    let
        shapeGridSize =
            Shape.data shape |> .gridSize

        x =
            ((toFloat Board.xCellCount / 2) - (toFloat shapeGridSize / 2)) |> floor

        y =
            Board.yCellCount - shapeGridSize
    in
    { shape = shape, coord = ( x, y ) }



-- GAME PLAY/MOVEMENT


{-| Called when the delay between automatic drops of the currently dropping shape has elapsed (i.e. initially every
second or so). Drops the current shape one row if possible, otherwise treats it as now having landed, and uses the next
shape as the new dropping shape.

Returns a tuple where the first value is the updated game, and the second value is a boolean indicating whether the
caller (`Main`) should now generate a new random shape. If that's true, it should do so, probably asynchronously, and
call `shapeGenerated` when it's available.

-}
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


{-| Moves the currently dropping shape in the supplied direction, if possible. Returns the same tuple as `timerDrop`
(see it for more details).
-}
moveShape : MoveDirection -> Game -> ( Game, Bool )
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


{-| Rotates the currently dropping shape in the supplied direction, if possible. Returns the updated game.
-}
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


{-| Handles the case when the dropping shape has landed: appends its blocks to the board and takes the next item off the
buffer to be the new "next" shape. Returns the same tuple as `timerDrop` (see it for more details).
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
                -- animation (fading? flashing?) to show the ones being removed. (Currently we ignore the second value
                -- in the tuple returned below, but we'll need it when adding this animation.)
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


{-| Calculates the next coordinate of the supplied coordinate after it is moved one cell in the supplied direction.
-}
nextCoord : MoveDirection -> Block.Coord -> Block.Coord
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


{-| Called when a random shape has been generated and is ready to add to the buffer.
-}
shapeGenerated : Game -> Shape -> Game
shapeGenerated (Game model) shape =
    Game { model | shapeBuffer = model.shapeBuffer ++ [ shape ] }



-- QUERYING INFORMATION ABOUT THE GAME


{-| Gets all the blocks that are currently in the game, including landed blocks and ones that are part of the dropping
shape. Used for rendering, as the distinction between the dropping blocks and the landed blocks there is irrelevant.

Returns a list of tuples, where the first value in the tuple is the block's coordinates, and the second value is its
colour.

-}
blocks : Game -> List ( Block.Coord, Block.Colour )
blocks (Game game) =
    let
        { colour } =
            Shape.data game.droppingShape.shape

        droppingShapeBlocks =
            calcShapeBlocksBoardCoords game.droppingShape |> List.map (\coord -> ( coord, colour ))
    in
    Board.occupiedCells game.board ++ droppingShapeBlocks


{-| Gets the timer drop delay for the supplied game, i.e. how long, in milliseconds, before the currently dropping shape
should be automatically dropped down a row.
-}
timerDropDelay : Game -> Int
timerDropDelay (Game model) =
    model.timerDropDelay
