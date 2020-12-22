module Shape exposing
    ( Bag
    , BlockColour(..)
    , RotationDirection(..)
    , Shape
    , allColours
    , allShapes
    , clippedBlocks
    , createShapeBag
    , data
    , next
    , rotate
    , withOrgRotation
    )

{-| A shape represents a set of four contiguous blocks currently falling from the top of the board to the bottom. Once
it lands, the blocks that make up that shape become part of the board itself, and are no longer represented by a shape.

A shape is conceptually defined as a grid of some size (whatever size is required to fit the shape in fully, in every
possible rotation), containing the coordinates of 4 contiguous blocks within that grid, placed roughly around the middle
(as close as possible). The shapes can be rotated: this is done by conceptually turning the whole grid on its side
around its centre point.

For example, the straight line shape requires a 4x4 grid, and would initially be defined as follows:

    +---+---+---+---+
    |   |   |   |   |
    +---+---+---+---+
    | X | X | X | X |
    +---+---+---+---+
    |   |   |   |   |
    +---+---+---+---+
    |   |   |   |   |
    +---+---+---+---+

When rotated clockwise, this would become:

    +---+---+---+---+
    |   |   | X |   |
    +---+---+---+---+
    |   |   | X |   |
    +---+---+---+---+
    |   |   | X |   |
    +---+---+---+---+
    |   |   | X |   |
    +---+---+---+---+

In the example of the shape which "a plus sign with a bit missing", it only requires a 3x3 grid:

    +---+---+---+
    |   | X |   |
    +---+---+---+
    | X | X | X |
    +---+---+---+
    |   |   |   |
    +---+---+---+

When this is rotated clockwise it becomes the following:

    +---+---+---+
    |   | X |   |
    +---+---+---+
    |   | X | X |
    +---+---+---+
    |   | X |   |
    +---+---+---+

The data in the shape is simply the coordinates of the 4 cells which are filled. The x- and y-axes run along the bottom
and up the left of the grid, and coordinates indexes are 0-based. The way the shapes are initially generated, is such
that the top of the shape's grid should be at the top of board when that shape is first added to the game.

-}

import Coord exposing (Coord)
import Random
import Random.List


{-| The colour of a block which forms part of a shape and eventually part of the board, when it lands.
-}
type BlockColour
    = Cyan
    | Blue
    | Orange
    | Yellow
    | Green
    | Purple
    | Red


{-| All colours used in the shapes.
-}
allColours : List BlockColour
allColours =
    [ Cyan, Blue, Orange, Yellow, Green, Purple, Red ]


{-| A shape currently in the process of dropping down the board.
-}
type Shape
    = Shape ShapeData


{-| The data associated with a `Shape`.
-}
type alias ShapeData =
    { gridSize : Int, blocks : List Coord, colour : BlockColour, orgBlocks : List Coord }


{-| The direction in which a shape can be rotated.
-}
type RotationDirection
    = Clockwise
    | Anticlockwise


{-| Gets the data associated with the passed in shape.
-}
data : Shape -> { gridSize : Int, blocks : List Coord, colour : BlockColour }
data (Shape { gridSize, blocks, colour }) =
    { gridSize = gridSize, blocks = blocks, colour = colour }


{-| All the shapes used in the game. Returned as a tuple to represent a "non-empty list" rather than just a normal list,
as this is required in a few places.
-}
allShapes : ( Shape, List Shape )
allShapes =
    -- Straight line (initially horizontal)
    ( createShape { gridSize = 4, blocks = [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 3, 2 ) ], colour = Cyan }
    , List.map
        createShape
        [ -- L-shape on its back:
          --     x
          -- x x x
          { gridSize = 3, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 2, 2 ) ], colour = Orange }
        , -- Mirror image of the above:
          -- x
          -- x x x
          { gridSize = 3, blocks = [ ( 0, 2 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ], colour = Blue }
        , -- Plus-sign with a bit on the bottom missing:
          --   x
          -- x x x
          { gridSize = 3, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ], colour = Purple }
        , -- Almost a "z-shape":
          -- x x
          --   x x
          { gridSize = 3, blocks = [ ( 0, 2 ), ( 1, 2 ), ( 1, 1 ), ( 2, 1 ) ], colour = Red }
        , -- Mirror image of the above:
          --   x x
          -- x x
          { gridSize = 3, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ], colour = Green }
        , -- Square:
          -- x x
          -- x x
          { gridSize = 2, blocks = [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ], colour = Yellow }
        ]
    )


createShape : { gridSize : Int, blocks : List Coord, colour : BlockColour } -> Shape
createShape { gridSize, blocks, colour } =
    Shape { gridSize = gridSize, blocks = blocks, colour = colour, orgBlocks = blocks }


{-| Rotates the supplied shape in the given direction.
-}
rotate : RotationDirection -> Shape -> Shape
rotate direction (Shape shapeData) =
    let
        calcCoords : Coord -> Coord
        calcCoords =
            case direction of
                Clockwise ->
                    -- Take this shape:
                    --     +---+---+---+
                    --     |   | X |   |
                    --     +---+---+---+
                    --     | X | X | X |
                    --     +---+---+---+
                    --     |   |   |   |
                    --     +---+---+---+
                    -- When rotated clockwise this becomes:
                    --     +---+---+---+
                    --     |   | X |   |
                    --     +---+---+---+
                    --     |   | X | X |
                    --     +---+---+---+
                    --     |   | X |   |
                    --     +---+---+---+
                    -- The coordinates change as follows:
                    --     (0,1) -> (1,2)
                    --     (1,1) -> (1,1)
                    --     (1,2) -> (2,1)
                    --     (2,1) -> (1,0)
                    -- So as can be seen, the new y-coordinate is the original x-coordinate subtracted from 2,
                    -- and the new x-coordinate is the original y-coordinate.
                    --
                    -- Now take this shape:
                    --     +---+---+---+---+
                    --     |   |   |   |   |
                    --     +---+---+---+---+
                    --     | X | X | X | X |
                    --     +---+---+---+---+
                    --     |   |   |   |   |
                    --     +---+---+---+---+
                    --     |   |   |   |   |
                    --     +---+---+---+---+
                    -- Becomes this when rotated clockwise:
                    --     +---+---+---+---+
                    --     |   |   | X |   |
                    --     +---+---+---+---+
                    --     |   |   | X |   |
                    --     +---+---+---+---+
                    --     |   |   | X |   |
                    --     +---+---+---+---+
                    --     |   |   | X |   |
                    --     +---+---+---+---+
                    -- The coordinates change as follows:
                    --     (0,2) -> (2,3)
                    --     (1,2) -> (2,2)
                    --     (2,2) -> (2,1)
                    --     (3,2) -> (2,0)
                    -- So as can be seen, the new y-coordinate is the original x-coordinate subtracted from 3,
                    -- and the new x-coordinate is the original y-coordinate.
                    --
                    -- So given the grid size, we can rotate the shape clockwise using this formula:
                    \( x, y ) -> ( y, shapeData.gridSize - 1 - x )

                Anticlockwise ->
                    -- The opposite of the above: the new y-coordinate is the original x-coordinate, and he new
                    -- x-coordinate is the original y-coordinate subtracted from (gridSize - 1).
                    \( x, y ) -> ( shapeData.gridSize - 1 - y, x )
    in
    Shape { shapeData | blocks = List.map calcCoords shapeData.blocks }


{-| Gets a "clipped" version of the supplied shape, i.e. the blocks of the shape, with all empty space around them
removed. For example the straight line shape is normally horizontal on the 3rd line of a 4x4 grid: this would return a
4x1 grid with just the line itself. This is used in situations where the shape needs to be rendered without a surrounding
grid, e.g. in the preview of the upcoming shape.
-}
clippedBlocks : Shape -> List Coord
clippedBlocks (Shape { blocks }) =
    let
        calcMin current new =
            if current == -1 then
                new

            else
                min current new

        ( minX, minY ) =
            blocks
                |> List.foldl
                    (\( blockX, blockY ) ( accMinX, accMinY ) -> ( calcMin accMinX blockX, calcMin accMinY blockY ))
                    ( -1, -1 )
    in
    blocks |> List.map (\( x, y ) -> ( x - minX, y - minY ))


{-| Gets a copy of the supplied shape, with the rotation as it was when the shape was originally created (i.e. before it
was rotated any number of times by the user).
-}
withOrgRotation : Shape -> Shape
withOrgRotation (Shape shapeData) =
    Shape { shapeData | blocks = shapeData.orgBlocks }



-- SHAPE BAG


{-| Represents a "bag of shape". This starts off as full of all 7 shapes in the game (in a random order), and can have
one shape at a time removed from it till eventually it's empty, at which point the 7 shapes are used to re-fill it, in a
new random order.
-}
type Bag
    = Bag { seed : Random.Seed, shapes : List Shape }


{-| Creates a newly filly shape bag, using the passed in seed to provide pseudo-randomness.
-}
createShapeBag : Random.Seed -> Bag
createShapeBag seed =
    let
        ( firstShape, restShape ) =
            allShapes
    in
    Bag { seed = seed, shapes = firstShape :: restShape }


{-| Retrieves the next shape from the bag (refilling if it required). Returns that shape, along with a new bag to use in
subsequent calls.
-}
next : Bag -> ( Shape, Bag )
next (Bag { seed, shapes }) =
    let
        ( ( maybeShape, remainingShapes ), newSeed ) =
            Random.step (Random.List.choose shapes) seed
    in
    case maybeShape of
        Just shape ->
            ( shape, Bag { seed = newSeed, shapes = remainingShapes } )

        Nothing ->
            -- Bag is empty - just create a new full bag and use that instead.
            createShapeBag seed |> next
