module Shape exposing (RotationDirection(..), Shape, ShapeBuilder, ShapeData, builders, data, rotate)

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
and up the left of the grid, and coordinates indices are 0-based.

-}

-- test
-- sadf

import Block exposing (BlockColour)


{-| A shape currently in the process of dropping down the board.
-}
type Shape
    = Shape ShapeData


{-| The data associated with a `Shape`.
-}
type alias ShapeData =
    { gridSize : Int, blocks : List Block.Coord, colour : Block.BlockColour }


type RotationDirection
    = Clockwise
    | Anticlockwise


{-| Gets the data associated with the passed in shape.
-}
data : Shape -> ShapeData
data (Shape shapeData) =
    shapeData


{-| A function which is used to generate a valid shape.
-}
type alias ShapeBuilder =
    BlockColour -> Shape


{-| A list of functions, each of which creates a different shape, of some given colour. This list contains the functions
for generating all possible shapes. As this has to be known to the compiler to be non-empty, a tuple is returned with a
default shape builder, followed by the rest of the shape builders.
-}
builders : ( ShapeBuilder, List ShapeBuilder )
builders =
    let
        convertToBuilder =
            \{ gridSize, blocks } colour -> Shape { gridSize = gridSize, blocks = blocks, colour = colour }
    in
    ( -- Straight line (initially horizontal)
      convertToBuilder { gridSize = 4, blocks = [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 3, 2 ) ] }
    , List.map convertToBuilder
        [ -- L-shape on its back:
          --     x
          -- x x x
          { gridSize = 3, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 2, 2 ) ] }
        , -- Mirror image of the above:
          -- x
          -- x x x
          { gridSize = 3, blocks = [ ( 0, 2 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ] }
        , -- Plus-sign with a bit on the bottom missing:
          --   x
          -- x x x
          { gridSize = 3, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ] }
        , -- Almost a "z-shape":
          -- x x
          --   x x
          { gridSize = 3, blocks = [ ( 0, 2 ), ( 1, 2 ), ( 1, 1 ), ( 2, 1 ) ] }
        , -- Mirror image of the above:
          --   x x
          -- x x
          { gridSize = 3, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ] }
        , -- Square:
          -- x x
          -- x x
          { gridSize = 2, blocks = [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ] }
        ]
    )


{-| Rotates the supplied shape in the given direction.
-}
rotate : RotationDirection -> Shape -> Shape
rotate direction (Shape shapeData) =
    let
        calcCoords : Block.Coord -> Block.Coord
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
