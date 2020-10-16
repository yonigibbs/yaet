module Shape exposing (RotationDirection(..), Shape, ShapeData, builders, data, rotate)

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
    |   |   |   |   |
    +---+---+---+---+
    | X | X | X | X |
    +---+---+---+---+
    |   |   |   |   |
    +---+---+---+---+

When rotated clockwise, this would become:

    +---+---+---+---+
    |   | X |   |   |
    +---+---+---+---+
    |   | X |   |   |
    +---+---+---+---+
    |   | X |   |   |
    +---+---+---+---+
    |   | X |   |   |
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

import Block


type Shape
    = Shape ShapeData


type alias ShapeData =
    { gridSize : GridSize, blocks : List Block.Coord, colour : Block.Colour }


type GridSize
    = Three
    | Four


type RotationDirection
    = Clockwise
    | Anticlockwise


data : Shape -> ShapeData
data (Shape shapeData) =
    shapeData


{-| A list of functions, each of which creates a different shape, of some given colour. This list contains the functions
for generating all possible shapes.
-}
builders : List (Block.Colour -> Shape)
builders =
    [ -- Straight line (initially horizontal)
      { gridSize = Four, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ] }
    , -- L-shape on its back:
      --     x
      -- x x x
      { gridSize = Three, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 2, 2 ) ] }
    , -- Mirror image of the above:
      -- x
      -- x x x
      { gridSize = Three, blocks = [ ( 0, 2 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ] }
    , -- Plus-sign with a bit on the bottom missing:
      --   x
      -- x x x
      { gridSize = Three, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ] }
    , -- Almost a "z-shape":
      -- x x
      --   x x
      { gridSize = Three, blocks = [ ( 0, 2 ), ( 1, 2 ), ( 1, 1 ), ( 2, 1 ) ] }
    , -- Mirror image of the above:
      --   x x
      -- x x
      { gridSize = Three, blocks = [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ] }
    ]
        |> List.map (\{ gridSize, blocks } colour -> Shape { gridSize = gridSize, blocks = blocks, colour = colour })


rotate : RotationDirection -> Shape -> Shape
rotate direction (Shape shapeData) =
    let
        calcCoords : Block.Coord -> Block.Coord
        calcCoords =
            case ( direction, shapeData.gridSize ) of
                ( Clockwise, Three ) ->
                    -- The centre-point is at coordinate (1,1), which has an actual cell in it. For example, this:
                    --     +---+---+---+
                    --     |   | X |   |
                    --     +---+---+---+
                    --     | X | X | X |
                    --     +---+---+---+
                    --     |   |   |   |
                    --     +---+---+---+
                    -- Becomes this:
                    --     +---+---+---+
                    --     |   | X |   |
                    --     +---+---+---+
                    --     |   | X | X |
                    --     +---+---+---+
                    --     |   | X |   |
                    --     +---+---+---+
                    -- Turning clockwise, the coordinates change as follows:
                    --     (0,1) -> (1,2)
                    --     (1,1) -> (1,1)
                    --     (1,2) -> (2,1)
                    --     (2,1) -> (1,0)
                    -- So as can be seen, the new y-coordinate is the original x-coordinate subtracted from 2,
                    -- and the new x-coordinate is the original y-coordinate
                    \( x, y ) -> ( y, 2 - x )

                ( Anticlockwise, Three ) ->
                    -- The opposite of the above: the new y-coordinate is the original x-coordinate, and he new
                    -- x-coordinate is the original y-coordinate subtracted from 2.
                    \( x, y ) -> ( 2 - y, x )

                _ ->
                    Debug.todo "implement"
    in
    Shape { shapeData | blocks = List.map calcCoords shapeData.blocks }
