module Coord exposing (Coord)

{-| Module for the `Coord` type (see that type for details).
-}


{-| Represents the coordinates of a block within some parent "container": this could be the coordinates of a block on a
board, or a block within a shape. This is simply a tuple of integer values representing the x- and y-coordinates of the
block, where the x-axis runs along the bottom of the containing grid, and the y-axis runs up the left hand side of it
(i.e. like a standard line chart, rather than like SVG coordinates, where the y-axis runs _down_ the left hand side).

(A tuple is used rather than a record as it is comparable, allowing for easier comparisons etc.)

-}
type alias Coord =
    ( Int, Int )
