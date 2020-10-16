module Block exposing (Colour(..), Coord)

{-| This module contains functionality related to a single block which either forms part of a shape which is currently
dropping, or part of the board, once it's landed.
-}


{-| The colour of a block which forms part of a shape and eventually part of the board, when it lands.
-}
type Colour
    = Blue
    | Red
    | Orange
    | Yellow
    | Purple


{-| Represents the coordinates of a block within some parent "container": this could be the coordinates of a block on a
board, or a block within a shape. This is simply a tuple of integer values representing the x- and y-coordinates of the
block.

(A tuple is used rather than a record as it is comparable, allowing for easier comparisons etc.)

-}
type alias Coord =
    ( Int, Int )
