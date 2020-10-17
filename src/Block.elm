module Block exposing (BlockColour(..), Coord, toDarkColour, toLightColour)

{-| This module contains functionality related to a single block which either forms part of a shape which is currently
dropping, or part of the board, once it's landed.
-}

-- TODO: does the name of this module make sense? It's here as the types in it are required by higher-level modules like
-- `Game` and `Board`. Maybe it should just be called `Common` or something like that?

import Color exposing (Color)


{-| The colour of a block which forms part of a shape and eventually part of the board, when it lands.
-}
type BlockColour
    = Blue
    | Red
    | Orange
    | Yellow
    | Purple
    | Green


toDarkColour : BlockColour -> Color
toDarkColour blockColour =
    case blockColour of
        Blue ->
            Color.darkBlue

        Red ->
            Color.darkRed

        Orange ->
            Color.darkOrange

        Yellow ->
            Color.darkYellow

        Purple ->
            Color.darkPurple

        Green ->
            Color.darkGreen


toLightColour : BlockColour -> Color
toLightColour blockColour =
    case blockColour of
        Blue ->
            Color.lightBlue

        Red ->
            Color.lightRed

        Orange ->
            Color.lightOrange

        Yellow ->
            Color.lightYellow

        Purple ->
            Color.lightPurple

        Green ->
            Color.lightGreen


{-| Represents the coordinates of a block within some parent "container": this could be the coordinates of a block on a
board, or a block within a shape. This is simply a tuple of integer values representing the x- and y-coordinates of the
block, where the x-axis runs along the bottom of the containing grid, and the y-axis runs up the left hand side of it
(i.e. like a standard line chart, rather than like SVG coordinates, where the y-axis runs _down_ the left hand side).

(A tuple is used rather than a record as it is comparable, allowing for easier comparisons etc.)

-}
type alias Coord =
    ( Int, Int )
