module BlockColour exposing (BlockColour(..), toDarkColour, toLightColour)

{-| This module contains the `BlockColour` type and related functionalit
-}

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


{-| Converts the `BlockColour` value to a `Color` value that can be used when rendering the game. Gets the light
version of that colour, as blocks are rendered using a combination of the dark and light shades of the block's colour.
-}
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


{-| Converts the `BlockColour` value to a `Color` value that can be used when rendering the game. Gets the dark
version of that colour, as blocks are rendered using a combination of the dark and light shades of the block's colour.
-}
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
