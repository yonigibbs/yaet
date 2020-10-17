module AsciiGrid exposing (blockColourConfig, build)

{-| This module contains functions which allow tests to define some sort of block data (i.e. shapes or boards) in plain
ascii text. For example this would be how to represent the straight-line shape:

    -- Create a multi-line string literal like this:
    """
    ----
    xxxx
    ----
    ----
    """

A lower-case `x` represents a cell which has a block in it it; any other character (hyphens generally used) means the
cell is empty.

-}

import Block
import Dict exposing (Dict)


{-| Parses the passed in textual representation of a grid of blocks, and returns a list of the blocks which are
occupied. How it knows which blocks are occupied is by the character in each location: if this character appears in the
`config` dictionary, it means it's populated, otherwise it's empty (by convention a hyphen is used for empty blocks).
Each item in the returned list is a tuple whose first value is the coordinate of the block, and the second its value.
The value is specified in the `config` dictionary, by matching it to its corresponding character.

Note that the returned list of coordinates is sorted by the coordinate, as that allows tests to more easily compare the
returned list to some expected list.

-}
build : String -> Dict Char a -> List ( Block.Coord, a )
build asciiGrid config =
    let
        lineCoords : Int -> String -> List ( Block.Coord, a )
        lineCoords y lineText =
            String.toList lineText
                |> List.indexedMap
                    (\x char ->
                        case Dict.get char config of
                            Just a ->
                                [ ( ( x, y ), a ) ]

                            Nothing ->
                                []
                    )
                |> List.concat

        trimStartNewline s =
            if String.startsWith "\n" s then
                String.dropLeft 1 s

            else
                s

        trimEndNewline s =
            if String.endsWith "\n" s then
                String.dropRight 1 s

            else
                s
    in
    -- TODO: this doesn't do any validation that all rows are the same length, etc: should it? Would using a parser be better?
    asciiGrid
        |> trimStartNewline
        |> trimEndNewline
        |> String.lines
        |> List.reverse
        |> List.indexedMap lineCoords
        |> List.concat
        |> List.sortBy Tuple.first


{-| The configuration dictionary to supply to `AsciiGrid.Build` which maps each character to its corresponding colour.
-}
blockColourConfig : Dict.Dict Char Block.BlockColour
blockColourConfig =
    Dict.fromList
        [ ( 'b', Block.Blue )
        , ( 'r', Block.Red )
        , ( 'o', Block.Orange )
        , ( 'y', Block.Yellow )
        , ( 'p', Block.Purple )
        , ( 'g', Block.Green )
        ]
