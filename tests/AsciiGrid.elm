module AsciiGrid exposing (build)

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


{-| Parses the passed in textual representation of a grid of blocks, and returns the coordinates of those blocks which
are populated. Note that the returned list of coordinates is sorted, as that allows tests to more easily compare the
returned list to some expected list.
-}
build : String -> List Block.Coord
build string =
    let
        lineCoords : Int -> String -> List Block.Coord
        lineCoords lineIndex lineText =
            String.indexes "x" lineText
                |> List.map (\xIndex -> ( xIndex, lineIndex ))

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
    string
        |> trimStartNewline
        |> trimEndNewline
        |> String.lines
        |> List.reverse
        |> List.indexedMap lineCoords
        |> List.concat
        |> List.sort
