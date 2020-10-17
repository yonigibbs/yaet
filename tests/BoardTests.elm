module BoardTests exposing (suite)

{-| Tests for the Board module. Board is an opaque type so we can't construct it ourselves here, pre-populated in some
way. Instead, we start off with an empty board, then build it up by appending cells to it.
-}

import AsciiGrid
import Block exposing (BlockColour)
import Board exposing (Board)
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Board"
        [ describe "occupiedCells" <|
            [ occupiedCellsTest "Empty board" "" []
            , occupiedCellsTest "Populated board"
                """
y----pp-yg-
b---g-ro-b-
"""
                [ -- First row
                  ( ( 0, 0 ), Block.Blue )
                , ( ( 4, 0 ), Block.Green )
                , ( ( 6, 0 ), Block.Red )
                , ( ( 7, 0 ), Block.Orange )
                , ( ( 9, 0 ), Block.Blue )

                -- Second row
                , ( ( 0, 1 ), Block.Yellow )
                , ( ( 5, 1 ), Block.Purple )
                , ( ( 6, 1 ), Block.Purple )
                , ( ( 8, 1 ), Block.Yellow )
                , ( ( 9, 1 ), Block.Green )
                ]
            ]
        , describe "append" <|
            [ appendTest "Append straight line at bottom left of empty board" "" "yyyy" "yyyy"
            , appendTest "Append Z on bottom row of board with cells" """
r----oo-yg-
r---b-pp-y-
""" """
---rr------
--rr-------
""" """
r--rroo-yg-
r-rrb-pp-y-
"""
            ]
        , describe "areCellsAvailable"
            [ areCellsAvailableTest "Empty board" "" "xxxx" True
            , areCellsAvailableTest "Straight line into available space on bottom row" """
b----bb-yy-
b----bbb-y-
""" "-rrrr-----" True
            , areCellsAvailableTest "Straight line into unavailable space on bottom row" """
b----bb-yy-
b-r--bbb-y-
""" "-yyyy-----" False
            , areCellsAvailableTest "Straight line into available space on third row" """
y----y--bb-
r----oo-pp-
b-oorry-pp-
r-oy-bbb-p-
""" """
-rrrr-----
----------
----------
""" True
            , areCellsAvailableTest "Straight line into unavailable space on third row" """
y----y--bb-
r--r-oo-pp-
b-oorry-pp-
r-oy-bbb-p-
""" """
-rrrr-----
----------
----------
""" False
            ]
        ]


occupiedCellsTest : String -> String -> List ( Block.Coord, BlockColour ) -> Test
occupiedCellsTest testDescr asciiBoard expectedOccupiedCells =
    test testDescr <|
        \_ ->
            buildBoard asciiBoard
                |> Board.occupiedCells
                |> List.sortBy Tuple.first
                |> Expect.equal (expectedOccupiedCells |> List.sortBy Tuple.first)


areCellsAvailableTest : String -> String -> String -> Bool -> Test
areCellsAvailableTest testDescr asciiBoard asciiShape expectedAvailable =
    test testDescr <|
        \_ ->
            let
                shapeCoords =
                    AsciiGrid.build asciiShape AsciiGrid.blockColourConfig |> List.map Tuple.first
            in
            buildBoard asciiBoard
                |> (\board -> Board.areCellsAvailable board shapeCoords)
                |> Expect.equal expectedAvailable


appendTest : String -> String -> String -> String -> Test
appendTest testDescr orgBoard newBlocks expectedBoard =
    test testDescr <|
        \_ ->
            AsciiGrid.build newBlocks AsciiGrid.blockColourConfig
                |> List.foldl (\( coord, colour ) board -> Board.append board colour [ coord ]) (buildBoard orgBoard)
                |> Board.occupiedCells
                |> List.sortBy Tuple.first
                |> Expect.equal (buildBoard expectedBoard |> Board.occupiedCells |> List.sortBy Tuple.first)


{-| Builds a board from the supplied ASCII grid. The supplied grid doesn't need to be the full 10x20: just whatever
portion contains occupied cells.
-}
buildBoard : String -> Board
buildBoard asciiBoard =
    AsciiGrid.build asciiBoard AsciiGrid.blockColourConfig
        |> List.foldl (\( coord, colour ) board -> Board.append board colour [ coord ]) Board.emptyBoard
