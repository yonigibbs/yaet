module BoardTests exposing (suite)

{-| Tests for the Board module. Board is an opaque type so we can't construct it ourselves here, pre-populated in some
way. Instead, we start off with an empty board, then build it up by appending cells to it.
-}

import AsciiGrid
import BlockColour exposing (BlockColour)
import Coord exposing (Coord)
import Expect
import GameBoard exposing (GameBoard)
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
                  ( ( 0, 0 ), BlockColour.Blue )
                , ( ( 4, 0 ), BlockColour.Green )
                , ( ( 6, 0 ), BlockColour.Red )
                , ( ( 7, 0 ), BlockColour.Orange )
                , ( ( 9, 0 ), BlockColour.Blue )

                -- Second row
                , ( ( 0, 1 ), BlockColour.Yellow )
                , ( ( 5, 1 ), BlockColour.Purple )
                , ( ( 6, 1 ), BlockColour.Purple )
                , ( ( 8, 1 ), BlockColour.Yellow )
                , ( ( 9, 1 ), BlockColour.Green )
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


occupiedCellsTest : String -> String -> List ( Coord, BlockColour ) -> Test
occupiedCellsTest testDescr asciiBoard expectedOccupiedCells =
    test testDescr <|
        \_ ->
            buildBoard asciiBoard
                |> GameBoard.occupiedCells
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
                |> (\board -> GameBoard.areCellsAvailable board shapeCoords)
                |> Expect.equal expectedAvailable


appendTest : String -> String -> String -> String -> Test
appendTest testDescr orgBoard newBlocks expectedBoard =
    test testDescr <|
        \_ ->
            AsciiGrid.build newBlocks AsciiGrid.blockColourConfig
                |> List.foldl (\( coord, colour ) board -> GameBoard.append board colour [ coord ]) (buildBoard orgBoard)
                |> GameBoard.occupiedCells
                |> List.sortBy Tuple.first
                |> Expect.equal (buildBoard expectedBoard |> GameBoard.occupiedCells |> List.sortBy Tuple.first)


{-| Builds a board from the supplied ASCII grid. The supplied grid doesn't need to be the full 10x20: just whatever
portion contains occupied cells.
-}
buildBoard : String -> GameBoard
buildBoard asciiBoard =
    AsciiGrid.build asciiBoard AsciiGrid.blockColourConfig
        |> List.foldl (\( coord, colour ) board -> GameBoard.append board colour [ coord ]) GameBoard.emptyBoard
