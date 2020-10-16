module BoardTests exposing (suite)

{-| Tests for the Board module. Board is an opaque type so we can't construct it ourselves here, pre-populated in some
way. Instead, we start off with an empty board, then build it up by appending cells to it.
-}

import AsciiGrid
import Block
import Board exposing (Board)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Board"
        [ describe "occupiedCells" <|
            [ occupiedCellsTest "Empty board" "" []
            , occupiedCellsTest "Populated board" """
x----xx-xx-
x---x-xx-x-
""" [ ( 0, 0 ), ( 4, 0 ), ( 6, 0 ), ( 7, 0 ), ( 9, 0 ), ( 0, 1 ), ( 5, 1 ), ( 6, 1 ), ( 8, 1 ), ( 9, 1 ) ]
            ]
        , describe "append" <|
            [ appendTest "Append straight line at bottom left of empty board" "" "xxxx" "xxxx"
            , appendTest "Append Z on bottom row of board with cells" """
x----xx-xx-
x---x-xx-x-
""" """
---xx------
--xx-------
""" """
x--xxxx-xx-
x-xxx-xx-x-
"""
            ]
        , describe "areCellsAvailable"
            [ areCellsAvailableTest "Empty board" "" "xxxx" True
            , areCellsAvailableTest "Straight line into available space on bottom row" """
x----xx-xx-
x----xxx-x-
""" "-xxxx-----" True
            , areCellsAvailableTest "Straight line into unavailable space on bottom row" """
x----xx-xx-
x-x--xxx-x-
""" "-xxxx-----" False
            , areCellsAvailableTest "Straight line into available space on third row" """
x----x--xx-
x----xx-xx-
x-xxxxx-xx-
x-xx-xxx-x-
""" """
-xxxx-----
----------
----------
""" True
            , areCellsAvailableTest "Straight line into unavailable space on third row" """
x----x--xx-
x--x-xx-xx-
x-xxxxx-xx-
x-xx-xxx-x-
""" """
-xxxx-----
----------
----------
""" False
            ]
        ]


occupiedCellsTest : String -> String -> List Block.Coord -> Test
occupiedCellsTest testDescr asciiBoard expectedOccupiedCells =
    -- TODO: this currently doesn't test colours - add this in
    test testDescr <|
        \_ ->
            buildBoard asciiBoard
                |> Board.occupiedCells
                |> List.map .coord
                |> List.sort
                |> Expect.equal (List.sort expectedOccupiedCells)


areCellsAvailableTest : String -> String -> String -> Bool -> Test
areCellsAvailableTest testDescr asciiBoard asciiShape expectedAvailable =
    test testDescr <|
        \_ ->
            buildBoard asciiBoard
                |> (\board -> Board.areCellsAvailable board (AsciiGrid.build asciiShape))
                |> Expect.equal expectedAvailable


appendTest : String -> String -> String -> String -> Test
appendTest testDescr orgBoard newBlocks expectedBoard =
    test testDescr <|
        \_ ->
            Board.append (buildBoard orgBoard) Block.Blue (AsciiGrid.build newBlocks)
                |> Board.occupiedCells
                |> List.map .coord
                |> List.sort
                |> Expect.equal (AsciiGrid.build expectedBoard)


{-| Builds a board from the supplied ASCII grid. The supplied grid doesn't need to be the full 10x20: just whatever
portion contains occupied cells. The colour is unimportant currently so hard-coded to use blue. This will probably
change in future as colour-related tests are added.
-}
buildBoard : String -> Board
buildBoard asciiBoard =
    AsciiGrid.build asciiBoard
        |> Board.append Board.emptyBoard Block.Blue
