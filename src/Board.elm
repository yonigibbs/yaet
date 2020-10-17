module Board exposing (Board, append, areCellsAvailable, emptyBoard, occupiedCells, xCellCount, yCellCount)

{-| This module contains functionality related to representing a board. This is a 10x20 grid with cells, which can either
be empty or have a block in them. Importantly, the board represents only _landed_ blocks: the shape which is currently
dropping (and which is rendered onto the grid represented by the board), is _not_ part of the data in the board.
-}

import Array exposing (Array)
import Block exposing (BlockColour)


{-| Represents the board. This is a 10x20 grid with cells, which can either be empty or have a block in them.
Importantly, the board represents only _landed_ blocks: the shape which is currently dropping (and which is rendered
onto the grid represented by the board), is _not_ part of the data in the board.
-}
type Board
    = Board (Array Row)


{-| A cell in the board: either `Empty` or `Occupied`, in which case it has a colour associated with it.
-}
type Cell
    = Empty
    | Occupied BlockColour


{-| A row in the grid. An alias for an array of `Cell`s.
-}
type alias Row =
    Array Cell


emptyRow : Row
emptyRow =
    Array.repeat xCellCount Empty


{-| Gets the empty board to use at the start of the game.
-}
emptyBoard : Board
emptyBoard =
    Board <| Array.repeat yCellCount emptyRow


{-| Gets a list of all the occupied cells in the supplied board.

Returns a list of tuples, where the first value in the tuple is the block's coordinates, and the second value is its
colour.

-}
occupiedCells : Board -> List ( Block.Coord, BlockColour )
occupiedCells (Board board) =
    let
        rowPopulatedCells : Int -> Row -> List ( Block.Coord, BlockColour )
        rowPopulatedCells y row =
            row
                |> Array.indexedMap
                    (\x cell ->
                        case cell of
                            Empty ->
                                []

                            Occupied colour ->
                                [ ( ( x, y ), colour ) ]
                    )
                |> Array.toList
                |> List.concat
    in
    board
        |> Array.indexedMap rowPopulatedCells
        |> Array.toList
        |> List.concat


isEmptyCell : Cell -> Bool
isEmptyCell cell =
    case cell of
        Empty ->
            True

        Occupied _ ->
            False


{-| Checks whether all the supplied coordinates are free on the supplied board.
-}
areCellsAvailable : Board -> List Block.Coord -> Bool
areCellsAvailable (Board board) coords =
    let
        isCellFree : Block.Coord -> Bool
        isCellFree ( x, y ) =
            Array.get y board
                |> Maybe.andThen (Array.get x)
                |> Maybe.map isEmptyCell
                |> Maybe.withDefault False
    in
    -- TODO: If there are multiple cells in the same row, this will get that row from the board's array multiple times:
    -- this could be optimised. Might increase code complexity, and optimisation will probably be negligible. Investigate.
    List.all isCellFree coords


{-| Appends the supplied coordinates as occupied cells onto the supplied board.
-}
append : Board -> BlockColour -> List Block.Coord -> Board
append (Board board) colour coords =
    let
        appendCell : Block.Coord -> Array Row -> Array Row
        appendCell ( x, y ) rows =
            -- Get the row
            Array.get y rows
                -- Set the cell in the row (and get back the updated row)
                |> Maybe.map (Array.set x (Occupied colour))
                -- Set this new row in the board (an array of arrays)
                |> Maybe.map (\row -> Array.set y row rows)
                -- If any of the actions above failed, just return the originally supplied board.
                |> Maybe.withDefault rows
    in
    List.foldl appendCell board coords |> Board


{-| The size of the grid along its x-axis, i.e. how wide the board is, counted in the number of cells.
-}
xCellCount =
    10


{-| The size of the grid along its y-axis, i.e. how tall the board is, counted in the number of cells.
-}
yCellCount =
    20