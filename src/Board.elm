module Board exposing (Board, append, areCellsAvailable, emptyBoard, occupiedCells, xSize, ySize)

{-| This module contains functionality related to representing a board. This is a 10x20 grid with cells, which can either
be empty or have a block in them. Importantly, the board represents only _landed_ blocks: the shape which is currently
dropping (and which is rendered onto the grid represented by the board), is _not_ part of the data in the board.
-}

import Array exposing (Array)
import Block


type Board
    = Board (Array Row)


type Cell
    = Empty
    | Occupied Block.Colour


type alias Row =
    Array Cell


{-| Represents a cell on the board, which has a block in it (i.e. is already occupied).
-}
type alias OccupiedCell =
    { coord : Block.Coord, colour : Block.Colour }


emptyRow : Row
emptyRow =
    Array.repeat xSize Empty


{-| Gets the empty board to use at the start of the game.
-}
emptyBoard : Board
emptyBoard =
    Board <| Array.repeat ySize emptyRow


{-| Gets a list of all the occupied cells in the supplied board.
-}
occupiedCells : Board -> List OccupiedCell
occupiedCells (Board board) =
    let
        rowPopulatedCells : Int -> Row -> List OccupiedCell
        rowPopulatedCells y row =
            row
                |> Array.indexedMap
                    (\x cell ->
                        case cell of
                            Empty ->
                                []

                            Occupied colour ->
                                [ { coord = ( x, y ), colour = colour } ]
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
append : Board -> Block.Colour -> List Block.Coord -> Board
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
xSize =
    10


{-| The size of the grid along its y-axis, i.e. how tall the board is, counted in the number of cells.
-}
ySize =
    20
