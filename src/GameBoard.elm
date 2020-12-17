module GameBoard exposing
    ( GameBoard
    , append
    , areCellsAvailable
    , colCount
    , completedRows
    , emptyBoard
    , occupiedCells
    , removeRows
    , rowCount
    )

{-| This module contains functionality related to representing a board during gameplay. This is a 10x20 grid with cells,
which can either be empty or have a block in them. Importantly, the board represents only _landed_ blocks: the shape
which is currently dropping (and which is rendered onto the grid represented by the board), is _not_ part of the data in
the board.
-}

import Array exposing (Array)
import Coord exposing (Coord)
import Shape


{-| Represents the board. This is a 10x20 grid with cells, which can either be empty or have a block in them.
Importantly, the board represents only _landed_ blocks: the shape which is currently dropping (and which is rendered
onto the grid represented by the board), is _not_ part of the data in the board.
-}
type GameBoard
    = GameBoard (Array Row)


{-| A cell in the board: either `Empty` or `Occupied`, in which case it has a colour associated with it.
-}
type Cell
    = Empty
    | Occupied Shape.BlockColour


{-| A row in the grid. An alias for an array of `Cell`s.
-}
type alias Row =
    Array Cell


emptyRow : Row
emptyRow =
    Array.repeat colCount Empty


{-| Gets the empty board to use at the start of the game.
-}
emptyBoard : GameBoard
emptyBoard =
    GameBoard <| Array.repeat rowCount emptyRow


{-| Gets a list of all the occupied cells in the supplied board.

Returns a list of tuples, where the first value in the tuple is the block's coordinates, and the second value is its
colour.

-}
occupiedCells : GameBoard -> List ( Coord, Shape.BlockColour )
occupiedCells (GameBoard board) =
    let
        rowPopulatedCells : Int -> Row -> List ( Coord, Shape.BlockColour )
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


isOccupiedCell : Cell -> Bool
isOccupiedCell =
    isEmptyCell >> not


{-| Checks whether all the supplied coordinates are free on the supplied board (and within its legal coordinates).
-}
areCellsAvailable : GameBoard -> List Coord -> Bool
areCellsAvailable (GameBoard board) coords =
    let
        isCellFree : Coord -> Bool
        isCellFree ( x, y ) =
            Array.get y board
                |> Maybe.andThen (Array.get x)
                |> Maybe.map isEmptyCell
                |> Maybe.withDefault False
    in
    -- TODO: If there are multiple cells in the same row, this will get that row from the board's array multiple times:
    -- this could be optimised. Might increase code complexity, and optimisation will probably be negligible. Investigate.
    List.all (\( x, y ) -> x >= 0 && x < colCount && y >= 0 && y < rowCount && isCellFree ( x, y )) coords


{-| Gets a list of the indexes of the completed rows, if any.
-}
completedRows : GameBoard -> List Int
completedRows (GameBoard rows) =
    rows
        |> Array.indexedMap (\index row -> ( index, Array.toList row |> List.all isOccupiedCell ))
        |> Array.filter (\( _, isCompleted ) -> isCompleted)
        |> Array.map Tuple.first
        |> Array.toList


{-| Removes the lines at the supplied indexes (adding new empty
-}
removeRows : GameBoard -> List Int -> GameBoard
removeRows (GameBoard rows) indexes =
    let
        keptRows =
            Array.toIndexedList rows
                |> List.filterMap
                    (\( index, row ) ->
                        if not <| List.member index indexes then
                            Just row

                        else
                            Nothing
                    )
                |> Array.fromList

        newEmptyRows =
            Array.repeat (List.length indexes) emptyRow
    in
    GameBoard <| Array.append keptRows newEmptyRows


{-| Appends the supplied coordinates as occupied cells onto the supplied board. Note that this doesn't automatically
removed any newly completed lines.
-}
append : GameBoard -> Shape.BlockColour -> List Coord -> GameBoard
append (GameBoard board) colour coords =
    let
        appendCell : Coord -> Array Row -> Array Row
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
    List.foldl appendCell board coords |> GameBoard


{-| The number of rows in the board.
-}
rowCount : Int
rowCount =
    20


{-| The number of columns in the board.
-}
colCount : Int
colCount =
    10
