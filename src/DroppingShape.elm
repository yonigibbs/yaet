module DroppingShape exposing (DroppingShape, calcBoardCoords, calcShapeBlocksBoardCoords)

import Coord exposing (Coord)
import Shape exposing (Shape)


{-| A shape currently dropping on the board (either in the actual game, or on the welcome screen). Contains the `Shape`
itself, and its coordinates. The latter are the coordinates on the board of the bottom left corner of the grid which
contains the shape (see comments on the `Shape` module itself for more info).
-}
type alias DroppingShape =
    { shape : Shape -- The shape itself
    , gridCoord : Coord -- The coordinates of the bottom-left corner of the grid containing the shape, on the board
    }


{-| Calculates the coordinates of the blocks of the supplied dropping shape on the board. The dropping shape's blocks'
coordinates are relative to the coordinates of the shape itself.
-}
calcShapeBlocksBoardCoords : DroppingShape -> List Coord
calcShapeBlocksBoardCoords { gridCoord, shape } =
    Shape.data shape
        |> .blocks
        |> calcBoardCoords gridCoord


{-| Like `calcShapeBlocksBoardCoords` but not specifically tied to the `DroppingShape` type, to allow more general usage
(e.g. on the welcome screen where it's a letter being dropped, not the opaque `Shape` type).
-}
calcBoardCoords : Coord -> List Coord -> List Coord
calcBoardCoords gridCoordOnBoard blockCoordsInGrid =
    let
        ( shapeX, shapeY ) =
            gridCoordOnBoard
    in
    blockCoordsInGrid |> List.map (\( x, y ) -> ( x + shapeX, y + shapeY ))
