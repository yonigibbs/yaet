module ShapeUtils exposing (ExpectedShape, Orientation(..), ShapeType(..), expectEquals, getExpectedShape, getShape)

import AsciiGrid
import Coord exposing (Coord)
import Dict
import Expect exposing (Expectation)
import Shape exposing (Shape)


{-| The different shapes that can appear in the game.
-}
type ShapeType
    = LShape
    | LMirrorShape
    | ZShape
    | SShape
    | TShape
    | Square
    | Line


{-| The four orientations a shape can be in when rotated.
-}
type Orientation
    = InitialOrientation
    | ClockwiseOrientation
    | AnticlockwiseOrientation
    | OneEightyOrientation


{-| Defines a shape that we can test against, namely the coordinates of the blocks in the shape.
-}
type ExpectedShape
    = ExpectedShape (List Coord)


{-| Gets a shape of the given type, when the given colour, in the initial orientation in which it will be shown in the
game.
-}
getShape : ShapeType -> Shape
getShape shapeType =
    -- Shape is an opaque type so we can't construct shapes ourselves here. Instead, we use the `allShapes` function
    -- and ask it to create all the shapes for us, then we look for the one that has the same blocks as those which we're
    -- trying to create.
    let
        blockCoords : List Coord
        blockCoords =
            shapeCoords InitialOrientation shapeType
    in
    Shape.allShapes
        |> (\( first, rest ) -> first :: rest)
        |> List.filter (\shape_ -> (Shape.data shape_ |> .blocks |> List.sort) == blockCoords)
        |> List.head
        -- Crash the test if shape not found - means a bug in the test code somewhere.
        |> (\maybeHead ->
                case maybeHead of
                    Just head ->
                        head

                    Nothing ->
                        Debug.todo "Invalid shape"
           )


{-| Gets an expected shape (shape data to test actual Shape values against), of the given type, colour and orientation.
-}
getExpectedShape : Orientation -> ShapeType -> ExpectedShape
getExpectedShape orientation shapeType =
    ExpectedShape (shapeCoords orientation shapeType)


{-| Gets a list of the coordinates of a shape of the given type, when at the given orientation.
-}
shapeCoords : Orientation -> ShapeType -> List Coord
shapeCoords orientation shapeType =
    let
        asciiShape =
            toAsciiShapeTemplate shapeType
                |> toAsciiShape orientation
    in
    AsciiGrid.build asciiShape (Dict.fromList [ ( 'x', () ) ])
        |> List.map Tuple.first


{-| An expectation to be used in tests, which will ensure that the actual shape matches the expected shape.
-}
expectEquals : ExpectedShape -> Shape -> Expectation
expectEquals (ExpectedShape expectedBlocks) actual =
    Shape.data actual |> .blocks |> List.sort |> Expect.equal expectedBlocks


toAsciiShape : Orientation -> AsciiShapeTemplate -> String
toAsciiShape orientation =
    case orientation of
        InitialOrientation ->
            .initial

        ClockwiseOrientation ->
            .clockwise

        AnticlockwiseOrientation ->
            .anticlockwise

        OneEightyOrientation ->
            .oneEighty


toAsciiShapeTemplate : ShapeType -> AsciiShapeTemplate
toAsciiShapeTemplate shapeType =
    case shapeType of
        LShape ->
            lShape

        LMirrorShape ->
            lMirrorShape

        ZShape ->
            zShape

        SShape ->
            sShape

        TShape ->
            tShape

        Square ->
            square

        Line ->
            line


{-| A record containing ascii representations of a shape at its four possible orientations.
-}
type alias AsciiShapeTemplate =
    { initial : String, clockwise : String, anticlockwise : String, oneEighty : String }


lShape : AsciiShapeTemplate
lShape =
    { initial = """
--x
xxx
---
"""
    , clockwise = """
-x-
-x-
-xx
"""
    , anticlockwise = """
xx-
-x-
-x-
"""
    , oneEighty = """
---
xxx
x--
"""
    }


lMirrorShape : AsciiShapeTemplate
lMirrorShape =
    { initial = """
x--
xxx
---
"""
    , clockwise = """
-xx
-x-
-x-
"""
    , anticlockwise = """
-x-
-x-
xx-
"""
    , oneEighty = """
---
xxx
--x
"""
    }


zShape : AsciiShapeTemplate
zShape =
    { initial = """
xx-
-xx
---
"""
    , clockwise = """
--x
-xx
-x-
"""
    , anticlockwise = """
-x-
xx-
x--
"""
    , oneEighty = """
---
xx-
-xx
"""
    }


sShape : AsciiShapeTemplate
sShape =
    { initial = """
-xx
xx-
---
"""
    , clockwise = """
-x-
-xx
--x
"""
    , anticlockwise = """
x--
xx-
-x-
"""
    , oneEighty = """
---
-xx
xx-
"""
    }


tShape : AsciiShapeTemplate
tShape =
    { initial = """
-x-
xxx
---
"""
    , clockwise = """
-x-
-xx
-x-
"""
    , anticlockwise = """
-x-
xx-
-x-
"""
    , oneEighty = """
---
xxx
-x-
"""
    }


square : AsciiShapeTemplate
square =
    let
        squareBlocks =
            """
xx
xx
"""
    in
    { initial = squareBlocks
    , clockwise = squareBlocks
    , anticlockwise = squareBlocks
    , oneEighty = squareBlocks
    }


line : AsciiShapeTemplate
line =
    { initial = """
----
xxxx
----
----
"""
    , clockwise = """
--x-
--x-
--x-
--x-
"""
    , anticlockwise = """
-x--
-x--
-x--
-x--
"""
    , oneEighty = """
----
----
xxxx
----
"""
    }
