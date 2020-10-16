module ShapeTests exposing (suite)

import Block
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing ((|.), (|=), Parser)
import Shape exposing (Shape)
import Test exposing (..)


{-| Tests for the Shape module. Shape is an opaque type so we can't construct shapes ourselves here. Instead, we use the
shapeBuilders function and look for specific block shapes from it to create specific shapes in our tests.

TODO: investigate to see if there's a better solution: seems a bit hacky (though it's only for tests so maybe it's OK.)

-}
suite : Test
suite =
    describe "Shape"
        [ describe "rotate" <|
            List.concat
                [ allRotationTests "L-shape" ellShape
                , allRotationTests "L-shape-mirror" ellShapeMirror
                , allRotationTests "Z-shape" zedShape
                , allRotationTests "Z-shape-mirror" zedShapeMirror
                , allRotationTests "Almost-plus-sign-shape" almostPlusSignShape
                , allRotationTests "Straight-line" lineShape
                , allRotationTests "Square" square
                ]
        ]


allRotationTests : String -> ShapeAsString -> List Test
allRotationTests shapeDescr { org, clockwise, anticlockwise, oneEighty } =
    [ rotationTest (shapeDescr ++ " clockwise turn once") org Shape.Clockwise 1 clockwise
    , rotationTest (shapeDescr ++ " anticlockwise turn once") org Shape.Anticlockwise 1 anticlockwise
    , rotationTest (shapeDescr ++ " clockwise turn twice") org Shape.Clockwise 2 oneEighty
    , rotationTest (shapeDescr ++ " anticlockwise turn twice") org Shape.Anticlockwise 2 oneEighty
    ]


rotationTest : String -> String -> Shape.RotationDirection -> Int -> String -> Test
rotationTest testDescr orgShape direction turns expectedShape =
    test testDescr <|
        \_ ->
            findShape orgShape
                |> Maybe.map (rotateXTimes direction turns)
                |> Maybe.map Shape.data
                |> Maybe.map .blocks
                |> Maybe.map List.sort
                |> Expect.equal (Just <| shapeBlocks expectedShape)


rotateXTimes : Shape.RotationDirection -> Int -> Shape -> Shape
rotateXTimes direction turns shape =
    List.range 0 (turns - 1) |> List.foldl (\_ shape_ -> Shape.rotate direction shape_) shape


findShape : String -> Maybe Shape
findShape =
    shapeBlocks >> findShapeFromBlocks


findShapeFromBlocks : List Block.Coord -> Maybe Shape
findShapeFromBlocks blocks =
    let
        soughtBlocks =
            List.sort blocks
    in
    Shape.builders
        |> List.map (\buildShape -> buildShape Block.Blue)
        |> List.filter (\shape -> (Shape.data shape |> .blocks |> List.sort) == soughtBlocks)
        |> List.head


shapeBlocks : String -> List Block.Coord
shapeBlocks string =
    let
        lineCoords : Int -> String -> List Block.Coord
        lineCoords lineIndex lineText =
            String.indexes "x" lineText
                |> List.map (\xIndex -> ( xIndex, lineIndex ))
    in
    string
        |> String.dropLeft 1
        |> String.dropRight 1
        |> String.lines
        |> List.reverse
        |> List.indexedMap lineCoords
        |> List.concat
        |> List.sort


type alias ShapeAsString =
    { org : String, clockwise : String, anticlockwise : String, oneEighty : String }


ellShape : ShapeAsString
ellShape =
    { org = """
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


ellShapeMirror : ShapeAsString
ellShapeMirror =
    { org = """
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


zedShape : ShapeAsString
zedShape =
    { org = """
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


zedShapeMirror : ShapeAsString
zedShapeMirror =
    { org = """
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


almostPlusSignShape : ShapeAsString
almostPlusSignShape =
    { org = """
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


square : ShapeAsString
square =
    let
        squareBlocks =
            """
xx
xx
"""
    in
    { org = squareBlocks
    , clockwise = squareBlocks
    , anticlockwise = squareBlocks
    , oneEighty = squareBlocks
    }


lineShape : ShapeAsString
lineShape =
    { org = """
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
