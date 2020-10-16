module ShapeTests exposing (suite)

import AsciiGrid
import Block
import Expect exposing (Expectation)
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
                |> Expect.equal (Just <| AsciiGrid.build expectedShape)


rotateXTimes : Shape.RotationDirection -> Int -> Shape -> Shape
rotateXTimes direction turns shape =
    List.range 0 (turns - 1) |> List.foldl (\_ shape_ -> Shape.rotate direction shape_) shape


findShape : String -> Maybe Shape
findShape asciiShape =
    let
        blocks =
            AsciiGrid.build asciiShape
    in
    Shape.builders
        |> (\( first, rest ) -> first :: rest)
        |> List.map (\buildShape -> buildShape Block.Blue)
        |> List.filter (\shape_ -> (Shape.data shape_ |> .blocks |> List.sort) == blocks)
        |> List.head


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
