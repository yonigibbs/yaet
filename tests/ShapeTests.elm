module ShapeTests exposing (suite)

import Shape exposing (Shape)
import ShapeUtils
import Test exposing (..)


suite : Test
suite =
    describe "Shape"
        [ describe "rotate" <|
            List.concat
                [ allRotationTests "L-shape" ShapeUtils.Ell
                , allRotationTests "L-shape-mirror" ShapeUtils.EllMirror
                , allRotationTests "Z-shape" ShapeUtils.Zed
                , allRotationTests "Z-shape-mirror" ShapeUtils.ZedMirror
                , allRotationTests "Half-plus-sign-shape" ShapeUtils.HalfPlus
                , allRotationTests "Straight-line" ShapeUtils.Line
                , allRotationTests "Square" ShapeUtils.Square
                ]
        ]


allRotationTests : String -> ShapeUtils.ShapeType -> List Test
allRotationTests shapeDescr shapeType =
    [ rotationTest (shapeDescr ++ " clockwise turn once") shapeType Shape.Clockwise 1 ShapeUtils.ClockwiseOrientation
    , rotationTest (shapeDescr ++ " clockwise turn twice") shapeType Shape.Clockwise 2 ShapeUtils.OneEightyOrientation
    , rotationTest (shapeDescr ++ " clockwise turn three times") shapeType Shape.Clockwise 3 ShapeUtils.AnticlockwiseOrientation
    , rotationTest (shapeDescr ++ " clockwise turn four times") shapeType Shape.Clockwise 4 ShapeUtils.InitialOrientation
    , rotationTest (shapeDescr ++ " anticlockwise turn once") shapeType Shape.Anticlockwise 1 ShapeUtils.AnticlockwiseOrientation
    , rotationTest (shapeDescr ++ " anticlockwise turn twice") shapeType Shape.Anticlockwise 2 ShapeUtils.OneEightyOrientation
    , rotationTest (shapeDescr ++ " anticlockwise turn three times") shapeType Shape.Anticlockwise 3 ShapeUtils.ClockwiseOrientation
    , rotationTest (shapeDescr ++ " anticlockwise turn four times") shapeType Shape.Anticlockwise 4 ShapeUtils.InitialOrientation
    ]


rotationTest : String -> ShapeUtils.ShapeType -> Shape.RotationDirection -> Int -> ShapeUtils.Orientation -> Test
rotationTest testDescr shapeType direction turns expectedOrientation =
    test testDescr <|
        \_ ->
            let
                expectedShape =
                    ShapeUtils.getExpectedShape expectedOrientation shapeType
            in
            ShapeUtils.getShape shapeType
                |> rotateXTimes direction turns
                |> ShapeUtils.expectEquals expectedShape


rotateXTimes : Shape.RotationDirection -> Int -> Shape -> Shape
rotateXTimes direction turns shape =
    List.range 1 turns |> List.foldl (\_ shape_ -> Shape.rotate direction shape_) shape
