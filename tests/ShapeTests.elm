module ShapeTests exposing (suite)

import Coord exposing (Coord)
import Expect
import Shape exposing (Shape)
import ShapeUtils
import Test exposing (..)


suite : Test
suite =
    describe "Shape"
        [ describe "rotate" <|
            List.concat
                [ allRotationTests "L-shape" ShapeUtils.LShape
                , allRotationTests "L-mirror-shape" ShapeUtils.LMirrorShape
                , allRotationTests "Z-shape" ShapeUtils.ZShape
                , allRotationTests "S-shape" ShapeUtils.SShape
                , allRotationTests "T-shape" ShapeUtils.TShape
                , allRotationTests "Line" ShapeUtils.Line
                , allRotationTests "Square" ShapeUtils.Square
                ]
        , describe "clippedBlocks" <|
            [ clippedBlocksTest "Straight-line"
                (ShapeUtils.getShape ShapeUtils.Line)
                [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
            , clippedBlocksTest "Straight-line (rotated clockwise once)"
                (ShapeUtils.getShape ShapeUtils.Line |> rotateXTimes Shape.Clockwise 1)
                [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
            , clippedBlocksTest "L-shape"
                (ShapeUtils.getShape ShapeUtils.LShape)
                [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 2, 1 ) ]
            , clippedBlocksTest "Square"
                (ShapeUtils.getShape ShapeUtils.Square)
                [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
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


clippedBlocksTest : String -> Shape -> List Coord -> Test
clippedBlocksTest testDescr shape expectedBlocks =
    test testDescr <|
        \_ ->
            shape
                |> Shape.clippedBlocks
                |> List.sort
                |> Expect.equal (List.sort expectedBlocks)
