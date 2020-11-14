module GameView exposing (view)

{-| This module is responsible for rendering the game. Uses SVG but exposes no SVG information so that a different
rendering technology can be swapped in later if required.
-}

import BlockColour exposing (BlockColour)
import Board
import Color exposing (Color)
import Coord exposing (Coord)
import HighlightAnimation
import Html exposing (Html)
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgT


{-| Renders the current state of the board into an HTML element, using SVG.
-}
view : List ( Coord, BlockColour ) -> Maybe HighlightAnimation.Model -> Html msg
view normalBlocks highlightAnimation =
    let
        normalBlocksSvg =
            normalBlocks
                |> List.map
                    (\( coord, colour ) ->
                        drawBlock coord (BlockColour.toLightColour colour) (BlockColour.toDarkColour colour)
                    )
                |> List.concat

        highlightedBlocksSvg =
            case highlightAnimation of
                Just animation ->
                    HighlightAnimation.animatedBlocks animation
                        -- TODO: could cache colour calculations here per colour
                        |> List.map
                            (\( coord, colour ) ->
                                drawBlock coord
                                    (HighlightAnimation.animatedColour animation (BlockColour.toLightColour colour))
                                    (HighlightAnimation.animatedColour animation (BlockColour.toDarkColour colour))
                            )
                        |> List.concat

                Nothing ->
                    []
    in
    svg
        [ SvgA.width boardSizeX, SvgA.height boardSizeY ]
        ([ Svg.rect
            [ SvgA.width <| SvgT.percent 100
            , SvgA.height <| SvgT.percent 100
            , SvgA.rx <| SvgT.px 5
            , SvgA.ry <| SvgT.px 5
            , SvgA.fill <| SvgT.Paint Color.black
            ]
            []
         ]
            ++ grid
            ++ normalBlocksSvg
            ++ highlightedBlocksSvg
        )


{-| Draws the vertical and horizontal lines on the board that make it look like a grid.
-}
grid : List (Svg msg)
grid =
    let
        drawLines : Int -> (SvgT.Length -> Svg msg) -> List (Svg msg)
        drawLines cellCount drawLine =
            List.range 1 (cellCount - 1)
                |> List.map (\index -> index |> toFloat |> (*) cellSize |> SvgT.px)
                |> List.map drawLine
    in
    drawLines Board.colCount (\x -> gridLine x (SvgT.px 0) x boardSizeY)
        ++ drawLines Board.rowCount (\y -> gridLine (SvgT.px 0) y boardSizeX y)


gridLine : SvgT.Length -> SvgT.Length -> SvgT.Length -> SvgT.Length -> Svg msg
gridLine x1 y1 x2 y2 =
    Svg.line
        [ SvgA.x1 x1
        , SvgA.y1 y1
        , SvgA.x2 x2
        , SvgA.y2 y2
        , SvgA.stroke <| SvgT.Paint Color.lightGray
        , SvgA.strokeWidth <| SvgT.px 0.5
        ]
        []


{-| Draws a block at the given coordinate, and of the given colour.
-}
drawBlock : Coord -> Color -> Color -> List (Svg msg)
drawBlock coord lightColour darkColour =
    let
        ( x1, y1 ) =
            coordToGridPos coord |> Tuple.mapBoth ((+) 1) ((+) 1)

        innerSize =
            cellSize - 2
    in
    [ -- The main block (square)
      Svg.rect
        [ SvgA.x <| SvgT.px x1
        , SvgA.y <| SvgT.px y1
        , SvgA.width <| SvgT.px innerSize
        , SvgA.height <| SvgT.px innerSize
        , SvgA.fill <| SvgT.Paint lightColour
        ]
        []
    , -- A triangle of a slightly darker colour.
      Svg.polygon
        [ SvgA.points
            [ ( x1 + innerSize, y1 + innerSize )
            , ( x1 + innerSize, y1 )
            , ( x1, y1 + innerSize )
            ]
        , SvgA.fill <| SvgT.Paint darkColour
        ]
        []
    ]


{-| Gets the position on the grid of the bottom left hand corner of a cell with the supplied coordinates.
-}
coordToGridPos : Coord -> ( Float, Float )
coordToGridPos ( x, y ) =
    ( toFloat x * cellSize, Board.rowCount - y - 1 |> toFloat |> (*) cellSize )


{-| The width and height of each cell, in pixels.
-}
cellSize : Float
cellSize =
    30


{-| The horizontal size of the board, in pixels.
-}
boardSizeX : SvgT.Length
boardSizeX =
    cellSize * toFloat Board.colCount |> SvgT.px


{-| The vertical size of the board, in pixels.
-}
boardSizeY : SvgT.Length
boardSizeY =
    cellSize * toFloat Board.rowCount |> SvgT.px
