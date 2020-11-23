module BoardView exposing (Config, view)

{-| This module is responsible for rendering a board (typically during a game, but also used in the welcome scree).

Uses SVG but exposes no SVG information so that a different rendering technology can be swapped in later if required.

-}

import BlockColour exposing (BlockColour)
import Color exposing (Color)
import Coord exposing (Coord)
import HighlightAnimation
import Html exposing (Html)
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgT


{-| Defines the configuration information required to render the board.

  - `cellSize`: The width and height of each cell, in pixels.
  - `rowCount`: The number of rows in the board.
  - `colCount`: The number of columns in the board.

-}
type alias Config =
    { cellSize : Int, rowCount : Int, colCount : Int }


{-| Renders the current state of the board into an HTML element, using SVG.
-}
view : Config -> List ( Coord, BlockColour ) -> Maybe HighlightAnimation.Model -> Html msg
view config normalBlocks highlightAnimation =
    let
        normalBlocksSvg =
            normalBlocks
                |> List.map
                    (\( coord, colour ) ->
                        drawBlock
                            config
                            coord
                            (BlockColour.toLightColour colour)
                            (BlockColour.toDarkColour colour)
                    )
                |> List.concat

        highlightedBlocksSvg =
            case highlightAnimation of
                Just animation ->
                    HighlightAnimation.animatedBlocks animation
                        -- TODO: could cache colour calculations here per colour
                        |> List.map
                            (\( coord, colour ) ->
                                drawBlock
                                    config
                                    coord
                                    (HighlightAnimation.animatedColour animation (BlockColour.toLightColour colour))
                                    (HighlightAnimation.animatedColour animation (BlockColour.toDarkColour colour))
                            )
                        |> List.concat

                Nothing ->
                    []
    in
    svg
        [ SvgA.width <| boardSizeX config, SvgA.height <| boardSizeY config ]
        ([ Svg.defs []
            [ Svg.radialGradient [ SvgA.id "fade-out-overlay" ]
                [ Svg.stop [ SvgA.offset "35%", SvgA.stopOpacity <| SvgT.Opacity 0, SvgA.stopColor "rgb(30,30,30)" ] []
                , Svg.stop [ SvgA.offset "100%", SvgA.stopOpacity <| SvgT.Opacity 100, SvgA.stopColor "rgb(30,30,30)" ] []
                ]

            --Svg.radialGradient [ SvgA.id "fill-fade" ]
            --    [ Svg.stop [ SvgA.offset "75%", SvgA.stopColor "rgb(0,0,0)" ] []
            --    , Svg.stop [ SvgA.offset "100%", SvgA.stopColor "rgb(30,30,30)" ] []
            --    ]
            --, Svg.linearGradient
            --    [ SvgA.id "horizontal-line-fade"
            --    , SvgA.x1 <| SvgT.percent 0
            --    , SvgA.x2 <| SvgT.percent 100
            --    , SvgA.y1 <| SvgT.percent 0
            --    , SvgA.y2 <| SvgT.percent 0
            --    ]
            --    [ Svg.stop [ SvgA.offset "0%", SvgA.stopColor "rgb(30,30,30)" ] []
            --    , Svg.stop [ SvgA.offset "50%", SvgA.stopColor "rgb(238,238,236)" ] []
            --    , Svg.stop [ SvgA.offset "100%", SvgA.stopColor "rgb(30,30,30)" ] []
            --    ]
            --, Svg.linearGradient
            --    [ SvgA.id "vertical-line-fade"
            --    , SvgA.x1 <| SvgT.percent 0
            --    , SvgA.x2 <| SvgT.percent 0
            --    , SvgA.y1 <| SvgT.percent 0
            --    , SvgA.y2 <| SvgT.percent 100
            --    ]
            --    [ Svg.stop [ SvgA.offset "0%", SvgA.stopColor "rgb(30,30,30)" ] []
            --    , Svg.stop [ SvgA.offset "50%", SvgA.stopColor "rgb(238,238,236)" ] []
            --    , Svg.stop [ SvgA.offset "100%", SvgA.stopColor "rgb(30,30,30)" ] []
            --    ]
            ]
         , Svg.rect
            [ SvgA.width <| SvgT.percent 100
            , SvgA.height <| SvgT.percent 100
            , SvgA.rx <| SvgT.px 5
            , SvgA.ry <| SvgT.px 5
            , SvgA.fill <| SvgT.Paint Color.black

            --, SvgA.fill <| SvgT.Reference "fill-fade"
            ]
            []
         ]
            ++ grid config
            ++ normalBlocksSvg
            ++ highlightedBlocksSvg
            ++ [ Svg.rect
                    [ SvgA.width <| SvgT.percent 100
                    , SvgA.height <| SvgT.percent 100
                    , SvgA.rx <| SvgT.px 5
                    , SvgA.ry <| SvgT.px 5
                    , SvgA.fill <| SvgT.Reference "fade-out-overlay"
                    ]
                    []
               ]
        )


{-| Draws the vertical and horizontal lines on the board that make it look like a grid.
-}
grid : Config -> List (Svg msg)
grid ({ cellSize, colCount, rowCount } as config) =
    let
        drawLines : Int -> LineDirection -> List (Svg msg)
        drawLines cellCount direction =
            List.range 1 (cellCount - 1)
                |> List.map (gridLine config direction)
    in
    drawLines colCount Vertical ++ drawLines rowCount Horizontal


type LineDirection
    = Horizontal
    | Vertical


gridLine : Config -> LineDirection -> Int -> Svg msg
gridLine ({ cellSize } as config) direction index =
    let
        { width, height, startPos, stroke } =
            case direction of
                Horizontal ->
                    { width = boardSizeX config, height = SvgT.px 0.2, startPos = ( 0, index * cellSize ), stroke = "horizontal-line-fade" }

                Vertical ->
                    { width = SvgT.px 0.2, height = boardSizeY config, startPos = ( index * cellSize, 0 ), stroke = "vertical-line-fade" }

        ( x, y ) =
            startPos |> Tuple.mapBoth (toFloat >> SvgT.px) (toFloat >> SvgT.px)
    in
    Svg.rect
        [ SvgA.x x
        , SvgA.y y
        , SvgA.width width
        , SvgA.height height
        , SvgA.stroke <| SvgT.Paint Color.lightGray

        --, SvgA.stroke <| SvgT.Reference stroke
        , SvgA.strokeWidth <| SvgT.px 0.2
        ]
        []


{-| Draws a block at the given coordinate, and of the given colour.
-}
drawBlock : Config -> Coord -> Color -> Color -> List (Svg msg)
drawBlock config coord lightColour darkColour =
    let
        ( x1, y1 ) =
            coordToGridPos config coord
                |> Tuple.mapBoth ((+) 1) ((+) 1)
                |> Tuple.mapBoth toFloat toFloat

        innerSize =
            config.cellSize - 2 |> toFloat
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
coordToGridPos : Config -> Coord -> ( Int, Int )
coordToGridPos { cellSize, rowCount } ( x, y ) =
    ( x * cellSize, (rowCount - y - 1) * cellSize )


{-| The horizontal size of the board, in pixels.
-}
boardSizeX : Config -> SvgT.Length
boardSizeX { cellSize, colCount } =
    cellSize * colCount |> toFloat |> SvgT.px


{-| The vertical size of the board, in pixels.
-}
boardSizeY : Config -> SvgT.Length
boardSizeY { cellSize, rowCount } =
    cellSize * rowCount |> toFloat |> SvgT.px
