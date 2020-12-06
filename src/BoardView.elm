module BoardView exposing (BlockViewInfo, BorderStyle(..), Config, view, withColour, withOpacity)

{-| This module is responsible for rendering a board (typically during a game, but also used in the welcome screen).

Uses SVG but exposes no SVG information so that a different rendering technology can be swapped in later if required.

-}

import BlockColour exposing (BlockColour)
import Color exposing (Color)
import Coord exposing (Coord)
import Element exposing (Element)
import Element.Border
import HighlightAnimation
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgT


{-| Defines the configuration information required to render the board.

  - `cellSize`: The width and height of each cell, in pixels.
  - `rowCount`: The number of rows in the board.
  - `colCount`: The number of columns in the board.
  - `borderStyle`: The type of border to put around the board.

-}
type alias Config =
    { cellSize : Int, rowCount : Int, colCount : Int, borderStyle : BorderStyle }


{-| Defines the style of the border to be applied to the board:

  - `Solid`: renders a solid line around the board. Used for a normal game.
  - `Fade`: fades the edges of the board out into the supplied colour. Used in the welcome screen.

-}
type BorderStyle
    = Solid
    | Fade Element.Color


{-| Describes a block to be rendered, namely its coordinates on the board, its colour, and its opacity.
-}
type alias BlockViewInfo =
    { coord : Coord, colour : BlockColour, opacity : Float }


{-| Renders the current state of the board into an HTML element, using SVG.
-}
view : Config -> List BlockViewInfo -> Maybe HighlightAnimation.Model -> Element msg
view ({ borderStyle } as config) normalBlocks highlightAnimation =
    let
        ( overlay, borderAttrs ) =
            case borderStyle of
                Solid ->
                    ( []
                    , [ Element.Border.width 3
                      , Element.Border.color <| Element.rgb255 100 100 100
                      , Element.Border.glow (Element.rgb255 200 200 200) 1
                      ]
                    )

                Fade colourToFadeTo ->
                    ( fadeEdgesOverlay <| elmUIColourToColour colourToFadeTo, [] )

        background =
            Svg.rect
                [ SvgA.width <| SvgT.percent 100
                , SvgA.height <| SvgT.percent 100
                , SvgA.rx <| SvgT.px 5
                , SvgA.ry <| SvgT.px 5
                , SvgA.fill <| SvgT.Paint Color.black
                ]
                []

        normalBlocksSvg =
            drawBlocks config BlockColour.toLightColour BlockColour.toDarkColour normalBlocks

        highlightedBlocksSvg =
            case highlightAnimation of
                Just animation ->
                    HighlightAnimation.animatedBlocks animation
                        |> withOpacity (HighlightAnimation.animatedOpacity animation)
                        |> drawBlocks config
                            (BlockColour.toLightColour >> HighlightAnimation.animatedColour animation)
                            (BlockColour.toDarkColour >> HighlightAnimation.animatedColour animation)

                Nothing ->
                    []
    in
    Element.el borderAttrs
        (Element.html <|
            svg
                [ SvgA.width <| boardSizeX config, SvgA.height <| boardSizeY config ]
                ([ background ]
                    ++ grid config
                    ++ normalBlocksSvg
                    ++ highlightedBlocksSvg
                    ++ overlay
                )
        )


{-| Converts a list of tuples containing coordinates and colour into a list of `BlockViewInfo`, by setting the specified
opacity on each one (and converting the tuples to records).
-}
withOpacity : Float -> List ( Coord, BlockColour ) -> List BlockViewInfo
withOpacity opacity blocks =
    blocks |> List.map (\( coord, colour ) -> { coord = coord, colour = colour, opacity = opacity })


{-| Converts a list of coordinates to a tuple containing the coordinates and the given colour.
-}
withColour : BlockColour -> List Coord -> List ( Coord, BlockColour )
withColour colour coords =
    coords |> List.map (\coord -> ( coord, colour ))


{-| Renders an overlay of the given colour which is transparent in the middle but gradually increases its opacity, which
provides the effect of the edges of the board "fading out".
-}
fadeEdgesOverlay : Color -> List (Svg msg)
fadeEdgesOverlay colourToFadeTo =
    let
        fadeOutOverlayId =
            "fade-out-overlay"
    in
    [ Svg.defs []
        [ Svg.radialGradient [ SvgA.id fadeOutOverlayId ]
            [ Svg.stop [ SvgA.offset "35%", SvgA.stopOpacity <| SvgT.Opacity 0, SvgA.stopColor <| Color.toCssString colourToFadeTo ] []
            , Svg.stop [ SvgA.offset "100%", SvgA.stopOpacity <| SvgT.Opacity 100, SvgA.stopColor <| Color.toCssString colourToFadeTo ] []
            ]
        ]
    , Svg.rect
        [ SvgA.width <| SvgT.percent 100
        , SvgA.height <| SvgT.percent 100
        , SvgA.rx <| SvgT.px 5
        , SvgA.ry <| SvgT.px 5
        , SvgA.fill <| SvgT.Reference fadeOutOverlayId
        ]
        []
    ]


{-| Draws the supplied blocks using the given functions to get the actual colours to apply to each one.
-}
drawBlocks : Config -> (BlockColour -> Color) -> (BlockColour -> Color) -> List BlockViewInfo -> List (Svg msg)
drawBlocks config toLightColour toDarkColour blocks =
    blocks
        |> List.map
            (\{ coord, colour, opacity } -> drawBlock config coord opacity (toLightColour colour) (toDarkColour colour))
        |> List.concat


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


{-| Draws a line in the grid, in the given direction (top-to-bottom or left-to-right) at the given cell index.
-}
gridLine : Config -> LineDirection -> Int -> Svg msg
gridLine ({ cellSize } as config) direction index =
    let
        offset =
            index * cellSize |> toFloat |> SvgT.px

        { x1, y1, x2, y2 } =
            case direction of
                Horizontal ->
                    { x1 = SvgT.px 0, x2 = boardSizeX config, y1 = offset, y2 = offset }

                Vertical ->
                    { x1 = offset, x2 = offset, y1 = SvgT.px 0, y2 = boardSizeY config }
    in
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
drawBlock : Config -> Coord -> Float -> Color -> Color -> List (Svg msg)
drawBlock config coord opacity lightColour darkColour =
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
        , SvgA.opacity <| SvgT.Opacity opacity
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
        , SvgA.opacity <| SvgT.Opacity opacity
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


elmUIColourToColour : Element.Color -> Color
elmUIColourToColour =
    Element.toRgb >> Color.fromRgba
