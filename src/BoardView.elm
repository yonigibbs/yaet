module BoardView exposing (BlockViewInfo, BorderStyle(..), Config, view, withColour, withOpacity)

{-| This module is responsible for rendering a board (typically during a game, but also used in the welcome screen).

Uses SVG but exposes no SVG information so that a different rendering technology can be swapped in later if required.

-}

import Color exposing (Color)
import Coord exposing (Coord)
import Element exposing (Element)
import Element.Border
import HighlightAnimation
import Shape
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgT


{-| Defines the configuration information required to render the board.

  - `cellSize`: The width and height of each cell, in pixels.
  - `rowCount`: The number of rows in the board.
  - `colCount`: The number of columns in the board.
  - `borderStyle`: The type of border to put around the board.
  - `showGridLines`: Whether to show grid lines or not. Generally true (e.g. for a game), but false when for example
    showing a preview of the upcoming shape next to the actual game board.

-}
type alias Config =
    { cellSize : Int, rowCount : Int, colCount : Int, borderStyle : BorderStyle, showGridLines : Bool }


{-| Defines the style of the border to be applied to the board:

  - `Solid`: renders a solid line around the board. Used for a normal game.
  - `Fade`: fades the edges of the board out into the supplied colour. Used in the welcome screen.
  - `None`: no border is shown (e.g. when showing a preview of the upcoming shape next to the actual game board).

-}
type BorderStyle
    = Solid
    | Fade Element.Color
    | None


{-| Describes a normal block to be rendered, namely its coordinates on the board, its colour, and its opacity.
-}
type alias BlockViewInfo =
    { coord : Coord, colour : Shape.BlockColour, opacity : Float }


{-| Describes how a block should be filled in.

  - `Filled`: a normal block, filled in in a given colour, with the opacity supplied here.
  - `Unfilled`: a block used to show a preview of where a landing should would eventually land, shown unfilled.

-}
type BlockFillType
    = Filled Float
    | Unfilled


{-| Renders the current state of the board into an HTML element, using SVG. Parameters:

  - `config`: configuration information required to render the board (e.g. the sizes, etc).
  - `showPauseOverlay`: whether to overlay the board with a "Pause" image (used when the game is paused).
  - `normalBlocks`: the normal blocks (e.g. the landed blocks, but the blocks of the currently dropping shape, if it's
    not to be animated at the moment).
  - `previewLandingBlocks`: the coordinates and colour of the blocks of the currently dropping shape in the position
    where it would land if it were not to be moved left or right. These are shown as not-filled-in.
  - `highlightAnimation`: if an animation is currently ongoing (e.g. for a shape which has just landed, or for rows
    being removed) this should describe the animation (e.g. the type of animation, its current progress, and the blocks
    in it).

-}
view : Config -> Bool -> List BlockViewInfo -> List ( Coord, Shape.BlockColour ) -> Maybe HighlightAnimation.Model -> Element msg
view ({ borderStyle, showGridLines } as config) showPauseOverlay normalBlocks previewLandingBlocks highlightAnimation =
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

                None ->
                    ( [], [] )

        background =
            Svg.rect
                [ SvgA.width <| SvgT.percent 100
                , SvgA.height <| SvgT.percent 100
                , SvgA.rx <| SvgT.px 5
                , SvgA.ry <| SvgT.px 5
                , SvgA.fill <| SvgT.Paint Color.black
                ]
                []

        highlightedBlocks =
            case highlightAnimation of
                Just animation ->
                    HighlightAnimation.animatedBlocks animation
                        |> withOpacity (HighlightAnimation.animatedOpacity animation)
                        |> asFilled
                        |> drawBlocks config (HighlightAnimation.animatedColour animation)

                Nothing ->
                    []

        gridSvg =
            if showGridLines then
                grid config

            else
                []

        pauseOverlaySvg =
            if showPauseOverlay then
                pauseOverlay

            else
                []
    in
    Element.el borderAttrs
        (Element.html <|
            svg
                [ SvgA.width <| boardSizeX config, SvgA.height <| boardSizeY config ]
                ([ blockSvgDefs config.cellSize, background ]
                    ++ gridSvg
                    ++ drawBlocks config identity (asUnfilled previewLandingBlocks)
                    ++ drawBlocks config identity (asFilled normalBlocks)
                    ++ highlightedBlocks
                    ++ overlay
                    ++ pauseOverlaySvg
                )
        )


{-| Gets a rectangle with 60% opacity to overlay the full board rectangle (to fade its original contents out a bit), with
a "Pause" image on it. Used when the game is paused.
-}
pauseOverlay : List (Svg msg)
pauseOverlay =
    let
        bar x =
            Svg.rect
                [ SvgA.width <| SvgT.percent 10
                , SvgA.height <| SvgT.percent 20
                , SvgA.x <| SvgT.percent x
                , SvgA.y <| SvgT.percent 40
                , SvgA.rx <| SvgT.px 3
                , SvgA.ry <| SvgT.px 3
                , SvgA.fill <| SvgT.Paint Color.white
                ]
                []
    in
    [ Svg.rect
        [ SvgA.width <| SvgT.percent 100
        , SvgA.height <| SvgT.percent 100
        , SvgA.fill <| SvgT.Paint <| Color.rgb255 50 50 50
        , SvgA.opacity <| SvgT.Opacity 0.6
        ]
        []
    , bar 35
    , bar 55
    ]


{-| Converts a list of tuples containing coordinates and colour into a list of `BlockViewInfo`, by setting the specified
opacity on each one (and converting the tuples to records).
-}
withOpacity : Float -> List ( Coord, Shape.BlockColour ) -> List BlockViewInfo
withOpacity opacity blocks =
    blocks |> List.map (\( coord, colour ) -> { coord = coord, colour = colour, opacity = opacity })


{-| Converts the passed in list of `BlockViewInfo` records to a list of records in the format required by the
`drawBlocks` function, setting their `fillType` to `Filled`, with the given opacity.
-}
asFilled : List BlockViewInfo -> List { coord : Coord, colour : Shape.BlockColour, fillType : BlockFillType }
asFilled blocks =
    blocks |> List.map (\{ coord, colour, opacity } -> { coord = coord, colour = colour, fillType = Filled opacity })


{-| Converts the passed in list of `BlockViewInfo` records to a list of records in the format required by the
`drawBlocks` function, setting their `fillType` to `Unfilled`.
-}
asUnfilled : List ( Coord, Shape.BlockColour ) -> List { coord : Coord, colour : Shape.BlockColour, fillType : BlockFillType }
asUnfilled blocks =
    blocks |> List.map (\( coord, colour ) -> { coord = coord, colour = colour, fillType = Unfilled })


{-| Converts a list of coordinates to a tuple containing the coordinates and the given colour.
-}
withColour : Shape.BlockColour -> List Coord -> List ( Coord, Shape.BlockColour )
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


{-| Draws the supplied blocks using the given functions to get the actual colours to apply to each one. `colourConverter`
is a function which is used to take the block's default colour and transform it (e.g. during an animation).
-}
drawBlocks : Config -> (Color -> Color) -> List { coord : Coord, colour : Shape.BlockColour, fillType : BlockFillType } -> List (Svg msg)
drawBlocks config colourConverter blocks =
    blocks
        |> List.map
            (\{ coord, colour, fillType } -> drawBlock config coord fillType (blockColour colour |> colourConverter))


{-| Draws a block at the given coordinate, and of the given colour.
-}
drawBlock : Config -> Coord -> BlockFillType -> Color -> Svg msg
drawBlock config coord fillType colour =
    let
        ( x1, y1 ) =
            coordToGridPos config coord
                |> Tuple.mapBoth ((+) 1) ((+) 1)
                |> Tuple.mapBoth toFloat toFloat

        fillTypeAttrs =
            case fillType of
                Filled opacity ->
                    [ SvgA.fill <| SvgT.Paint colour, SvgA.opacity <| SvgT.Opacity opacity ]

                Unfilled ->
                    [ SvgA.stroke <| SvgT.Paint colour, SvgA.opacity <| SvgT.Opacity 0.6 ]
    in
    Svg.use ([ SvgA.xlinkHref "#block", SvgA.x <| SvgT.px x1, SvgA.y <| SvgT.px y1 ] ++ fillTypeAttrs) []


{-| Gets the SVG `defs` which is referred to by every block. Defines the way that a block is rendered.

Taken from <https://upload.wikimedia.org/wikipedia/commons/3/39/Tetrominoes_IJLO_STZ_Worlds.svg>

-}
blockSvgDefs : Int -> Svg msg
blockSvgDefs cellSize =
    let
        blockSize =
            cellSize - 2 |> toFloat

        edgeSize =
            blockSize / 8

        raisedInnerSize =
            blockSize - (2 * edgeSize)

        numberPair : Float -> Float -> String
        numberPair f1 f2 =
            String.join " " [ String.fromFloat f1, String.fromFloat f2 ]
    in
    Svg.defs []
        [ Svg.g [ SvgA.id "block" ]
            [ Svg.rect [ SvgA.height <| SvgT.px blockSize, SvgA.width <| SvgT.px blockSize ] []

            -- TODO: use folkertdev/svg-path-dsl instead of strings here?
            , Svg.path
                [ SvgA.fill <| SvgT.Paint <| Color.rgb255 230 230 230
                , SvgA.opacity <| SvgT.Opacity 0.7
                , SvgA.d <|
                    String.join ","
                        [ "m0"
                        , numberPair 0 edgeSize
                        , numberPair edgeSize raisedInnerSize
                        , numberPair 0 edgeSize
                        , String.fromFloat -edgeSize
                        ]
                ]
                []
            , Svg.path
                [ SvgA.fill <| SvgT.Paint Color.black
                , SvgA.opacity <| SvgT.Opacity 0.1
                , SvgA.d <|
                    String.join ","
                        [ "m0"
                        , numberPair 0 edgeSize
                        , numberPair edgeSize 0
                        , numberPair raisedInnerSize -edgeSize
                        , String.fromFloat edgeSize ++ " m" ++ String.fromFloat blockSize
                        , numberPair -blockSize -edgeSize
                        , numberPair edgeSize 0
                        , numberPair raisedInnerSize edgeSize
                        , String.fromFloat edgeSize
                        ]
                ]
                []
            , Svg.path
                [ SvgA.fill <| SvgT.Paint Color.black
                , SvgA.opacity <| SvgT.Opacity 0.5
                , SvgA.d <|
                    String.join ","
                        [ "m0"
                        , numberPair blockSize edgeSize
                        , numberPair -edgeSize raisedInnerSize
                        , numberPair 0 edgeSize
                        , String.fromFloat edgeSize
                        ]
                ]
                []
            ]
        ]


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
        , SvgA.stroke <| SvgT.Paint <| Color.rgb255 30 30 30
        , SvgA.strokeWidth <| SvgT.px 1
        ]
        []


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


blockColour : Shape.BlockColour -> Color
blockColour colour =
    case colour of
        Shape.Cyan ->
            Color.rgb255 0 240 240

        Shape.Blue ->
            Color.rgb255 0 0 240

        Shape.Orange ->
            Color.rgb255 240 160 0

        Shape.Yellow ->
            Color.rgb255 240 240 0

        Shape.Green ->
            Color.rgb255 0 240 0

        Shape.Purple ->
            Color.rgb255 160 0 240

        Shape.Red ->
            Color.rgb255 240 0 0
