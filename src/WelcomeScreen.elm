module WelcomeScreen exposing (view)

import BlockColour exposing (BlockColour)
import BoardView
import Coord exposing (Coord)
import Element exposing (Element)
import UIHelpers


view : msg -> Element msg
view startGameMsg =
    Element.column [ Element.spacingXY 0 25 ]
        [ BoardView.view boardViewConfig (lettersToBoardBlocks initialLetters) Nothing
        , Element.row [ Element.centerX ] [ UIHelpers.button "Start Game" startGameMsg ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15, rowCount = 20, colCount = 100, borderStyle = BoardView.Fade UIHelpers.mainBackgroundColour }


type alias DroppingLetter =
    { blocks : List Coord, colour : BlockColour, gridCoord : Coord }


initialLetters : List DroppingLetter
initialLetters =
    [ { blocks = tBlocks, colour = BlockColour.Blue, gridCoord = ( 35, 7 ) }
    , { blocks = eBlocks, colour = BlockColour.Red, gridCoord = ( 41, 7 ) }
    , { blocks = tBlocks, colour = BlockColour.Orange, gridCoord = ( 46, 7 ) }
    , { blocks = rBlocks, colour = BlockColour.Yellow, gridCoord = ( 52, 7 ) }
    , { blocks = iBlocks, colour = BlockColour.Purple, gridCoord = ( 57, 7 ) }
    , { blocks = sBlocks, colour = BlockColour.Green, gridCoord = ( 59, 7 ) }
    ]


tBlocks : List Coord
tBlocks =
    [ ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 4, 6 ), ( 2, 6 ), ( 2, 5 ), ( 2, 4 ), ( 2, 3 ), ( 2, 2 ), ( 2, 1 ), ( 2, 0 ) ]


eBlocks : List Coord
eBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 1, 3 ), ( 2, 3 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]


rBlocks : List Coord
rBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 5 ), ( 3, 4 ), ( 2, 3 ), ( 1, 3 ), ( 1, 2 ), ( 2, 1 ), ( 3, 0 ) ]


iBlocks : List Coord
iBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ) ]


sBlocks : List Coord
sBlocks =
    [ ( 0, 1 ), ( 1, 0 ), ( 2, 0 ), ( 3, 1 ), ( 3, 2 ), ( 2, 3 ), ( 1, 3 ), ( 0, 4 ), ( 0, 5 ), ( 1, 6 ), ( 2, 6 ), ( 3, 5 ) ]


lettersToBoardBlocks : List DroppingLetter -> List ( Coord, BlockColour )
lettersToBoardBlocks =
    List.map letterToBoardBlocks >> List.concat


letterToBoardBlocks : DroppingLetter -> List ( Coord, BlockColour )
letterToBoardBlocks { blocks, colour, gridCoord } =
    let
        ( gridX, gridY ) =
            gridCoord
    in
    -- TODO: this is v similar to Game.calcShapeBlocksBoardCoords - poss create shared module for stuff related to drawing blocks on the board?
    blocks
        |> List.map (\( x, y ) -> ( x + gridX, y + gridY ))
        |> List.map (\coord -> ( coord, colour ))
