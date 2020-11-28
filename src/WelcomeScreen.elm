module WelcomeScreen exposing (view)

import BoardView
import Element exposing (Element)
import UIHelpers


view : msg -> Element msg
view startGameMsg =
    Element.column [ Element.spacingXY 0 25 ]
        [ BoardView.view boardViewConfig [] Nothing
        , Element.row [ Element.centerX ] [ UIHelpers.button "Start Game" startGameMsg ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15, rowCount = 20, colCount = 100, borderStyle = BoardView.Fade UIHelpers.mainBackgroundColour }
