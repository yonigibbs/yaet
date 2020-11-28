module WelcomeScreen exposing (view)

import BoardView
import Color exposing (Color)
import ColourUtils
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input


view : Color -> msg -> Element msg
view backgroundColour startGameMsg =
    Element.column []
        [ BoardView.view (boardViewConfig backgroundColour) [] Nothing |> Element.html
        , Element.row [ Element.centerX ]
            [ Element.Input.button
                [ Element.Background.color <| ColourUtils.colourToElmUIColour backgroundColour
                , Element.Font.color <| Element.rgb255 198 195 195
                , Element.Border.color <| Element.rgb255 198 195 195
                , Element.Border.width 2
                , Element.Border.rounded 20
                , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]
                ]
                { onPress = Just startGameMsg
                , label = Element.row [ Element.paddingEach { top = 5, right = 7, bottom = 7, left = 7 } ] [ Element.text "Start Game" ]
                }
            ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : Color -> BoardView.Config
boardViewConfig backgroundColour =
    { cellSize = 15, rowCount = 20, colCount = 100, borderStyle = BoardView.Fade backgroundColour }
