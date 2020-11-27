module WelcomeScreen exposing (view)

import BoardView
import Color exposing (Color)
import Element exposing (Element)
import Element.Background
import Element.Input


view : Color -> msg -> Element msg
view backgroundColour startGameMsg =
    Element.column []
        [ BoardView.view (boardViewConfig backgroundColour) [] Nothing |> Element.html
        , Element.row [ Element.centerX ]
            [ Element.Input.button
                [ Element.Background.color <| Element.rgb 238 238 238
                ]
                { onPress = Just startGameMsg, label = Element.text "Start Game" }
            ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : Color -> BoardView.Config
boardViewConfig backgroundColour =
    { cellSize = 15, rowCount = 20, colCount = 100, borderStyle = BoardView.Fade backgroundColour }
