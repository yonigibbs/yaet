module WelcomeScreen exposing (view)

import BoardView
import Element exposing (Element)
import Html.Attributes


view : Element msg
view =
    Element.column [ Element.centerX ]
        [ Element.text "Hello"
        , Element.column
            [ -- TODO: assuming background colour here - parameterise or put in utils module or something.
              --Element.Border.shadow
              --    { offset = ( 50, 50 ), size = 25, blur = 25, color = Element.rgb255 100 100 100 }
              Element.htmlAttribute (Html.Attributes.id "xxx")
            , Element.htmlAttribute
                (Html.Attributes.style "box-shadow" "125px 125px 50px 0 white inset, -25px -25px 50px 0 white inset")

            --Element.Border.glow (Element.rgb255 100 100 100) 100
            ]
            [ BoardView.view boardViewConfig [] Nothing |> Element.html ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15, rowCount = 20, colCount = 50 }
