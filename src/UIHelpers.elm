module UIHelpers exposing (button, buttonBorderColor, edges, mainBackgroundColour)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


{-| The main background colour of the screen.
-}
mainBackgroundColour : Element.Color
mainBackgroundColour =
    Element.rgb255 30 30 30


buttonBorderColor : Element.Color
buttonBorderColor =
    Element.rgb255 198 195 195



-- TODO: currently only used in welcome screen - poss don't need in this module? Poss don't need this module at all?


button : String -> msg -> Element msg
button caption msg =
    Element.Input.button
        [ Element.Background.color mainBackgroundColour
        , Element.Font.color buttonBorderColor
        , Element.Border.color buttonBorderColor
        , Element.Border.width 2
        , Element.Border.rounded 20
        , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]
        ]
        { onPress = Just msg
        , label = Element.row [ Element.paddingEach { top = 5, right = 7, bottom = 7, left = 7 } ] [ Element.text caption ]
        }
