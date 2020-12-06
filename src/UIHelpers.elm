module UIHelpers exposing (buttonBorderColor, edges, mainBackgroundColour)

{-| Miscellaneous helper functions related to the UI/rendering.
-}

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


{-| The colour to put on buttons.
-}
buttonBorderColor : Element.Color
buttonBorderColor =
    Element.rgb255 198 195 195
