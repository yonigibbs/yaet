module UIHelpers exposing (edges, mainBackgroundColour, mainForegroundColour)

{-| Miscellaneous helper functions related to the UI/rendering.
-}

import Element exposing (Element)
import Element.Border


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


{-| The colour to use by default for the foreground (fonts, buttons, etc)
-}
mainForegroundColour : Element.Color
mainForegroundColour =
    Element.rgb255 198 195 195
