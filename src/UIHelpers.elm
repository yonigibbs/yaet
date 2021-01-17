module UIHelpers exposing (edges, mainBackgroundColour, mainForegroundColour, showModal)

{-| Miscellaneous helper functions related to the UI/rendering.
-}

import Element exposing (Element)
import Element.Background
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


showModal : Element msg -> Element msg
showModal contents =
    Element.el
        [ Element.Background.color mainBackgroundColour
        , Element.Border.color mainForegroundColour
        , Element.Border.width 2
        , Element.centerX
        , Element.centerY
        , Element.padding 10
        , Element.Border.rounded 10
        ]
        contents
        |> modalMask


modalMask : Element msg -> Element msg
modalMask contents =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color <| Element.rgba255 20 20 20 0.7
        , Element.Border.width 0
        , Element.inFront contents
        ]
        Element.none
