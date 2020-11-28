module ColourUtils exposing (colourToElmUIColour)

import Color exposing (Color)
import Element


colourToElmUIColour : Color -> Element.Color
colourToElmUIColour colour =
    let
        { red, green, blue, alpha } =
            Color.toRgba colour
    in
    Element.rgba red green blue alpha
