module UIHelpers exposing (edges, mainBackgroundColour, mainForegroundColour, showModal)

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


{-| The colour to use by default for the foreground (fonts, buttons, etc)
-}
mainForegroundColour : Element.Color
mainForegroundColour =
    Element.rgb255 198 195 195


showModal : { onSubmit : msg, onCancel : msg, custom : List ( String, msg ) } -> Element msg -> Element msg
showModal { onSubmit, onCancel, custom } contents =
    let
        extraButtons =
            custom |> List.map (\( caption, onPress ) -> modalButton caption onPress)
    in
    Element.column
        [ Element.Background.color mainBackgroundColour
        , Element.Border.color mainForegroundColour
        , Element.Border.width 2
        , Element.centerX
        , Element.centerY
        , Element.padding 10
        , Element.spacingXY 0 20
        , Element.Border.rounded 10
        ]
        [ contents
        , Element.row [ Element.centerX, Element.spacingXY 10 0 ] <|
            extraButtons
                ++ [ modalButton "Save" onSubmit
                   , modalButton "Cancel" onCancel
                   ]
        ]
        |> modalMask


modalButton : String -> msg -> Element msg
modalButton caption onPress =
    Element.Input.button
        [ Element.Background.color <| Element.rgb255 180 180 180 --mainBackgroundColour
        , Element.Font.color mainBackgroundColour -- mainForegroundColour

        -- , Element.Border.color mainForegroundColour
        , Element.Border.width 1
        , Element.Border.rounded 5
        , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 1 ]
        , Element.focused []
        , Element.Font.size 15
        , Element.Font.semiBold
        ]
        { onPress = Just onPress
        , label = Element.el [ Element.paddingXY 5 3 ] (Element.text caption)
        }


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
