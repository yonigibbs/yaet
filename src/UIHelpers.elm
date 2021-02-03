module UIHelpers exposing (edges, hijackOn, mainBackgroundColour, mainForegroundColour, showModal)

{-| Miscellaneous helper functions related to the UI/rendering.
-}

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Events
import Json.Decode as JD


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


showModal : { onSubmit : Maybe msg, onCancel : msg, custom : List ( String, msg ) } -> Element msg -> Element msg
showModal { onSubmit, onCancel, custom } contents =
    let
        extraButtons =
            custom |> List.map (\( caption, onPress ) -> modalButton caption (Just onPress))
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
        , Element.row [ Element.spacingXY 10 0, Element.centerX ] <|
            extraButtons
                ++ [ modalButton "OK" onSubmit
                   , modalButton "Cancel" (Just onCancel)
                   ]
        ]
        |> modalMask


modalButton : String -> Maybe msg -> Element msg
modalButton caption onPress =
    let
        label =
            Element.el [ Element.paddingXY 5 3 ] (Element.text caption)

        ( enablednessAttrs, contents ) =
            case onPress of
                Just _ ->
                    ( [ Element.Background.color <| Element.rgba255 180 180 180 1
                      , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 1 ]
                      ]
                    , Element.Input.button [ Element.focused [] ] { onPress = onPress, label = label }
                    )

                Nothing ->
                    ( [ Element.Background.color <| Element.rgba255 180 180 180 0.3 ]
                    , label
                    )
    in
    Element.el
        ([ Element.Font.color mainBackgroundColour
         , Element.Border.width 1
         , Element.Border.rounded 8
         , Element.Font.size 14
         , Element.Font.semiBold
         , Element.paddingXY 2 1
         ]
            ++ enablednessAttrs
        )
        contents


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


{-| Hijacks an event by hooking into it and "preventing default" on it.
-}
hijackOn : String -> JD.Decoder msg -> Html.Attribute msg
hijackOn event decoder =
    Html.Events.preventDefaultOn event (JD.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
