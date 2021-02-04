module Button exposing (State(..), Style(..), button)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import UIHelpers


type Style
    = MainScreen
    | ModalDialog


type State msg
    = Inaccessible
    | Enabled msg
    | Disabled


type alias Config msg =
    { style : Style, caption : String, state : State msg }


type FontWeight
    = SemiBold
    | Medium


button : Config msg -> Element msg
button ({ style, caption, state } as config) =
    Element.el (commonAttrs state style ++ stateBasedAttrs state style) <| buttonElement config


buttonElement : Config msg -> Element msg
buttonElement { style, caption, state } =
    let
        label =
            Element.el (labelAttrs style) <| Element.text caption
    in
    case state of
        Enabled msg ->
            Element.Input.button [] { onPress = Just msg, label = label }

        _ ->
            label


labelAttrs : Style -> List (Element.Attribute msg)
labelAttrs style =
    case style of
        MainScreen ->
            [ Element.paddingEach { top = 5, right = 7, bottom = 7, left = 7 } ]

        ModalDialog ->
            [ Element.paddingXY 5 3 ]


commonAttrs : State msg -> Style -> List (Element.Attribute msg)
commonAttrs state style =
    case style of
        MainScreen ->
            [ Element.Font.color UIHelpers.mainForegroundColour
            , Element.Border.color UIHelpers.mainForegroundColour
            , Element.Border.width 2
            , Element.Border.rounded 20
            , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]

            --, Element.focused [ Element.Border.rounded 1 ]
            ]

        ModalDialog ->
            [ Element.Font.color UIHelpers.mainBackgroundColour
            , Element.Border.width 1
            , Element.Border.rounded 8
            , Element.Font.size 14
            , Element.Font.semiBold
            , Element.paddingXY 2 1
            ]


stateBasedAttrs : State msg -> Style -> List (Element.Attribute msg)
stateBasedAttrs state style =
    case ( style, state ) of
        ( MainScreen, Inaccessible ) ->
            [ Element.Background.color UIHelpers.mainBackgroundColour ]

        ( MainScreen, Enabled _ ) ->
            [ Element.Background.color UIHelpers.mainBackgroundColour
            , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]
            ]

        ( MainScreen, Disabled ) ->
            [ Element.Background.color <| withOpacity 0.3 UIHelpers.mainBackgroundColour ]

        ( ModalDialog, Inaccessible ) ->
            [ Element.Background.color UIHelpers.invertedButtonColour ]

        ( ModalDialog, Enabled _ ) ->
            [ Element.Background.color UIHelpers.invertedButtonColour
            , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 1 ]
            ]

        ( ModalDialog, Disabled ) ->
            [ Element.Background.color <| withOpacity 0.3 UIHelpers.invertedButtonColour ]


withOpacity : Float -> Element.Color -> Element.Color
withOpacity opacity colour =
    colour |> Element.toRgb |> (\rgb -> { rgb | alpha = opacity }) |> Element.fromRgb
