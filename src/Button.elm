module Button exposing (Config, State(..), Style(..), button)

{-| This module exposes functionality for showing buttons on the UI in a consistent way. elm-ui doesn't provide a built-in
way to disable controls (without resorting to the HTML "escape-hatch") so instead this module provides buttons which,
when disabled (or inaccessible, e.g. because they're under a modal dialog overlay) renders them not as buttons but as
simple divs. This prevents "disabled" buttons having focus. It also removes any styling that should be inapplicable to
such buttons, e.g. glow effects on hover.
-}

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import UIHelpers


{-| Defines the different styles of button shown in this game:

  - `MainScreen`: A button shown in one of the main screens (currently only the Welcome screen). Rounded, black
    background with grey text.
  - `ModalDialog`: A button shown on a modal dialog (e.g. the Save and Cancel buttons).

-}
type Style
    = MainScreen
    | ModalDialog


{-| Describes the state of a button:

  - `Enabled`: The button is enabled and in a normal state. It can have effects like glowing when hovered over, etc. The
    data associated with this variant is the message to invoke when the button is clicked.
  - `Disabled`: The button is explicitly disabled, e.g. a modal dialog in an invalid state which has a Submit button
    which the user cannot click yet. This will have a lower opacity than an eanbled button, has no hover effects, and
    cannot be clicked by the user.
  - `Inaccessible`: The button isn't disabled, but cannot be clicked by the user for another reason, e.g. because there's
    a modal dialog overlaid above it. It looks the same as an enabled button, but cannot be clicked or have focus, and
    has no hover effects.

-}
type State msg
    = Enabled msg
    | Disabled
    | Inaccessible


{-| The information required to configure a button so it can be rendered on the screen.
-}
type alias Config msg =
    { style : Style, caption : String, state : State msg }


{-| Returns an elm-ui `Element` representing a button configured in the given way.
-}
button : Config msg -> Element msg
button ({ style, caption, state } as config) =
    Element.el (commonAttrs style ++ stateBasedAttrs state style) <| buttonElement config


{-| The element representing the button itself. Typically an actual elm-ui `Element.Input.button`, unless the button isn't
enabled: in such cases a normal element is used instead (so it can't have focus or be clicked).
-}
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


{-| Attributes to apply to the label of the button.
-}
labelAttrs : Style -> List (Element.Attribute msg)
labelAttrs style =
    case style of
        MainScreen ->
            [ Element.paddingEach { top = 5, right = 7, bottom = 7, left = 7 } ]

        ModalDialog ->
            [ Element.paddingXY 5 3 ]


{-| Attributes to apply to the button regardless of its state.
-}
commonAttrs : Style -> List (Element.Attribute msg)
commonAttrs style =
    case style of
        MainScreen ->
            [ Element.Font.color UIHelpers.mainForegroundColour
            , Element.Border.color UIHelpers.mainForegroundColour
            , Element.Border.width 2
            , Element.Border.rounded 20
            , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 2 ]
            ]

        ModalDialog ->
            [ Element.Font.color UIHelpers.mainBackgroundColour
            , Element.Border.width 1
            , Element.Border.rounded 8
            , Element.Font.size 14
            , Element.Font.semiBold
            , Element.paddingXY 2 1
            ]


{-| Attributes to apply to the button, which depend on the state.
-}
stateBasedAttrs : State msg -> Style -> List (Element.Attribute msg)
stateBasedAttrs state style =
    case ( style, state ) of
        ( MainScreen, Enabled _ ) ->
            [ Element.Background.color UIHelpers.mainBackgroundColour
            , Element.mouseOver [ Element.Border.glow mainScreenButtonGlowColour 2 ]
            ]

        ( MainScreen, Inaccessible ) ->
            [ Element.Background.color UIHelpers.mainBackgroundColour ]

        ( MainScreen, Disabled ) ->
            [ Element.Background.color <| withOpacity 0.3 UIHelpers.mainBackgroundColour ]

        ( ModalDialog, Enabled _ ) ->
            [ Element.Background.color modalButtonColour
            , Element.mouseOver [ Element.Border.glow mainScreenButtonGlowColour 1 ]
            ]

        ( ModalDialog, Inaccessible ) ->
            [ Element.Background.color modalButtonColour ]

        ( ModalDialog, Disabled ) ->
            [ Element.Background.color <| withOpacity 0.3 modalButtonColour ]


{-| Returns a copy of the supplied colour, with the given opacity.
-}
withOpacity : Float -> Element.Color -> Element.Color
withOpacity opacity colour =
    colour |> Element.toRgb |> (\rgb -> { rgb | alpha = opacity }) |> Element.fromRgb


{-| The background colour of a button on a modal dialog (grey).
-}
modalButtonColour : Element.Color
modalButtonColour =
    Element.rgb255 180 180 180


{-| The colour which a button on the main screen glows when hovered over.
-}
mainScreenButtonGlowColour : Element.Color
mainScreenButtonGlowColour =
    Element.rgb255 198 195 195
