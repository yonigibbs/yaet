module Modal exposing (CloseButton(..), Config, CustomButton, SubmitButton(..), defaultConfig, dialog, subscriptions, withCustomButton)

{-| This module provides the ability to show modal dialogs overlaid over the page.
-}

import Browser.Events
import Button
import Element exposing (Element)
import Element.Background
import Element.Border
import Json.Decode as JD
import UIHelpers


{-| Defines the type of button used to dismiss the dialog. Currently the only difference between the two variants is the
caption they result in. In future more differences in behaviour could be added (e.g. Cancel could prompt the user to see
if they want to save their changes or not).
-}
type CloseButton msg
    = Cancel { onPress : msg }
    | Close { onPress : msg }


{-| Defines the button the user should have to "submit" (i.e. save) the dialog, if applicable:

  - `None`: No such button should be shown. Typically used on dialogs that only show data, rather than let the user edit
    some data.
  - `Save`: A button with a "Save" caption should be shown. The `onPress` value defines the message to invoke when this
    button is pressed. If the value is `Nothing` then the button is "disabled" (see `Button` module for more info on this).

-}
type SubmitButton msg
    = None
    | Save { onPress : Maybe msg }


{-| Defines a custom button to show, along with the Close/Cancel/Submit button.
-}
type alias CustomButton msg =
    { caption : String, onPress : Maybe msg }


{-| Defines the information required to render a modal dialog, e.g. whether to show a Submit button, any custom buttons
to show, etc.
-}
type alias Config msg =
    { closeButton : CloseButton msg, submitButton : SubmitButton msg, customButtons : List (CustomButton msg) }


{-| The default configuration for modal dialogs. Defines that they should hvae a Cancel button with the given message,
and a Save button with the given message (which might be Nothing if the button is currently disabled). Has no custom
buttons. This can be used as a starting point for a config which can then be amended using some of the builder functions
such as `withCustomButton`.
-}
defaultConfig : msg -> Maybe msg -> Config msg
defaultConfig onCancel onSave =
    { closeButton = Cancel { onPress = onCancel }, submitButton = Save { onPress = onSave }, customButtons = [] }


{-| Adds a custom button with the given details to the supplied config.
-}
withCustomButton : String -> Maybe msg -> Config msg -> Config msg
withCustomButton caption onPress config =
    { config | customButtons = config.customButtons ++ [ { caption = caption, onPress = onPress } ] }


{-| Renders a modal dialog based on the given config, and inside it puts the given `contents`.
-}
dialog : Config msg -> Element msg -> Element msg
dialog { closeButton, submitButton, customButtons } contents =
    let
        customButtonElements =
            customButtons |> List.map (\{ caption, onPress } -> modalButton caption onPress)

        submitButtonElement =
            case submitButton of
                None ->
                    []

                Save { onPress } ->
                    [ modalButton "OK" onPress ]

        cancelButtonElement =
            case closeButton of
                Cancel { onPress } ->
                    [ modalButton "Cancel" (Just onPress) ]

                Close { onPress } ->
                    [ modalButton "Close" (Just onPress) ]
    in
    Element.column
        [ Element.Background.color UIHelpers.mainBackgroundColour
        , Element.Border.color UIHelpers.mainForegroundColour
        , Element.Border.width 2
        , Element.centerX
        , Element.centerY
        , Element.padding 10
        , Element.spacingXY 0 20
        , Element.Border.rounded 10
        ]
        [ contents
        , Element.row [ Element.spacingXY 10 0, Element.centerX ] <|
            List.concat [ customButtonElements, submitButtonElement, cancelButtonElement ]
        ]
        |> modalMask


modalButton : String -> Maybe msg -> Element msg
modalButton caption onPress =
    let
        buttonState =
            case onPress of
                Just msg ->
                    Button.Enabled msg

                Nothing ->
                    Button.Disabled
    in
    Button.button { style = Button.ModalDialog, caption = caption, state = buttonState }


{-| The full-screen semi-opaque "mask" overlaid on the page, on top of which the actual modal dialog is shown.
-}
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


{-| The subscriptions to use for the modal dialog. Handles the user pressing the Enter or Escape keys to submit or cancel
a modal dialog.
-}
subscriptions : Config msg -> Sub msg
subscriptions { closeButton, submitButton } =
    -- TODO: there's a known issue that when the user presses the Enter key when a button has focus this can result in
    -- confusing behaviour.
    let
        onClose =
            case closeButton of
                Cancel { onPress } ->
                    onPress

                Close { onPress } ->
                    onPress

        onSubmit =
            case submitButton of
                None ->
                    Nothing

                Save { onPress } ->
                    onPress
    in
    escapeAndEnterKeyDecoder onClose onSubmit |> Browser.Events.onKeyDown


escapeAndEnterKeyDecoder : msg -> Maybe msg -> JD.Decoder msg
escapeAndEnterKeyDecoder onClose onSubmit =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case ( key, onSubmit ) of
                    ( "Escape", _ ) ->
                        JD.succeed onClose

                    ( "Enter", Just submit ) ->
                        JD.succeed submit

                    _ ->
                        JD.fail ""
            )
