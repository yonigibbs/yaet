module Modal exposing (CloseButton(..), Config, CustomButton, SubmitButton(..), defaultConfig, dialog, subscriptions, withCustomButton)

import Browser.Events
import Button
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Json.Decode as JD
import UIHelpers


type CloseButton msg
    = Cancel { onPress : msg }
    | Close { onPress : msg }


type SubmitButton msg
    = None
    | Save { onPress : Maybe msg }


type alias CustomButton msg =
    { caption : String, onPress : Maybe msg }


type alias Config msg =
    { closeButton : CloseButton msg, submitButton : SubmitButton msg, customButtons : List (CustomButton msg) }


defaultConfig : msg -> Maybe msg -> Config msg
defaultConfig onCancel onSave =
    { closeButton = Cancel { onPress = onCancel }, submitButton = Save { onPress = onSave }, customButtons = [] }


withCustomButton : String -> Maybe msg -> Config msg -> Config msg
withCustomButton caption onPress config =
    { config | customButtons = config.customButtons ++ [ { caption = caption, onPress = onPress } ] }


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



--let
--    label =
--        Element.el [ Element.paddingXY 5 3 ] (Element.text caption)
--
--    ( enablednessAttrs, contents ) =
--        case onPress of
--            Just _ ->
--                ( [ Element.Background.color <| Element.rgba255 180 180 180 1
--                  , Element.mouseOver [ Element.Border.glow (Element.rgb255 198 195 195) 1 ]
--                  ]
--                , Element.Input.button [] { onPress = onPress, label = label }
--                )
--
--            Nothing ->
--                ( [ Element.Background.color <| Element.rgba255 180 180 180 0.3 ]
--                , label
--                )
--in
--Element.el
--    ([ Element.Font.color UIHelpers.mainBackgroundColour
--     , Element.Border.width 1
--     , Element.Border.rounded 8
--     , Element.Font.size 14
--     , Element.Font.semiBold
--     , Element.paddingXY 2 1
--     ]
--        ++ enablednessAttrs
--    )
--    contents


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


subscriptions : Config msg -> Sub msg
subscriptions { closeButton, submitButton } =
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
