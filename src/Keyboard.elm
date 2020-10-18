module Keyboard exposing (Key(..), KeyMessages, keyEventDecoder)

import Json.Decode as JD


{-| All keys for which we have special handling.

TODO: make keyboard bindinds configurable.

-}
type Key
    = LeftArrow
    | RightArrow
    | DownArrow
    | Z
    | X
    | Other


type alias KeyMessages msg =
    { moveLeft : msg
    , moveRight : msg
    , dropOneRow : msg
    , rotateClockwise : msg
    , rotateAnticlockwise : msg
    }


keyEventDecoder : KeyMessages msg -> JD.Decoder msg
keyEventDecoder messages =
    keyDecoder
        |> JD.andThen
            (\key ->
                case key of
                    LeftArrow ->
                        JD.succeed messages.moveLeft

                    RightArrow ->
                        JD.succeed messages.moveRight

                    DownArrow ->
                        JD.succeed messages.dropOneRow

                    Z ->
                        JD.succeed messages.rotateAnticlockwise

                    X ->
                        JD.succeed messages.rotateClockwise

                    Other ->
                        JD.fail "Not a mapped key - ignoring"
            )


keyDecoder : JD.Decoder Key
keyDecoder =
    JD.field "key" JD.string |> JD.map toKey


toKey : String -> Key
toKey keyString =
    let
        _ =
            Debug.log "Key" keyString
    in
    case keyString of
        "ArrowLeft" ->
            LeftArrow

        "ArrowRight" ->
            RightArrow

        "ArrowDown" ->
            DownArrow

        "z" ->
            Z

        "x" ->
            X

        _ ->
            Other