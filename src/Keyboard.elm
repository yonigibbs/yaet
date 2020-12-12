module Keyboard exposing (Action(..), Config, buildConfig, decoder)

import Dict exposing (Dict)
import Json.Decode as JD


{-| All actions invoked by the keyboard, for which we have special handling.

TODO: make keyboard bindings configurable.
TODO: these action types will eventually be needed on buttons, so not really keyboard specific. Put in separate module somewhere?

-}
type Action
    = MoveLeft
    | MoveRight
    | DropOneRow
    | RotateClockwise
    | RotateAnticlockwise


type Config
    = Config (Dict String Action)


buildConfig :
    { moveLeft : String
    , moveRight : String
    , dropOneRow : String
    , rotateClockwise : String
    , rotateAnticlockwise : String
    }
    -> Config
buildConfig { moveLeft, moveRight, dropOneRow, rotateClockwise, rotateAnticlockwise } =
    Config <|
        Dict.fromList
            [ ( moveLeft, MoveLeft )
            , ( moveRight, MoveRight )
            , ( dropOneRow, DropOneRow )
            , ( rotateClockwise, RotateClockwise )
            , ( rotateAnticlockwise, RotateAnticlockwise )
            ]


{-| Decodes a key event, succeeding if it's one of the special keys we handle (see `Key` type), otherwise failing.
-}
decoder : Config -> JD.Decoder Action
decoder (Config config) =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case Dict.get key config of
                    Just action ->
                        JD.succeed action

                    Nothing ->
                        JD.fail "Not a mapped key - ignoring"
            )
