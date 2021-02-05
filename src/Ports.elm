port module Ports exposing (persistSettings)

import Json.Encode as JE


port persistSettings : JE.Value -> Cmd msg
