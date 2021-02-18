port module Ports exposing (persistHighScores, persistSettings)

{-| This module defines all ports used by the system.
-}

import Json.Encode as JE


{-| Persists settings to local storage.
-}
port persistSettings : JE.Value -> Cmd msg


{-| Persists high scores to local storage.
-}
port persistHighScores : JE.Value -> Cmd msg
