module HighScores exposing (HighScores, fromJson)

import Json.Encode as JE


type HighScores
    = HighScores


fromJson : JE.Value -> HighScores
fromJson json =
    -- TODO: read JSON from local storage and decode here
    HighScores
