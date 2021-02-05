module HighScores exposing (HighScores, fromJson)

-- TODO: document this when all implemented

import Json.Encode as JE


type HighScores
    = HighScores


fromJson : JE.Value -> HighScores
fromJson json =
    -- TODO: read JSON from local storage and decode here
    HighScores
