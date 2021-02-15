module HighScores exposing (EditableHighScores, Entry, HighScores, fromJson, map, maxItems, setName, withPossibleNewHighScore)

-- TODO: document this when all implemented

import Json.Encode as JE


type HighScores
    = HighScores (List Entry)


type alias Entry =
    { name : String, score : Int }


fromJson : JE.Value -> HighScores
fromJson json =
    -- TODO: read JSON from local storage and decode here
    fromEntries [ { name = "Number 1", score = 30 }, { name = "Number 2", score = 20 }, { name = "Number 3", score = 10 } ]


fromEntries : List Entry -> HighScores
fromEntries entries =
    entries |> List.sortBy .score |> HighScores


maxItems : Int
maxItems =
    5


withPossibleNewHighScore : Int -> HighScores -> Maybe EditableHighScores
withPossibleNewHighScore score (HighScores entries) =
    let
        ( above, below ) =
            entries |> List.partition (\entry -> entry.score >= score)

        aboveCount =
            List.length above
    in
    if aboveCount > maxItems then
        Nothing

    else
        Just <|
            EditableHighScores
                { above = above
                , new = { name = "", score = score }
                , below = List.take (maxItems - aboveCount - 1) below
                }


type EditableHighScores
    = EditableHighScores { above : List Entry, new : Entry, below : List Entry }


map : (Entry -> a) -> (Entry -> a) -> EditableHighScores -> List a
map existingEntriesMapper newEntryMapper (EditableHighScores { above, new, below }) =
    List.concat
        [ List.map existingEntriesMapper above
        , [ newEntryMapper new ]
        , List.map existingEntriesMapper below
        ]


setName : String -> EditableHighScores -> EditableHighScores
setName newName (EditableHighScores data) =
    EditableHighScores { data | new = data.new |> withName newName }


withName : String -> Entry -> Entry
withName name entry =
    { entry | name = String.left 10 <| name }
