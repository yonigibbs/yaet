module Scoring exposing (Scoring, getLevel, getLines, getPoints, init, plusRemovedLines)


type Scoring
    = Scoring ScoringData


type alias ScoringData =
    { points : Int, level : Int, lines : Int }


init : Scoring
init =
    Scoring { points = 0, level = 1, lines = 0 }


plusRemovedLines : Int -> Scoring -> Scoring
plusRemovedLines lines scoring =
    scoring
        |> plusPoints (removedLinePoints lines scoring)
        |> plusLines lines
        |> calcLevel


getPoints : Scoring -> Int
getPoints (Scoring { points }) =
    points


getLevel : Scoring -> Int
getLevel (Scoring { level }) =
    level


getLines : Scoring -> Int
getLines (Scoring { lines }) =
    lines


plusPoints : Int -> Scoring -> Scoring
plusPoints points (Scoring scoringData) =
    Scoring { scoringData | points = points + scoringData.points }


plusLines : Int -> Scoring -> Scoring
plusLines lines (Scoring scoringData) =
    Scoring { scoringData | lines = lines + scoringData.lines }


calcLevel : Scoring -> Scoring
calcLevel (Scoring scoringData) =
    let
        level =
            scoringData.lines
                + 1
                |> toFloat
                |> (\lines -> lines / 10)
                |> ceiling
    in
    Scoring { scoringData | level = level }


removedLinePoints : Int -> Scoring -> Int
removedLinePoints lines (Scoring { level }) =
    removedLineMultiplier lines * level


removedLineMultiplier : Int -> Int
removedLineMultiplier lines =
    case lines of
        1 ->
            40

        2 ->
            100

        3 ->
            300

        _ ->
            1200
