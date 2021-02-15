module Scoring exposing (Scoring, getLevel, getLines, getPoints, init, plusRemovedLines)

{-| This module contains all functionality related to the scoring of the game, namely the number of points scored so far,
the level, and the number of cleared lines.
-}


{-| The main type exposed (as an opaque type) from this module.
-}
type Scoring
    = Scoring ScoringData


{-| Record containing the three values related to scoring (points, level and lines).
-}
type alias ScoringData =
    { points : Int, level : Int, lines : Int }


{-| Gets the initial `Scoring` value to assign to a new game. This is 0 points and 0 lines, and level 1.
-}
init : Scoring
init =
    Scoring { points = 0, level = 1, lines = 0 }


{-| Increments the score based on the number of lines that have just been removed.
-}
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


{-| Calculates the level the score should be at. Every 10 cleared lines the level increments, starting initially at level 1.
-}
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


{-| Calculates the number of points to award for the supplied number of removed lines.
-}
removedLinePoints : Int -> Scoring -> Int
removedLinePoints lines (Scoring { level }) =
    removedLineMultiplier lines * level


{-| Gets the value to multiply by the number of removed lines: the more lines are cleared in one more the higher the
multiplier.
-}
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
