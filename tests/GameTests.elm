module GameTests exposing (suite)

{-| Tests for the Game module. Game is an opaque type so we can't construct it ourselves here, pre-populated in some
way. Instead, we start off with a new game (supplying a known set of shapes rather than random ones), then progress the
game by simulating user and timer actions.
-}

import Coord exposing (Coord)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Game exposing (Game)
import GameBoard
import Shape exposing (Shape)
import ShapeUtils
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Game"
        [ test "Drops initial shape two rows after two timer drop events." <|
            \_ ->
                newGame defaultInitialGameState
                    |> repeat 2 timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame BottomPadding """
                                ----------
                                ----------
                                -----o----
                                ---ooo----
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Highlights landed shape after moving initial shape to left edge then letting it drop to bottom." <|
            \_ ->
                newGame defaultInitialGameState
                    |> repeat 3 (executeUserActions [ Game.Move Game.Left ])
                    |> repeat 18 timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame TopPadding """
                                --O-------
                                OOO-------
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Next shape appears after shape drops to bottom (and bottom shape no longer highlighted)." <|
            \_ ->
                newGame defaultInitialGameState
                    |> repeat 3 (executeUserActions [ Game.Move Game.Left ])
                    |> repeat 19 timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame NoPadding """
                                ----yy----
                                ----yy----
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                --o-------
                                ooo-------
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Completed row highlighted." <|
            \_ ->
                newGame defaultInitialGameState
                    -- Move blue L to the left (will first first three columns)
                    |> repeat 3 (executeUserActions [ Game.Move Game.Left ])
                    -- Drop it the 18 rows required to get it to the bottom, then timer drop to make next shape appear.
                    |> repeat 18 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    -- Red square has now appeared: move it left one, then drop it to fill columns 4 and 5.
                    |> progressGame (executeUserActions [ Game.Move Game.Left ])
                    |> repeat 18 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    -- Yellow line has now appeared: move it right two then drop it to fill columns 6-9 (intersperse
                    -- some timer drops before the user interactions).
                    |> repeat 2 timerDrop
                    |> progressGame (executeUserActions [ Game.Move Game.Right ])
                    |> progressGame timerDrop
                    |> progressGame (executeUserActions [ Game.Move Game.Right ])
                    |> repeat 15 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    -- Green half-plus has now appeared: it's on its back so rotate it once anti-clockwise then move it
                    -- all the way to the right before dropping it.
                    |> progressGame (executeUserActions [ Game.Rotate Shape.Anticlockwise ])
                    |> repeat 5 (executeUserActions [ Game.Move Game.Right ])
                    |> repeat 17 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame TopPadding """
                                ---------p
                                --oyy---pp
                                OOOYYCCCCP
                                """
                            , rowRemoval = RowBeingRemoved
                            }
                        )
        , test "New shape added after completed row removed." <|
            \_ ->
                newGame defaultInitialGameState
                    -- Repeat same steps as previous test, then update the game to make the completed row disappear, and
                    -- the next shape appear.
                    |> repeat 3 (executeUserActions [ Game.Move Game.Left ])
                    |> repeat 18 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    |> progressGame (executeUserActions [ Game.Move Game.Left ])
                    |> repeat 18 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    |> repeat 2 timerDrop
                    |> progressGame (executeUserActions [ Game.Move Game.Right ])
                    |> progressGame timerDrop
                    |> progressGame (executeUserActions [ Game.Move Game.Right ])
                    |> repeat 15 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    |> progressGame (executeUserActions [ Game.Rotate Shape.Anticlockwise ])
                    |> repeat 5 (executeUserActions [ Game.Move Game.Right ])
                    |> repeat 17 (executeUserActions [ Game.Move Game.Down ])
                    |> progressGame timerDrop
                    |> simulateRowRemovalAnimationComplete
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame TopPadding """
                                ---rr-----
                                ----rr----
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ---------p
                                --oyy---pp
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Does not move shape off board if user tries to move it too much." <|
            \_ ->
                newGame defaultInitialGameState
                    |> repeat 20 (executeUserActions [ Game.Move Game.Left ])
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame BottomPadding """
                                --o-------
                                ooo-------
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Moves L-shape back into play if rotation would cause it to move off the side." <|
            \_ ->
                -- Start off with the normal L-shape on its back
                newGame defaultInitialGameState
                    -- Rotate it once so it's the right way up
                    |> progressGame (executeUserActions [ Game.Rotate Shape.Clockwise ])
                    -- Move it all the way to the left
                    |> repeat 5 (executeUserActions [ Game.Move Game.Left ])
                    -- Rotate it clockwise again: naturally this will mean the shape is now off the board so this
                    -- wouldn't be allowed, but we have logic to "shift" it back into place (i.e. go back one cell to
                    -- the right).
                    |> progressGame (executeUserActions [ Game.Rotate Shape.Clockwise ])
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame BottomPadding """
                                ----------
                                ooo-------
                                o---------
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Moves line shape back into play if rotation would cause it to move off the side." <|
            \_ ->
                -- Same as previous test but with line: turn it so it's vertical then move it all the way (to the right
                -- this time), then rotate it.
                defaultInitialGameState
                    |> withInitialShape (ShapeUtils.getShape ShapeUtils.Line)
                    |> newGame
                    |> progressGame (executeUserActions [ Game.Rotate Shape.Clockwise ])
                    |> repeat 5 (executeUserActions [ Game.Move Game.Right ])
                    |> progressGame (executeUserActions [ Game.Rotate Shape.Clockwise ])
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame BottomPadding """
                                ----------
                                ----------
                                ------cccc
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        ]


{-| Describes whether, at any given point, the game is (or is expected to be) in a state where lines are being removed.
-}
type RowRemoval
    = NoRowRemoval
    | RowBeingRemoved


{-| A string representation of the blocks in the game (corresponding to what we get back from calling `Game.blocks`).
The string is a 10\*20 grid of characters where each cell is either a `-` for empty cells, or a character for occupied
cells, where that character represents the colour. See `occupiedCellChar` for more details.
-}
type AsciiGame
    = AsciiGame String


{-| Describes the expected state of a game at any given point.
-}
type ExpectedGame
    = ExpectedGame { game : AsciiGame, rowRemoval : RowRemoval }


{-| Contains the state of a game at a given point.
-}
type alias GameState =
    { game : Game (List Shape), rowRemoval : RowRemoval }


{-| Creates an expectation that the state of supplied game matches the supplied expected state.
-}
expectGame : ExpectedGame -> GameState -> Expectation
expectGame (ExpectedGame expected) actual =
    Expect.equal ( gameAsAscii actual.game, actual.rowRemoval ) ( expected.game, expected.rowRemoval )


{-| Gets a string representation of the the supplied game, so it can be compared to an expected game state.
-}
gameAsAscii : Game (List Shape) -> AsciiGame
gameAsAscii game =
    let
        { normal, highlighted } =
            Game.blocks game

        mapBlocks : Bool -> List ( Coord, Shape.BlockColour ) -> List ( Coord, ( Shape.BlockColour, Bool ) )
        mapBlocks isHighlighted =
            List.map (\( coord, colour ) -> ( coord, ( colour, isHighlighted ) ))

        allBlocks : Dict Coord ( Shape.BlockColour, Bool )
        allBlocks =
            List.concat [ mapBlocks False normal, mapBlocks True highlighted ]
                |> Dict.fromList

        cellAsString : Int -> Int -> String
        cellAsString x y =
            case Dict.get ( x, y ) allBlocks of
                Just ( colour, isHighlighted ) ->
                    occupiedCellChar colour isHighlighted

                Nothing ->
                    "-"

        row : Int -> String
        row y =
            List.range 0 (GameBoard.colCount - 1)
                |> List.map (\x -> cellAsString x y)
                |> String.concat
    in
    List.range 0 (GameBoard.rowCount - 1)
        |> List.map row
        |> List.reverse
        |> String.join "\n"
        |> AsciiGame


{-| Describes whether a string representation of a board should be padded at the bottom or the top. See `padAsciiBoard`
for more info.
-}
type AsciiBoardPadType
    = BottomPadding
    | TopPadding
    | NoPadding


{-| Builds an `AsciiGame` from the supplied string representation of it, where that string representation can miss the
bottom or top of the board out by any number of rows - they will be padded with empty rows. Allows the game state to be
represented in tests with fewer lines.
-}
buildAsciiGame : AsciiBoardPadType -> String -> AsciiGame
buildAsciiGame padType board =
    let
        suppliedLines : List String
        suppliedLines =
            String.trim board |> String.split "\n" |> List.map String.trim

        emptyRow : String
        emptyRow =
            List.repeat GameBoard.colCount "-" |> String.concat

        padding : List String
        padding =
            List.repeat
                (GameBoard.rowCount - List.length suppliedLines)
                emptyRow

        allLines =
            case padType of
                BottomPadding ->
                    suppliedLines ++ padding

                TopPadding ->
                    padding ++ suppliedLines

                NoPadding ->
                    suppliedLines
    in
    allLines
        |> String.join "\n"
        |> AsciiGame


{-| Starts a new game with the supplied initialisation info, and returns a `GameState` record ready to start the tests.
-}
newGame : List Shape -> GameState
newGame shapeBuffer =
    { game = Game.new getNextShape shapeBuffer, rowRemoval = NoRowRemoval }


getNextShape : List Shape -> ( Shape, List Shape )
getNextShape shapes =
    case shapes of
        [] ->
            Debug.todo "Not enough shapes in test shape buffer"

        first :: rest ->
            ( first, rest )


{-| The default initial state to use when creating a new game. The initial shapes allow a full row to be completed with
the first four shapes. The shapes in order are:

  - L (orange)
  - Square (yellow)
  - Line (cyan)
  - Half-plus (purple)
  - Z (red)
  - Z-mirror-image (green)
  - L-mirror-image (blue)

-}
defaultInitialGameState : List Shape
defaultInitialGameState =
    List.map ShapeUtils.getShape
        [ ShapeUtils.Ell
        , ShapeUtils.Square
        , ShapeUtils.Line
        , ShapeUtils.HalfPlus
        , ShapeUtils.Zed
        , ShapeUtils.ZedMirror
        , ShapeUtils.EllMirror
        ]


{-| Returns a copy of the supplied list, with the first entry replaced with the supplied shape.
-}
withInitialShape : Shape -> List Shape -> List Shape
withInitialShape initialShape shapes =
    case shapes of
        [] ->
            [ initialShape ]

        _ :: rest ->
            initialShape :: rest


timerDrop : Game (List Shape) -> Game.UpdateResult (List Shape)
timerDrop =
    Game.timerDrop getNextShape


executeUserActions : List Game.UserAction -> Game (List Shape) -> Game.UpdateResult (List Shape)
executeUserActions actions =
    Game.executeUserActions getNextShape actions


{-| Progresses the game by executing some function that updates the game (e.g. `Game.timerDrop` or `Game.rotateShape`).
This can either be simulating a user action like pressing an arrow, or simulating a timer event.
-}
progressGame : (Game (List Shape) -> Game.UpdateResult (List Shape)) -> GameState -> GameState
progressGame action state =
    let
        gameUpdateResult =
            action state.game
    in
    case gameUpdateResult of
        Game.NoChange ->
            state

        Game.Continue { game } ->
            { game = game, rowRemoval = NoRowRemoval }

        Game.RowBeingRemoved { game } ->
            { game = game, rowRemoval = RowBeingRemoved }

        Game.GameOver _ ->
            -- TODO: is this OK to do in tests?
            Debug.todo "Unexpected game over"

        Game.Paused _ ->
            state


{-| Executes the given action the given number of times, starting from the supplied state, and returning the state at the
end of all those actions.
-}
repeat : Int -> (Game (List Shape) -> Game.UpdateResult (List Shape)) -> GameState -> GameState
repeat count action state =
    List.range 1 count
        |> List.foldl (\_ state_ -> progressGame action state_) state


{-| Simulates the animation of a row removal having been completed, and progresses the game accordingly.
-}
simulateRowRemovalAnimationComplete : GameState -> GameState
simulateRowRemovalAnimationComplete { game } =
    { game = Game.onRowRemovalAnimationComplete game, rowRemoval = NoRowRemoval }


{-| Gets the character to use for a cell which is occupied, for the given colour and whether it's highlighted. The
character is the first letter of the colour in lower-case normally, but upper-cased for highlighted cells.
-}
occupiedCellChar : Shape.BlockColour -> Bool -> String
occupiedCellChar colour isHighlighted =
    let
        char =
            case colour of
                Shape.Blue ->
                    "b"

                Shape.Red ->
                    "r"

                Shape.Orange ->
                    "o"

                Shape.Yellow ->
                    "y"

                Shape.Purple ->
                    "p"

                Shape.Green ->
                    "g"

                Shape.Cyan ->
                    "c"
    in
    if isHighlighted then
        String.toUpper char

    else
        char
