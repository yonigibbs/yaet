module GameTests exposing (suite)

{-| Tests for the Game module. Game is an opaque type so we can't construct it ourselves here, pre-populated in some
way. Instead, we start off with a new game (supplying a known set of shapes rather than random ones), then progress the
game by simulating user and timer actions.
-}

import BlockColour exposing (BlockColour)
import Board
import Coord exposing (Coord)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Game exposing (Game)
import Shape
import ShapeUtils
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Game"
        [ test "Drops initial shape two rows after two timer drop events." <|
            \_ ->
                defaultInitialGameState
                    |> repeat 2 Game.timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame BottomPadding """
                                ----------
                                ----------
                                -----b----
                                ---bbb----
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Highlights landed shape after moving initial shape to left edge then letting it drop to bottom." <|
            \_ ->
                defaultInitialGameState
                    |> repeat 3 (Game.moveShape Game.Left)
                    |> repeat 18 Game.timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame TopPadding """
                                --B-------
                                BBB-------
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Next shape appears after shape drops to bottom (and bottom shape no longer highlighted)." <|
            \_ ->
                defaultInitialGameState
                    |> repeat 3 (Game.moveShape Game.Left)
                    |> repeat 19 Game.timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame NoPadding """
                                ----rr----
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
                                --b-------
                                bbb-------
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Completed row highlighted." <|
            \_ ->
                defaultInitialGameState
                    -- Move blue L to the left (will first first three columns)
                    |> repeat 3 (Game.moveShape Game.Left)
                    -- Drop it the 18 rows required to get it to the bottom, then timer drop to make next shape appear.
                    |> repeat 18 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    -- Red square has now appeared: move it left one, then drop it to fill columns 4 and 5.
                    |> progressGame (Game.moveShape Game.Left)
                    |> repeat 18 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    -- Yellow line has now appeared: move it right two then drop it to fill columns 6-9 (intersperse
                    -- some timer drops before the user interactions).
                    |> repeat 2 Game.timerDrop
                    |> progressGame (Game.moveShape Game.Right)
                    |> progressGame Game.timerDrop
                    |> progressGame (Game.moveShape Game.Right)
                    |> repeat 15 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    -- Green half-plus has now appeared: it's on its back so rotate it once anti-clockwise then move it
                    -- all the way to the right before dropping it.
                    |> progressGame (Game.rotateShape Shape.Anticlockwise)
                    |> repeat 5 (Game.moveShape Game.Right)
                    |> repeat 17 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame TopPadding """
                                ---------g
                                --brr---gg
                                BBBRRYYYYG
                                """
                            , rowRemoval = RowBeingRemoved
                            }
                        )
        , test "New shape added after completed row removed." <|
            \_ ->
                defaultInitialGameState
                    -- Repeat same steps as previous test, then update the game to make the completed row disappear, and
                    -- the next shape appear.
                    |> repeat 3 (Game.moveShape Game.Left)
                    |> repeat 18 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    |> progressGame (Game.moveShape Game.Left)
                    |> repeat 18 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    |> repeat 2 Game.timerDrop
                    |> progressGame (Game.moveShape Game.Right)
                    |> progressGame Game.timerDrop
                    |> progressGame (Game.moveShape Game.Right)
                    |> repeat 15 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    |> progressGame (Game.rotateShape Shape.Anticlockwise)
                    |> repeat 5 (Game.moveShape Game.Right)
                    |> repeat 17 (Game.moveShape Game.Down)
                    |> progressGame Game.timerDrop
                    |> simulateRowRemovalAnimationComplete
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame TopPadding """
                                ---oo-----
                                ----oo----
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
                                ---------g
                                --brr---gg
                                """
                            , rowRemoval = NoRowRemoval
                            }
                        )
        , test "Does not move shape off board if user tries to move it too much." <|
            \_ ->
                defaultInitialGameState
                    |> repeat 20 (Game.moveShape Game.Left)
                    |> expectGame
                        (ExpectedGame
                            { game = buildAsciiGame BottomPadding """
                                --b-------
                                bbb-------
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
    { game : Game, rowRemoval : RowRemoval }


{-| Creates an expectation that the state of supplied game matches the supplied expected state.
-}
expectGame : ExpectedGame -> GameState -> Expectation
expectGame (ExpectedGame expected) actual =
    ( gameAsAscii actual.game, actual.rowRemoval )
        |> Expect.equal ( expected.game, expected.rowRemoval )


{-| Gets a string representation of the the supplied game, so it can be compared to an expected game state.
-}
gameAsAscii : Game -> AsciiGame
gameAsAscii game =
    let
        { normal, highlighted } =
            Game.blocks game

        mapBlocks : Bool -> List ( Coord, BlockColour ) -> List ( Coord, ( BlockColour, Bool ) )
        mapBlocks isHighlighted =
            List.map (\( coord, colour ) -> ( coord, ( colour, isHighlighted ) ))

        allBlocks : Dict Coord ( BlockColour, Bool )
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
            List.range 0 (Board.colCount - 1)
                |> List.map (\x -> cellAsString x y)
                |> String.concat
    in
    List.range 0 (Board.rowCount - 1)
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
            List.repeat Board.colCount "-" |> String.concat

        padding : List String
        padding =
            List.repeat
                (Board.rowCount - List.length suppliedLines)
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


{-| The default initial state to use when creating a new game. The initial shapes allow a full row to be completed with
the first four shapes. The shapes in order are:

  - Blue L
  - Red square
  - Yellow line
  - Green half-plus
  - Orange Z
  - Purple Z-mirror-image
  - Red L-mirror-image

-}
defaultInitialGameState : GameState
defaultInitialGameState =
    let
        game =
            Game.new
                { initialShape = ShapeUtils.getShape BlockColour.Blue ShapeUtils.Ell
                , nextShape = ShapeUtils.getShape BlockColour.Red ShapeUtils.Square
                , shapeBuffer =
                    [ ShapeUtils.getShape BlockColour.Yellow ShapeUtils.Line
                    , ShapeUtils.getShape BlockColour.Green ShapeUtils.HalfPlus
                    , ShapeUtils.getShape BlockColour.Orange ShapeUtils.Zed
                    , ShapeUtils.getShape BlockColour.Purple ShapeUtils.ZedMirror
                    , ShapeUtils.getShape BlockColour.Red ShapeUtils.EllMirror
                    ]
                }
    in
    { game = game, rowRemoval = NoRowRemoval }


{-| Progresses the game by executing some function that returns a `MoveResult` (e.g. `Game.timerDrop` or
`Game.rotateShape`. This can either be simulating a user action like pressing an arrow, or simulating a timer event.
-}
progressGame : (Game -> Game.MoveResult) -> GameState -> GameState
progressGame action state =
    let
        moveResult =
            action state.game
    in
    case moveResult of
        Game.NoChange ->
            state

        Game.Continue { game } ->
            { game = game, rowRemoval = NoRowRemoval }

        Game.RowBeingRemoved { game } ->
            { game = game, rowRemoval = RowBeingRemoved }

        Game.GameOver _ ->
            -- TODO: is this OK to do in tests?
            Debug.todo "Unexpected game over"


{-| Executes the given action the given number of times, starting from the supplied state, and returning the state at the
end of all those actions.
-}
repeat : Int -> (Game -> Game.MoveResult) -> GameState -> GameState
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
occupiedCellChar : BlockColour -> Bool -> String
occupiedCellChar colour isHighlighted =
    let
        char =
            case colour of
                BlockColour.Blue ->
                    "b"

                BlockColour.Red ->
                    "r"

                BlockColour.Orange ->
                    "o"

                BlockColour.Yellow ->
                    "y"

                BlockColour.Purple ->
                    "p"

                BlockColour.Green ->
                    "g"
    in
    if isHighlighted then
        String.toUpper char

    else
        char
