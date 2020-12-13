module Game exposing
    ( Game
    , GameBlockInfo
    , InitialisationInfo
    , MoveDirection(..)
    , MoveResult(..)
    , UserAction(..)
    , blocks
    , executeUserActions
    , new
    , onRowRemovalAnimationComplete
    , shapeGenerated
    , timerDrop
    )

{-| This module has the model and logic for a game. It's responsible for responding to requests (typically originating
from the user or from a timer event) to move the dropping shape, add a new dropping shape when it's landed, etc. It
doesn't actually subscribe to timer/keyboard events etc. - instead, it exposes functions which the parent module calls
when those subscriptions fire (e.g. `moveShape`).

Similarly, it doesn't have any of its own messages (e.g. for generating a random next shape): instead, it receives the
shapes required to initialise the model, and a buffer of shapes to use next (e.g. whenever a shape lands and a new
dropping shape is required). Whenever this happens, it takes the next item from the buffer. Then, to refill the buffer,
it responds to the parent module with a flag telling it to generate a new random shape, then add it to the buffer by
calling `shapeGenerated`. The parent module, therefore, is responsible for making sure there are always enough shapes in
the buffer so that the game will never get into a situation where it needs a shape but the buffer doesn't contain one.
(Realistically as long as there is even one that should be fine, as the time it takes to generate the next random shape
should be negligible. But for safety it is better to have a few more.)

The above design decisions help keep this module simpler and more self-contained and, more importantly, easy to unit
test, as it doesn't directly rely on any randomness or any time passing, so can be controlled in a completely
predictable and time-independent fashion in tests.

-}

import BlockColour exposing (BlockColour)
import BoardView
import Coord exposing (Coord)
import DroppingShape exposing (DroppingShape)
import GameBoard exposing (GameBoard)
import Shape exposing (Shape)



-- MODEL TYPES


{-| A game in the process of being played.
-}
type Game
    = Game Model


{-| Information about the currently playing game.
-}
type alias Model =
    { board : GameBoard -- The current board (only landed blocks, not the currently dropping shape).
    , state : GameState -- The current state (see `GameState` for more details).
    , nextShape : Shape -- The next shape to use as the dropping shape once the current one lands.
    , shapeBuffer : List Shape -- Buffer of available random shapes. See comments on this module for details.
    }


{-| The current state the game is in:

  - `RegularGameState`: the usual state, when a shape is dropping down the board.
  - `RowRemovalGameState`: one or more rows have been completed and are in the process of being removed. They are still
    on the board at this point, but are being animated in some way (see `HighlightAnimation`), typically by flashing them.
    The data for this variant contains the indexes of the rows which are complete, and the dropping shape to render once
    the animation is complete and the game reverts to the regular state.

-}
type GameState
    = RegularGameState { droppingShape : DroppingShape }
    | RowRemovalGameState { completedRowIndexes : List Int, nextDroppingShape : DroppingShape }


{-| Information about all the blocks currently shown in the game (both the landed blocks and the blocks in the currently
dropping shape).

  - `normal`: contains the normal blocks which are rendered with no special effects (e.g. normally landed blocks, and
    the blocks of the dropping shape as it's dropping).
  - `highlighted`: any blocks that are to be highlighted in some way (e.g. the dropping shape when it's at the lowest
    row it can go on, just before being considered as having landed, or blocks that form part of a row about to
    be removed).

The type of animation isn't defined here: it's defined in `MoveResult`.

-}
type alias GameBlockInfo =
    { normal : List ( Coord, BlockColour ), highlighted : List ( Coord, BlockColour ) }



-- INITIALISATION


{-| The information required to create a new game. See comments on this module for information about what the shape
buffer is.
-}
type alias InitialisationInfo =
    { initialShape : Shape, nextShape : Shape, shapeBuffer : List Shape }


{-| Starts a new game with the supplied shape as the initially dropping shape.
-}
new : InitialisationInfo -> Game
new { initialShape, nextShape, shapeBuffer } =
    Game
        { board = GameBoard.emptyBoard
        , state = RegularGameState { droppingShape = initDroppingShape initialShape }
        , nextShape = nextShape
        , shapeBuffer = shapeBuffer
        }


{-| Creates a `DroppingShape` out of the supplied `Shape` by calculating its initial coordinates on the board.
-}
initDroppingShape : Shape -> DroppingShape
initDroppingShape shape =
    let
        shapeGridSize =
            Shape.data shape |> .gridSize

        x =
            ((toFloat GameBoard.colCount / 2) - (toFloat shapeGridSize / 2)) |> floor

        y =
            GameBoard.rowCount - shapeGridSize
    in
    { shape = shape, gridCoord = ( x, y ) }



-- GAME PLAY/MOVEMENT


{-| The direction in which a shape can be moved by the user (or automatically, in the case of `Down`).
-}
type MoveDirection
    = Left
    | Right
    | Down


type UserAction
    = Move MoveDirection
    | DropToBottom
    | Rotate Shape.RotationDirection


type UserActionResult
    = NotAllowed
    | Allowed DroppingShape


{-| The result of an action (either automated by a timer or made by the user) which moves a block:

  - `NoChange`: the move was rejected (e.g. trying to move a block left when it's already at the leftmost point it can
    go on the board). The game's model hasn't changed as a result of this attempt.
  - `Continue`: the game should continue as normal. The updated game is supplied in the variant's data, along with a
    `newShapeRequested` boolean which, if true, means a new dropping was added to the screen, so the buffer needs filling
    up with one more shape. The parent module should generate a random shape (asynchronously) and call `shapeGenerated`,
    passing in that random shape, when it's available. If the game has any highlighted cells then the parent module should
    now animate these. When that animation is complete, it should call `timerDrop` again, which will treat the dropping
    shape as now having landed.
  - `RowBeingRemoved`: one or more rows are being removed. This means that the parent module should now animate any
    highlighted blocks with the animation used for rows being removed (a "flash" effect). When that animation is
    complete, it should call `onRowRemovalAnimationComplete` again, which will remove those rows and drop all the other
    rows accordingly. This variant implicitly means that a new shape is now required (as when rows are removed a new
    dropping shape is always added next).
  - `EndGame`: the game has now ended.

-}
type MoveResult
    = NoChange
    | Continue { game : Game, newShapeRequested : Bool, shapeDropped : Bool }
    | RowBeingRemoved { game : Game }
    | GameOver { game : Game }


{-| Called when the delay between automatic drops of the currently dropping shape has elapsed (i.e. initially every
second or so). Drops the current shape one row if possible, otherwise treats it as now having landed, and uses the next
shape as the new dropping shape.
-}
timerDrop : Game -> MoveResult
timerDrop ((Game ({ state, board } as model)) as game) =
    case state of
        RegularGameState { droppingShape } ->
            let
                proposedDroppingShape =
                    { droppingShape | gridCoord = nextCoord Down droppingShape.gridCoord }
            in
            if isValidPosition board proposedDroppingShape then
                -- It's valid for the currently dropping shape to go down by one row, so just do that.
                continueWithUpdatedDroppingShape False True proposedDroppingShape model

            else
                -- It's not valid for the currently dropping shape to go down by one row, so it must have landed.
                handleDroppingShapeLanded game

        RowRemovalGameState _ ->
            NoChange


{-| Moves the currently dropping shape in the supplied direction, if possible.
-}
executeUserActions : Game -> List UserAction -> MoveResult
executeUserActions (Game ({ state, board } as model)) actions =
    case state of
        RegularGameState { droppingShape } ->
            let
                -- TODO: if any of the actions is Drop with a DropType of ToBottom then ignore all other actions and
                -- just execute that single action.
                ( newDroppingShape, anyChanges ) =
                    actions
                        |> List.foldl
                            (\action ( accDroppingShape, accAnyChanges ) ->
                                case executeUserAction board accDroppingShape action of
                                    NotAllowed ->
                                        ( accDroppingShape, accAnyChanges )

                                    Allowed updatedDroppingShape ->
                                        ( updatedDroppingShape, True )
                            )
                            ( droppingShape, False )

                shapeDropped =
                    anyChanges && (Tuple.second droppingShape.gridCoord /= Tuple.second newDroppingShape.gridCoord)
            in
            if anyChanges then
                continueWithUpdatedDroppingShape False shapeDropped newDroppingShape model

            else
                NoChange

        RowRemovalGameState _ ->
            NoChange


executeUserAction : GameBoard -> DroppingShape -> UserAction -> UserActionResult
executeUserAction board droppingShape action =
    case action of
        Move direction ->
            let
                proposedDroppingShape =
                    { droppingShape | gridCoord = nextCoord direction droppingShape.gridCoord }
            in
            if isValidPosition board proposedDroppingShape then
                Allowed proposedDroppingShape

            else
                NotAllowed

        DropToBottom ->
            Debug.todo "Implement dropping shape to bottom"

        Rotate direction ->
            case nextValidRotatedDroppingShape { droppingShape | shape = Shape.rotate direction droppingShape.shape } board of
                Just rotatedShape ->
                    Allowed rotatedShape

                Nothing ->
                    NotAllowed


{-| Gets the next valid position for a rotate shape, if one exists. Moves the shape back onto the board if its rotation
has meant that it's now off either side.
-}
nextValidRotatedDroppingShape : DroppingShape -> GameBoard -> Maybe DroppingShape
nextValidRotatedDroppingShape droppingShape board =
    let
        shapeCoords =
            DroppingShape.calcShapeBlocksBoardCoords droppingShape
    in
    if List.any (\( x, _ ) -> x < 0) shapeCoords then
        -- Shape is off the left edge of the board: move it right one cell then try again.
        nextValidRotatedDroppingShape { droppingShape | gridCoord = nextCoord Right droppingShape.gridCoord } board

    else if List.any (\( x, _ ) -> x >= GameBoard.colCount) shapeCoords then
        -- Shape is off the right edge of the board: move it left one cell then try again.
        nextValidRotatedDroppingShape { droppingShape | gridCoord = nextCoord Left droppingShape.gridCoord } board

    else if GameBoard.areCellsAvailable board shapeCoords then
        -- Shape is on the board and valid.
        Just droppingShape

    else
        -- Shape is on the board but in an invalid position.
        Nothing


{-| Handles the case when the dropping shape has landed: appends its blocks to the board and takes the next item off the
buffer to be the new "next" shape.
-}
handleDroppingShapeLanded : Game -> MoveResult
handleDroppingShapeLanded (Game ({ shapeBuffer, state, board, nextShape } as model)) =
    -- We can only do this we have a shape in the buffer, as we need to take a shape out the buffer and make it the new
    -- "next" shape.
    case ( shapeBuffer, state ) of
        ( firstInBuffer :: restOfBuffer, RegularGameState { droppingShape } ) ->
            -- We have one, so we can use it:
            let
                { colour } =
                    Shape.data droppingShape.shape

                nextBoard =
                    GameBoard.append board colour (DroppingShape.calcShapeBlocksBoardCoords droppingShape)

                completedRows =
                    GameBoard.completedRows nextBoard

                nextModel =
                    { model | board = nextBoard, nextShape = firstInBuffer, shapeBuffer = restOfBuffer }

                newDroppingShape =
                    initDroppingShape nextShape
            in
            case completedRows of
                [] ->
                    -- No completed rows - continue as normal, but only if the new dropping shape is valid at its
                    -- proposed position: if not, the game is over.
                    if isValidPosition nextBoard newDroppingShape then
                        continueWithUpdatedDroppingShape True False newDroppingShape nextModel

                    else
                        GameOver { game = Game { model | board = nextBoard } }

                completedRowIndexes ->
                    RowBeingRemoved
                        { game =
                            Game
                                { nextModel
                                    | state =
                                        RowRemovalGameState
                                            { completedRowIndexes = completedRowIndexes
                                            , nextDroppingShape = newDroppingShape
                                            }
                                }
                        }

        _ ->
            -- Nothing in the buffer so we can't accept this
            NoChange


continueWithUpdatedDroppingShape : Bool -> Bool -> DroppingShape -> Model -> MoveResult
continueWithUpdatedDroppingShape newShapeRequested shapedDropped droppingShape model =
    -- TODO: change params to take in a record for this function
    Continue
        { game = Game { model | state = RegularGameState { droppingShape = droppingShape } }
        , newShapeRequested = newShapeRequested
        , shapeDropped = shapedDropped
        }


{-| Called when the animation of rows about to be removed has completed. Removes those rows and returns the game to its
regular state.
-}
onRowRemovalAnimationComplete : Game -> Game
onRowRemovalAnimationComplete (Game ({ board, nextShape, state } as model)) =
    case state of
        RowRemovalGameState { completedRowIndexes, nextDroppingShape } ->
            let
                nextBoard =
                    GameBoard.removeRows board completedRowIndexes
            in
            Game
                { model
                    | board = nextBoard
                    , state = RegularGameState { droppingShape = nextDroppingShape }
                }

        _ ->
            -- Nothing in the buffer so we can't accept this
            Game model


{-| Called when a random shape has been generated and is ready to add to the buffer.
-}
shapeGenerated : Game -> Shape -> Game
shapeGenerated (Game model) shape =
    Game { model | shapeBuffer = model.shapeBuffer ++ [ shape ] }



-- QUERYING INFORMATION ABOUT THE GAME


{-| Calculates the next coordinate of the supplied coordinate after it is moved one cell in the supplied direction.
-}
nextCoord : MoveDirection -> Coord -> Coord
nextCoord direction ( x, y ) =
    case direction of
        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


{-| Calculates whether the supplied dropping shape is valid to be at its specified coordinates, for the given board.
-}
isValidPosition : GameBoard -> DroppingShape -> Bool
isValidPosition board droppingShape =
    DroppingShape.calcShapeBlocksBoardCoords droppingShape |> GameBoard.areCellsAvailable board


{-| Gets all the blocks that are currently in the game, including landed blocks and ones that are part of the dropping
shape. Used for rendering, as the distinction between the dropping blocks and the landed blocks there is irrelevant.
-}
blocks : Game -> GameBlockInfo
blocks (Game { board, state }) =
    case state of
        RegularGameState { droppingShape } ->
            let
                { colour } =
                    Shape.data droppingShape.shape

                droppingShapeBlocks =
                    DroppingShape.calcShapeBlocksBoardCoords droppingShape |> BoardView.withColour colour

                canDropMore =
                    isValidPosition board { droppingShape | gridCoord = nextCoord Down droppingShape.gridCoord }
            in
            if canDropMore then
                { normal = GameBoard.occupiedCells board ++ droppingShapeBlocks, highlighted = [] }

            else
                { normal = GameBoard.occupiedCells board, highlighted = droppingShapeBlocks }

        RowRemovalGameState { completedRowIndexes } ->
            let
                ( completedRowCells, normalCells ) =
                    GameBoard.occupiedCells board
                        |> List.partition (\( ( _, cellRowIndex ), _ ) -> List.member cellRowIndex completedRowIndexes)
            in
            { normal = normalCells, highlighted = completedRowCells }
