module Game exposing
    ( Game
    , GameBlockInfo
    , MoveDirection(..)
    , MoveResult(..)
    , UserAction(..)
    , blocks
    , executeUserActions
    , new
    , onRowRemovalAnimationComplete
    , previewLandingBlocks
    , timerDrop
    , upcomingShape
    )

{-| This module has the model and logic for a game. It's responsible for responding to requests (typically originating
from the user or from a timer event) to move the dropping shape, add a new dropping shape when it's landed, etc. It
doesn't have its own messages and doesn't subscribe to timer/keyboard events etc. - instead, it exposes functions which
the parent module calls when those events occur (e.g. `executeUserAction` and `timerDrop`).

It also doesn't have any built-in random behaviour: instead, it receives a `shapeBuffer` value (which is stored in this
module's model) along with a function which is used to get a new shape from that buffer. That function also returns an
updated version of the `shapeBuffer` for subsequent use. `shapeBuffer` is a type parameter used on some of the types in
this module, rather than a fixed type. This allows that shape buffer to be implemented differently for normal usage
(when random shapes are generated) and for unit tests (when fixed, known shapes are used).

The above design decisions help keep this module simpler and more self-contained and, more importantly, easy to unit
test, as it doesn't directly rely on any randomness or any time passing, so can be controlled in a completely
predictable and time-independent fashion in tests.

-}

import BoardView
import Coord exposing (Coord)
import DroppingShape exposing (DroppingShape)
import GameBoard exposing (GameBoard)
import Shape exposing (Shape)



-- MODEL TYPES


{-| A game in the process of being played.

See comments on this module for an explanation of the `shapeBuffer` type parameter.

-}
type Game shapeBuffer
    = Game (Model shapeBuffer)


{-| Information about the currently playing game.

See comments on this module for an explanation of the `shapeBuffer` type parameter.

-}
type alias Model shapeBuffer =
    { board : GameBoard -- The current board (only landed blocks, not the currently dropping shape).
    , state : GameState -- The current state (see `GameState` for more details).
    , nextShape : Shape -- The next shape to use as the dropping shape once the current one lands.
    , shapeBuffer : shapeBuffer -- A buffer used to get more shapes on demand.
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
    { normal : List ( Coord, Shape.BlockColour ), highlighted : List ( Coord, Shape.BlockColour ) }


{-| The function used to get another shape from the shape buffer. Also returns an updated buffer value to use in
subsequent calls.
-}
type alias GetShape shapeBuffer =
    shapeBuffer -> ( Shape, shapeBuffer )



-- INITIALISATION


{-| Starts a new game. Uses the supplied buffer (and `GetShape` function) to get the initially dropping shape.
-}
new : GetShape shapeBuffer -> shapeBuffer -> Game shapeBuffer
new getShape shapeBuffer =
    let
        ( initialShape, nextShape, shapeBuffer_ ) =
            getShape shapeBuffer
                |> (\( shape1, buffer1 ) ->
                        getShape buffer1
                            |> (\( shape2, buffer2 ) ->
                                    ( shape1, shape2, buffer2 )
                               )
                   )
    in
    Game
        { board = GameBoard.emptyBoard
        , state = RegularGameState { droppingShape = initDroppingShape initialShape }
        , nextShape = nextShape
        , shapeBuffer = shapeBuffer_
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


{-| The direction in which a shape can be moved by the user (or also automatically, in the case of `Down`).
-}
type MoveDirection
    = Left
    | Right
    | Down


{-| An action initiated by the user.
-}
type UserAction
    = Move MoveDirection
    | DropToBottom
    | Rotate Shape.RotationDirection


{-| The result of executing a single user action:

  - `NotAllowed`: the user action was not allowed (e.g. the user tried to move a shape left when it was already at the
    leftmost edge of the board.
  - `Allowed`: the user action was allowed. The newly updated dropping shape is the data associated with this variant.

-}
type UserActionResult
    = NotAllowed
    | Allowed DroppingShape


{-| The result of an action (either automated by a timer or made by the user) which moves a block:

  - `NoChange`: the move was rejected (e.g. trying to move a block left when it's already at the leftmost point it can
    go on the board). The game's model hasn't changed as a result of this attempt.
  - `Continue`: the game should continue as normal. The variant contains the following data:
      - `game`: The updated game.
      - `shapedDropped`: Boolean flag indicated whether the currently dropping shape was dropped. If so then the timer
        set up to automatically drop a shape every so often should be reset, now the user has done this manually.
  - `RowBeingRemoved`: one or more rows are being removed. This means that the parent module should now animate any
    highlighted blocks with the animation used for rows being removed (a "flash" effect). When that animation is
    complete, it should call `onRowRemovalAnimationComplete` again, which will remove those rows and drop all the other
    rows accordingly.
  - `EndGame`: the game has now ended.

See comments on this module for an explanation of the `shapeBuffer` type parameter.

-}
type MoveResult shapeBuffer
    = NoChange
    | Continue { game : Game shapeBuffer, shapeDropped : Bool }
    | RowBeingRemoved { game : Game shapeBuffer }
    | GameOver { game : Game shapeBuffer }


{-| Called when the delay between automatic drops of the currently dropping shape has elapsed (i.e. initially every
second or so). Drops the current shape one row if possible, otherwise treats it as now having landed, and uses the next
shape as the new dropping shape.
-}
timerDrop : GetShape shapeBuffer -> Game shapeBuffer -> MoveResult shapeBuffer
timerDrop getShape ((Game ({ state, board } as model)) as game) =
    case state of
        RegularGameState { droppingShape } ->
            let
                proposedDroppingShape =
                    { droppingShape | gridCoord = nextCoord Down droppingShape.gridCoord }
            in
            if isValidPosition board proposedDroppingShape then
                -- It's valid for the currently dropping shape to go down by one row, so just do that.
                continueWithUpdatedDroppingShape droppingShape proposedDroppingShape model

            else
                -- It's not valid for the currently dropping shape to go down by one row, so it must have landed.
                handleDroppingShapeLanded getShape droppingShape game

        RowRemovalGameState _ ->
            NoChange


{-| Executes the supplied of user actions (e.g. moves the dropping shape left and down, if those keys are currently
being held down).
-}
executeUserActions : List UserAction -> Game shapeBuffer -> MoveResult shapeBuffer
executeUserActions actions (Game ({ state, board } as model)) =
    case state of
        RegularGameState { droppingShape } ->
            -- TODO: if any of the actions is Drop with a DropType of ToBottom then ignore all other actions and
            -- just execute that single action.
            -- TODO: if the actions are "drop down" and "move left", and the shape is directly on top of another
            -- it cannot be dropped down, but can be moved left, so that's all that'll happen. But it might be that
            -- once it's moved left it _can_ now be dropped down. Consider handling this edge case.
            let
                -- Execute all the actions, and keep a boolean (anyChanges) which is set to true if any changes were made.
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
            in
            if anyChanges then
                continueWithUpdatedDroppingShape droppingShape newDroppingShape model

            else
                NoChange

        RowRemovalGameState _ ->
            NoChange


{-| Attempts to execute the supplied user action and returns a result defining whether or not the action was allowed.
-}
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
            -- TODO: "Implement dropping shape to bottom"
            NotAllowed

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
handleDroppingShapeLanded : GetShape shapeBuffer -> DroppingShape -> Game shapeBuffer -> MoveResult shapeBuffer
handleDroppingShapeLanded getShape droppingShape (Game ({ state, board, nextShape } as model)) =
    let
        { colour } =
            Shape.data droppingShape.shape

        nextBoard =
            DroppingShape.calcShapeBlocksBoardCoords droppingShape |> GameBoard.append board colour

        nextModel =
            model |> withBoard nextBoard |> withNextShape getShape

        newDroppingShape =
            initDroppingShape nextShape
    in
    case GameBoard.completedRows nextBoard of
        [] ->
            -- No completed rows - continue as normal, but only if the new dropping shape is valid at its
            -- proposed position: if not, the game is over.
            if isValidPosition nextBoard newDroppingShape then
                continueWithUpdatedDroppingShape droppingShape newDroppingShape nextModel

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


withBoard : GameBoard -> Model shapeBuffer -> Model shapeBuffer
withBoard board model =
    { model | board = board }


withNextShape : GetShape shapeBuffer -> Model shapeBuffer -> Model shapeBuffer
withNextShape getShape model =
    let
        ( nextShape, nextBuffer ) =
            getShape model.shapeBuffer
    in
    { model | nextShape = nextShape, shapeBuffer = nextBuffer }


continueWithUpdatedDroppingShape : DroppingShape -> DroppingShape -> Model shapeBuffer -> MoveResult shapeBuffer
continueWithUpdatedDroppingShape orgDroppingShape newDroppingShape model =
    let
        shapedDropped =
            Tuple.second orgDroppingShape.gridCoord /= Tuple.second newDroppingShape.gridCoord
    in
    Continue
        { game = Game { model | state = RegularGameState { droppingShape = newDroppingShape } }
        , shapeDropped = shapedDropped
        }


{-| Called when the animation of rows about to be removed has completed. Removes those rows and returns the game to its
regular state.
-}
onRowRemovalAnimationComplete : Game shapeBuffer -> Game shapeBuffer
onRowRemovalAnimationComplete (Game ({ board, state } as model)) =
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
blocks : Game shapeBuffer -> GameBlockInfo
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


{-| Gets the upcoming shape in the game.
-}
upcomingShape : Game shapeBuffer -> Shape
upcomingShape (Game game) =
    game.nextShape


{-| Gets an array of blocks which show where the currently dropping shape would land, were it to land right now (i.e. not
be moved left or right before it lands). Returns an empty list if a preview is not required (e.g. if the currently
dropping shape is already at its lowest possible position).
-}
previewLandingBlocks : Game shapeBuffer -> List ( Coord, Shape.BlockColour )
previewLandingBlocks (Game { board, state }) =
    case state of
        RegularGameState { droppingShape } ->
            let
                { colour } =
                    Shape.data droppingShape.shape

                landingShape : DroppingShape
                landingShape =
                    calcLandingPos board droppingShape
            in
            -- If shape is already where it would be, were it to land, we don't have a preview
            if landingShape.gridCoord == droppingShape.gridCoord then
                []

            else
                DroppingShape.calcShapeBlocksBoardCoords landingShape |> BoardView.withColour colour

        RowRemovalGameState { completedRowIndexes } ->
            -- We don't have a currently dropping shape so can't preview where it would land.
            []


{-| Gets a `DroppingShape` representing the supplied shape, at its lowest possible position (i.e. where it would land
if it weren't to be moved).
-}
calcLandingPos : GameBoard -> DroppingShape -> DroppingShape
calcLandingPos board droppingShape =
    let
        proposedDroppingShape =
            { droppingShape | gridCoord = nextCoord Down droppingShape.gridCoord }
    in
    if isValidPosition board proposedDroppingShape then
        calcLandingPos board proposedDroppingShape

    else
        droppingShape
