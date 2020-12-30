module Game exposing
    ( Game
    , GameBlockInfo
    , MoveDirection(..)
    , UpdateResult(..)
    , UserAction(..)
    , blocks
    , executeUserActions
    , holdShape
    , isPaused
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
    , holdInfo : Maybe HoldInfo -- Information about the shape currently in the Hold area, if any.
    }


{-| Defines the shape currently in the Hold area. Contains the `shape` itself, along with an `allowSwap` flag which
defines whether it's valid for the user to swap the currently dropping shape with this one. This is false initially when
a shape is first put into hold, and true once the shape it's swapped with lands.
-}
type alias HoldInfo =
    { shape : Shape, allowSwap : Bool }


{-| The current state the game is in:

  - `RegularGameState`: the usual state, when a shape is dropping down the board.
  - `RowRemovalGameState`: one or more rows have been completed and are in the process of being removed. They are still
    on the board at this point, but are being animated in some way (see `HighlightAnimation`), typically by flashing them.
    The data for this variant contains the indexes of the rows which are complete, and the dropping shape to render once
    the animation is complete and the game reverts to the regular state.
  - `PausedGameState`: the game is paused. We will always return to `RegularGameState` from this, so we only store the
    `droppingShape` that state needs. In the unlikely event that a user pauses the game while in the `RowRemovalGameState`
    (which is only 150ms, so pretty unlikely), we move out of that state to `RegularGameState` first (i.e. we pretend
    the row removal "flash" animation has completed) then go to the paused state.

-}
type GameState
    = RegularGameState { droppingShape : DroppingShape }
    | RowRemovalGameState { completedRowIndexes : List Int, nextDroppingShape : DroppingShape }
    | PausedGameState { droppingShape : DroppingShape }


{-| Information about all the blocks currently shown in the game (both the landed blocks and the blocks in the currently
dropping shape).

  - `normal`: contains the normal blocks which are rendered with no special effects (e.g. normally landed blocks, and
    the blocks of the dropping shape as it's dropping).
  - `highlighted`: any blocks that are to be highlighted in some way (e.g. the dropping shape when it's at the lowest
    row it can go on, just before being considered as having landed, or blocks that form part of a row about to
    be removed).

The type of animation isn't defined here: it's defined by whatever variant of `UpdateResult` is returned.

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
        , holdInfo = Nothing
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
    | Hold
    | TogglePause


{-| The result of executing a single user action:

  - `NotAllowed`: the user action was not allowed (e.g. the user tried to move a shape left when it was already at the
    leftmost edge of the board.
  - `Allowed`: the user action was allowed. The newly updated dropping shape is the data associated with this variant.

-}
type UserActionResult
    = NotAllowed
    | Allowed DroppingShape


{-| The result of an action (either automated by a timer or made by the user) which typically moves a block (although
might not, e.g. the action to pause a game):

  - `NoChange`: the move was rejected (e.g. trying to move a block left when it's already at the leftmost point it can
    go on the board). The game's model hasn't changed as a result of this attempt.
  - `Continue`: the game should continue as normal. The variant contains the following data:
      - `game`: The updated game.
      - `resetTimerDrop`: Boolean flag indicated whether the timer set up to automatically drop a shape every so often
        should be reset, typically because the user has manually done something to intervene, e.g. dropped a shape.
  - `RowBeingRemoved`: one or more rows are being removed. This means that the parent module should now animate any
    highlighted blocks with the animation used for rows being removed (a "flash" effect). When that animation is
    complete, it should call `onRowRemovalAnimationComplete` again, which will remove those rows and drop all the other
    rows accordingly.
  - `Paused`: the game has been paused.
  - `EndGame`: the game has now ended.

See comments on this module for an explanation of the `shapeBuffer` type parameter.

-}
type UpdateResult shapeBuffer
    = NoChange
    | Continue { game : Game shapeBuffer, resetTimerDrop : Bool }
    | RowBeingRemoved { game : Game shapeBuffer }
    | Paused { game : Game shapeBuffer }
    | GameOver { game : Game shapeBuffer }


{-| Called when the delay between automatic drops of the currently dropping shape has elapsed (i.e. initially every
second or so). Drops the current shape one row if possible, otherwise treats it as now having landed, and uses the next
shape as the new dropping shape.
-}
timerDrop : GetShape shapeBuffer -> Game shapeBuffer -> UpdateResult shapeBuffer
timerDrop getShape (Game ({ state, board } as model)) =
    case state of
        RegularGameState { droppingShape } ->
            let
                proposedDroppingShape =
                    { droppingShape | gridCoord = nextCoord Down droppingShape.gridCoord }
            in
            if isValidPosition board proposedDroppingShape then
                -- It's valid for the currently dropping shape to go down by one row, so just do that.
                continueWithUpdatedDroppingShape proposedDroppingShape False model

            else
                -- It's not valid for the currently dropping shape to go down by one row, so it must have landed.
                handleDroppingShapeLanded getShape droppingShape False model

        RowRemovalGameState _ ->
            NoChange

        PausedGameState _ ->
            NoChange


{-| Executes the supplied of user actions (e.g. moves the dropping shape left and down, if those keys are currently
being held down).
-}
executeUserActions : GetShape shapeBuffer -> List UserAction -> Game shapeBuffer -> UpdateResult shapeBuffer
executeUserActions getShape actions ((Game ({ state, board, holdInfo } as model)) as game) =
    if List.any isTogglePauseAction actions then
        -- Pausing the game: ignore all other actions.
        togglePause game

    else
        case state of
            RegularGameState { droppingShape } ->
                -- The Hold and Drop To Bottom actions are executed by themselves - other actions are ignored.
                if List.any isHoldAction actions then
                    executeHoldAction getShape model droppingShape

                else if List.any isDropToBottomAction actions then
                    executeDropToBottomAction getShape model droppingShape

                else
                    -- TODO: if the actions are "drop down" and "move left", and the shape is directly on top of another
                    -- it cannot be dropped down, but can be moved left, so that's all that'll happen. But it might be that
                    -- once it's moved left it _can_ now be dropped down. Consider handling this edge case.
                    let
                        -- Execute all the actions, and keep a boolean (anyChanges) which is set to true if any changes were made.
                        ( newDroppingShape, anyChanges ) =
                            actions
                                |> List.foldl
                                    (\action ( accDroppingShape, accAnyChanges ) ->
                                        let
                                            result =
                                                case action of
                                                    Move direction ->
                                                        executeMoveAction board accDroppingShape direction

                                                    Rotate direction ->
                                                        executeRotateAction board accDroppingShape direction

                                                    _ ->
                                                        -- These were handled separately above
                                                        NotAllowed
                                        in
                                        case result of
                                            NotAllowed ->
                                                ( accDroppingShape, accAnyChanges )

                                            Allowed updatedDroppingShape ->
                                                ( updatedDroppingShape, True )
                                    )
                                    ( droppingShape, False )

                        -- Reset the timer drop if the shape has moved down a row.
                        resetTimerDrop =
                            isOnDifferentRow droppingShape newDroppingShape
                    in
                    if anyChanges then
                        continueWithUpdatedDroppingShape newDroppingShape resetTimerDrop model

                    else
                        NoChange

            RowRemovalGameState _ ->
                NoChange

            PausedGameState _ ->
                NoChange


isHoldAction : UserAction -> Bool
isHoldAction action =
    case action of
        Hold ->
            True

        _ ->
            False


isDropToBottomAction : UserAction -> Bool
isDropToBottomAction action =
    case action of
        DropToBottom ->
            True

        _ ->
            False


isTogglePauseAction : UserAction -> Bool
isTogglePauseAction action =
    case action of
        TogglePause ->
            True

        _ ->
            False


executeRotateAction : GameBoard -> DroppingShape -> Shape.RotationDirection -> UserActionResult
executeRotateAction board droppingShape direction =
    case nextValidRotatedDroppingShape { droppingShape | shape = Shape.rotate direction droppingShape.shape } board of
        Just rotatedShape ->
            Allowed rotatedShape

        Nothing ->
            NotAllowed


executeMoveAction : GameBoard -> DroppingShape -> MoveDirection -> UserActionResult
executeMoveAction board droppingShape direction =
    let
        proposedDroppingShape =
            { droppingShape | gridCoord = nextCoord direction droppingShape.gridCoord }
    in
    if isValidPosition board proposedDroppingShape then
        Allowed proposedDroppingShape

    else
        NotAllowed


executeDropToBottomAction : GetShape shapeBuffer -> Model shapeBuffer -> DroppingShape -> UpdateResult shapeBuffer
executeDropToBottomAction getShape model droppingShape =
    handleDroppingShapeLanded getShape (calcLandingPos model.board droppingShape) True model


{-| Swaps the currently dropping shape with the shape previously put into hold (if there is one) or the next dropping
shape (if there isn't). Only does this if possible.
-}
executeHoldAction : GetShape shapeBuffer -> Model shapeBuffer -> DroppingShape -> UpdateResult shapeBuffer
executeHoldAction getShape ({ board, holdInfo, nextShape } as model) currentDroppingShape =
    let
        ( isSwapAllowed, shapeToSwap, buildModel ) =
            case holdInfo of
                Just currentHoldInfo ->
                    ( currentHoldInfo.allowSwap, currentHoldInfo.shape, identity )

                Nothing ->
                    ( True, nextShape, withNextShape getShape )
    in
    if isSwapAllowed then
        let
            newDroppingShape =
                initDroppingShape shapeToSwap
        in
        if isValidPosition board newDroppingShape then
            let
                newModel =
                    { model
                        | holdInfo = Just { shape = Shape.withOrgRotation currentDroppingShape.shape, allowSwap = False }
                        , state = RegularGameState { droppingShape = newDroppingShape }
                    }
                        |> buildModel
            in
            Continue { game = Game newModel, resetTimerDrop = True }

        else
            NoChange

    else
        NoChange


{-| Gets a boolean indicating whether the game is currently paused or not.
-}
isPaused : Game shapeBuffer -> Bool
isPaused (Game { state }) =
    case state of
        PausedGameState _ ->
            True

        _ ->
            False


{-| Pauses/resumes the game.
-}
togglePause : Game shapeBuffer -> UpdateResult shapeBuffer
togglePause ((Game model) as game) =
    case model.state of
        RegularGameState { droppingShape } ->
            Paused { game = Game { model | state = PausedGameState { droppingShape = droppingShape } } }

        RowRemovalGameState _ ->
            -- We're in the middle of animating a "flash" indicating that a row is being removed - just complete that to
            -- get back to normal playing state, then pause the game.
            onRowRemovalAnimationComplete game |> togglePause

        PausedGameState { droppingShape } ->
            Continue { game = Game { model | state = RegularGameState { droppingShape = droppingShape } }, resetTimerDrop = True }


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
handleDroppingShapeLanded : GetShape shapeBuffer -> DroppingShape -> Bool -> Model shapeBuffer -> UpdateResult shapeBuffer
handleDroppingShapeLanded getShape droppingShape resetTimerDrop ({ state, board, nextShape } as model) =
    let
        { colour } =
            Shape.data droppingShape.shape

        nextBoard =
            DroppingShape.calcShapeBlocksBoardCoords droppingShape |> GameBoard.append board colour

        nextModel =
            model |> withBoard nextBoard |> withNextShape getShape |> withAllowHoldSwap

        newDroppingShape =
            initDroppingShape nextShape
    in
    case GameBoard.completedRows nextBoard of
        [] ->
            -- No completed rows - continue as normal, but only if the new dropping shape is valid at its
            -- proposed position: if not, the game is over.
            if isValidPosition nextBoard newDroppingShape then
                continueWithUpdatedDroppingShape newDroppingShape resetTimerDrop nextModel

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


withAllowHoldSwap : Model shapeBuffer -> Model shapeBuffer
withAllowHoldSwap model =
    case model.holdInfo of
        Just holdInfo ->
            let
                newHoldInfo =
                    { holdInfo | allowSwap = True }
            in
            { model | holdInfo = Just newHoldInfo }

        Nothing ->
            -- Having no hold info means we can swap
            model


continueWithUpdatedDroppingShape : DroppingShape -> Bool -> Model shapeBuffer -> UpdateResult shapeBuffer
continueWithUpdatedDroppingShape newDroppingShape resetTimerDrop model =
    Continue
        { game = Game { model | state = RegularGameState { droppingShape = newDroppingShape } }
        , resetTimerDrop = resetTimerDrop
        }


isOnDifferentRow : DroppingShape -> DroppingShape -> Bool
isOnDifferentRow droppingShape1 droppingShape2 =
    Tuple.second droppingShape1.gridCoord /= Tuple.second droppingShape2.gridCoord


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
            blocksWithDroppingShape board droppingShape

        RowRemovalGameState { completedRowIndexes } ->
            let
                ( completedRowCells, normalCells ) =
                    GameBoard.occupiedCells board
                        |> List.partition (\( ( _, cellRowIndex ), _ ) -> List.member cellRowIndex completedRowIndexes)
            in
            { normal = normalCells, highlighted = completedRowCells }

        PausedGameState { droppingShape } ->
            blocksWithDroppingShape board droppingShape


blocksWithDroppingShape : GameBoard -> DroppingShape -> GameBlockInfo
blocksWithDroppingShape board droppingShape =
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


{-| Gets the upcoming shape in the game.
-}
upcomingShape : Game shapeBuffer -> Shape
upcomingShape (Game { nextShape }) =
    nextShape


{-| Gets the upcoming shape in the game.
-}
holdShape : Game shapeBuffer -> Maybe Shape
holdShape (Game { holdInfo }) =
    holdInfo |> Maybe.map .shape


{-| Gets an array of blocks which show where the currently dropping shape would land, were it to land right now (i.e. not
be moved left or right before it lands). Returns an empty list if a preview is not required (e.g. if the currently
dropping shape is already at its lowest possible position).
-}
previewLandingBlocks : Game shapeBuffer -> List ( Coord, Shape.BlockColour )
previewLandingBlocks (Game { board, state }) =
    let
        blocksFromDroppingShape droppingShape_ =
            let
                { colour } =
                    Shape.data droppingShape_.shape

                landingShape : DroppingShape
                landingShape =
                    calcLandingPos board droppingShape_
            in
            -- If shape is already where it would be, were it to land, we don't have a preview
            if landingShape.gridCoord == droppingShape_.gridCoord then
                []

            else
                DroppingShape.calcShapeBlocksBoardCoords landingShape |> BoardView.withColour colour
    in
    case state of
        RegularGameState { droppingShape } ->
            blocksFromDroppingShape droppingShape

        RowRemovalGameState _ ->
            -- We don't have a currently dropping shape so can't preview where it would land.
            []

        PausedGameState { droppingShape } ->
            blocksFromDroppingShape droppingShape


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
