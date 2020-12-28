module UserGame exposing (Model, Msg, UpdateResult(..), boardViewConfig, init, subscriptions, update, view)

{-| This module contains functionality related to a game being played by the user. The actual game logic itself is all
in the `Game` module, but that module itself is, in a sense, "inert" - it doesn't do anything by itself, instead requiring
a "controlling" module which will act on it (e.g. tell it to move a shape in some direction, tell it some time period has
elapsed, etc). This module does that when the game is being controlled normally by the user (as opposed to, for example,
in a unit test). This module provides that control, and also the view for rendering the game screen to the user (with
the board and other related items such as a Pause button, etc.).
-}

import BoardView
import Coord exposing (Coord)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Game exposing (Game)
import GameBoard
import HighlightAnimation
import Process
import Random
import Shape exposing (Shape)
import Task
import Time
import UserGameControl



-- MODEL


type Model
    = Initialising
    | Playing PlayingModel


{-| The data associated with a game currently being played.

  - `game`: The current game, passed into the `Game` module whenever events occur such as the user moving a block, etc.
  - `timerDropDelay`: How long, in ms, before the currently dropping shape should be automatically dropped down a row.
    As the game continues this decrements to make the game speed up.
  - `timerDropMessageId`: The ID associated with the `TimerDropDelayElapsed` messages which should be responded to. Every
    message of that type has an ID associated with it, which is the value that this model field had when the system decided
    to wait `timerDropDelay` ms before doing the next drop. If, by the time the message arrives (i.e. after the delay),
    the ID of the message still matches the value in the model, then the message should be acted on. However if, between
    the system starting to wait (`Process.sleep`) and the message arriving, the user manually drops a shape down a row,
    then we set a new value for this field in the model: when the message then arrives, we know to ignore it.
  - `normalBlocks`: The normal blocks which are to be rendered with no special effects (animation). These are calculated
    by calling `Game.blocks` (which calculates them based on the supplied `game`), so arguably it's a redundant duplication
    to store them in the model here, but this is done for performance reasons. `Game.blocks` has to do a bit of
    calculation to convert its own internal representation of the game's state to that required for rendering it, and we
    don't want that calculation to run multiple times a second when rendering animations, so we store it here. It's
    important that whenever `game` is updated with potentially new blocks, this field is correspondingly updated.
  - `previewLandingBlocks`: The blocks of the currently dropping shape, were it to land immediately now. This is shown
    to the user to let them see where the shape will land. Like `normalBlocks` this can be calculated at runtime from the
    `game`, but for performance reasons we cache it.
  - `highlightAnimation`: If any blocks are currently highlighted (e.g. because a shape is about to land) this contains
    the animation used to provide that highlighting. As for `normalBlocks`, this information can be calculated from the
    game, but for performance reasons is stored in the model.
  - `nextAnimationId`: The unique ID to use for the next animation. See the `Id` type in the `HighlightAnimation` module
    for more info on this.
  - `gameControl`: The model of the `UserGameControl` module, managing the user's controlling of the game, e.g. what
    keyboard keys are currently held down, etc.

-}
type alias PlayingModel =
    { game : Game Shape.Bag
    , timerDropDelay : Int
    , timerDropMessageId : Int
    , normalBlocks : List ( Coord, Shape.BlockColour )
    , previewLandingBlocks : List ( Coord, Shape.BlockColour )
    , highlightAnimation : Maybe HighlightAnimation.Model
    , nextAnimationId : HighlightAnimation.Id
    , gameControl : UserGameControl.Model
    }


{-| Initialises the game - this involves getting the current time to act as a random seed for generating random shapes.
-}
init : ( Model, Cmd Msg )
init =
    ( Initialising, Time.now |> Task.perform (Time.posixToMillis >> Random.initialSeed >> Initialised) )



-- UPDATE


type Msg
    = Initialised Random.Seed -- The random seed is available and the game is now ready to start.
    | GotGameControlMsg UserGameControl.Msg -- User requested some actions, e.g. clicked arrow to move or rotate currently dropping shape.
    | TimerDropDelayElapsed Int -- Currently dropping shape should drop one row
    | HighlightAnimationMsg HighlightAnimation.Msg -- A message from the `HighlightAnimation` module as occurred: this is passed to that module to handle.


{-| Data returned from the `update` function detailing anything the calling module needs to know, e.g. whether the game
is now over or not.
-}
type UpdateResult
    = Continue ( Model, Cmd Msg )
    | GameOver (Game Shape.Bag)


update : Msg -> Model -> UpdateResult
update msg model =
    case ( model, msg ) of
        ( Initialising, Initialised seed ) ->
            Continue <| startNewGame seed

        ( _, Initialised _ ) ->
            Continue ( model, Cmd.none )

        ( Playing playingModel, GotGameControlMsg gameControlMsg ) ->
            handleGameControlMsg playingModel gameControlMsg

        ( _, GotGameControlMsg _ ) ->
            Continue ( model, Cmd.none )

        ( Playing playingModel, TimerDropDelayElapsed id ) ->
            if playingModel.timerDropMessageId == id then
                -- Reset highlightAnimation to Nothing here because no animation should continue across a timer drop delay.
                -- This also prevents a weird edge case: near the end of a game if there are two empty rows at the top
                -- and a new shape appears which is two shapes tall, the model is updated and one animation (whose type
                -- is ShapeLanding) is replaced by another of exactly the same type (since we replace one "landing" shape
                -- with another "landing" shape). If we don't reset it to Nothing here, the code eventually gets confused
                -- and thinks it's the same animation continuing, but actually it's two distinct ones.
                Game.timerDrop Shape.next playingModel.game
                    |> handleMoveResult { playingModel | highlightAnimation = Nothing } True

            else
                Continue ( model, Cmd.none )

        ( _, TimerDropDelayElapsed _ ) ->
            Continue ( model, Cmd.none )

        ( _, HighlightAnimationMsg highlightAnimationMsg ) ->
            handleAnimationMsg model highlightAnimationMsg


{-| Starts a new game after it's been initialised.
-}
startNewGame : Random.Seed -> ( Model, Cmd Msg )
startNewGame seed =
    let
        newGame =
            Shape.createShapeBag seed |> Game.new Shape.next

        playingModel =
            { game = newGame
            , timerDropDelay = 1000 -- TODO: hard-coded 1000 here - configurable? right value?
            , timerDropMessageId = 0
            , normalBlocks = Game.blocks newGame |> .normal
            , previewLandingBlocks = Game.previewLandingBlocks newGame
            , highlightAnimation = Nothing -- We know initially there is nothing highlighted.
            , nextAnimationId = HighlightAnimation.initialId
            , gameControl = UserGameControl.init
            }
    in
    ( Playing playingModel, timerDropDelayCmd playingModel )


{-| Handles a message from the `UserGameControl` module. Asks that module to handle that message, and receives an updated
model in return (which is then stored in this module's model), along with zero or more user actions to be executed.
Then asks the `Game` module to execute those actions (e.g. move the shape down and left, if those two keys are currently
being pressed down). Returns an `UpdateResult` that informs the parent module what it has to do.

If one of the user actions was to drop the shape down a row then this also increments the model's `timerDropMessageId`
and ensures that a new `Process.sleep` task is returned in the `UpdateResult` so that the next timer drop occurs x
milliseconds from now (where x = model.timerDropDelay).

-}
handleGameControlMsg : PlayingModel -> UserGameControl.Msg -> UpdateResult
handleGameControlMsg playingModel gameControlMsg =
    let
        ( nextGameControlModel, actionsToExecute ) =
            UserGameControl.update playingModel.gameControl gameControlMsg

        moveResult =
            Game.executeUserActions Shape.next actionsToExecute playingModel.game

        -- If a shape was dropped then reset the timer drop delay
        startNewTimerDropDelay =
            case moveResult of
                Game.Continue { shapeRowChanged } ->
                    shapeRowChanged

                _ ->
                    -- If there was no change, or if the game is no in `RowBeingRemoved` state (i.e. we're "flashing"
                    -- the row(s) about to be removed) then we don't want/need a timer drop delay.
                    False

        -- If we're starting a new timer drop delay the increment the `timerDropMessageId` so that when the current
        -- `Process.sleep` eventually returns we'll know to ignore it.
        newTimerDropSubscriptionId =
            if startNewTimerDropDelay then
                playingModel.timerDropMessageId + 1

            else
                playingModel.timerDropMessageId
    in
    moveResult
        |> handleMoveResult
            { playingModel
                | gameControl = nextGameControlModel
                , timerDropMessageId = newTimerDropSubscriptionId
            }
            startNewTimerDropDelay


{-| Gets the command which will sleep for `timerDropDelay` ms then cause the `TimerDropDelayElapsed` message to be invoked.
-}
timerDropDelayCmd : { a | timerDropDelay : Int, timerDropMessageId : Int } -> Cmd Msg
timerDropDelayCmd { timerDropDelay, timerDropMessageId } =
    Process.sleep (toFloat timerDropDelay)
        |> Task.perform (always <| TimerDropDelayElapsed timerDropMessageId)


{-| Handles the result of a movement in the game, namely updates the model with the new game. If required, initiates
the asynchronous generation of a new random shape (which is then added to the game's model later). Will also start a new
timer drop delay (by including a `Process.sleep` task in the returned `UpdateResult`) if required.
-}
handleMoveResult : PlayingModel -> Bool -> Game.MoveResult Shape.Bag -> UpdateResult
handleMoveResult currentPlayingModel startNewTimerDropDelay moveResult =
    let
        -- Gets the command that should be returned in the `UpdateResult`, namely `timerDropDelayCmd` (if required)
        updateResultCmd =
            if startNewTimerDropDelay then
                timerDropDelayCmd currentPlayingModel

            else
                Cmd.none
    in
    case moveResult of
        Game.NoChange ->
            Continue ( Playing currentPlayingModel, updateResultCmd )

        Game.Continue { game } ->
            let
                nextBlocks =
                    Game.blocks game

                playingModel =
                    { currentPlayingModel
                        | game = game
                        , normalBlocks = nextBlocks.normal
                        , previewLandingBlocks = Game.previewLandingBlocks game
                    }
            in
            case nextBlocks.highlighted of
                [] ->
                    -- No animation required as there are no highlighted blocks
                    Continue ( Playing { playingModel | highlightAnimation = Nothing }, updateResultCmd )

                _ ->
                    -- There are some blocks we need to animate, but this animation might already be in progress (e.g.
                    -- if a block is on the bottom row but the user moves it left/right - it can just continue its
                    -- current animation).
                    case playingModel.highlightAnimation of
                        Nothing ->
                            -- Currently nothing is animated, but now should be, so start a new animation
                            Continue
                                ( playingModel |> withNewAnimation nextBlocks.highlighted HighlightAnimation.ShapeLanding |> Playing
                                , updateResultCmd
                                )

                        Just currentAnimation ->
                            if HighlightAnimation.highlightAnimationType currentAnimation == HighlightAnimation.ShapeLanding then
                                -- Just continue this current animation, but update the blocks on it
                                Continue
                                    ( Playing
                                        { playingModel
                                            | highlightAnimation =
                                                Just <| HighlightAnimation.withBlocks nextBlocks.highlighted currentAnimation
                                        }
                                    , updateResultCmd
                                    )

                            else
                                -- The current animation is different from the one we want now: start a new animation
                                Continue
                                    ( playingModel |> withNewAnimation nextBlocks.highlighted HighlightAnimation.ShapeLanding |> Playing
                                    , updateResultCmd
                                    )

        Game.RowBeingRemoved { game } ->
            let
                nextBlocks =
                    Game.blocks game

                playingModel =
                    { currentPlayingModel
                        | game = game
                        , normalBlocks = nextBlocks.normal
                        , previewLandingBlocks = Game.previewLandingBlocks game
                    }
            in
            -- Don't call updateResultCmd here - if a row is being removed we _don't_ want a timer drop delay to start yet.
            Continue
                ( playingModel |> withNewAnimation nextBlocks.highlighted HighlightAnimation.RowRemoval |> Playing
                , Cmd.none
                )

        Game.GameOver { game } ->
            GameOver game


{-| Starts a new animation.
-}
withNewAnimation : List ( Coord, Shape.BlockColour ) -> HighlightAnimation.Type -> PlayingModel -> PlayingModel
withNewAnimation highlightedBlocks animationType playingModel =
    let
        animationModel =
            HighlightAnimation.startNewAnimation
                playingModel.nextAnimationId
                animationType
                (totalAnimationTimeForType animationType playingModel)
                highlightedBlocks
    in
    { playingModel
        | nextAnimationId = HighlightAnimation.nextAnimationId playingModel.nextAnimationId
        , highlightAnimation = Just animationModel
    }


{-| Calculates the total time use for an animation of the given type.
-}
totalAnimationTimeForType : HighlightAnimation.Type -> { a | timerDropDelay : Int } -> Int
totalAnimationTimeForType animationType { timerDropDelay } =
    case animationType of
        HighlightAnimation.ShapeLanding ->
            timerDropDelay

        HighlightAnimation.RowRemoval ->
            150


{-| Handles a message from the `HighlightAnimation` module. Passes the message to that module to handle then based on the
result from that function updates this module's model.
-}
handleAnimationMsg : Model -> HighlightAnimation.Msg -> UpdateResult
handleAnimationMsg model msg =
    case model of
        Playing playingModel ->
            case playingModel.highlightAnimation of
                Just currentAnimation ->
                    case HighlightAnimation.update msg currentAnimation of
                        HighlightAnimation.IgnoreMsg ->
                            Continue ( model, Cmd.none )

                        HighlightAnimation.Continue nextAnimationModel ->
                            Continue ( Playing { playingModel | highlightAnimation = Just nextAnimationModel }, Cmd.none )

                        HighlightAnimation.Complete ->
                            case HighlightAnimation.highlightAnimationType currentAnimation of
                                HighlightAnimation.ShapeLanding ->
                                    -- We've finished animating a shape landing: stop animating. Whenever the timerDrop
                                    -- subscription fires next the game will move on.
                                    Continue ( Playing { playingModel | highlightAnimation = Nothing }, Cmd.none )

                                HighlightAnimation.RowRemoval ->
                                    -- When we've finished animating rows about to be removed, call
                                    -- `onRowRemovalAnimationComplete` which will remove those rows from the board and
                                    -- return the game to its regular state.
                                    let
                                        nextGame =
                                            Game.onRowRemovalAnimationComplete playingModel.game
                                    in
                                    Continue
                                        ( Playing
                                            { playingModel
                                                | game = nextGame
                                                , timerDropDelay = max (playingModel.timerDropDelay - 10) 100
                                                , normalBlocks = (Game.blocks nextGame).normal
                                                , highlightAnimation = Nothing
                                            }
                                        , timerDropDelayCmd playingModel
                                        )

                Nothing ->
                    Continue ( model, Cmd.none )

        _ ->
            Continue ( model, Cmd.none )



-- VIEW


view : Model -> Element msg
view model =
    let
        screenSection content =
            Element.el [ Element.width <| Element.px 400, Element.alignTop ] content

        ( gameBlocks, gamePreviewLandingBlocks, animationModel ) =
            case model of
                Initialising ->
                    ( [], [], Nothing )

                Playing { normalBlocks, previewLandingBlocks, highlightAnimation, game } ->
                    ( BoardView.withOpacity 1 normalBlocks, previewLandingBlocks, highlightAnimation )
    in
    Element.row [] <|
        -- TODO: show scores
        [ screenSection <| holdShapeView model
        , screenSection <|
            Element.el [ Element.centerX ] <|
                BoardView.view boardViewConfig gameBlocks gamePreviewLandingBlocks animationModel
        , screenSection <| upcomingShapeView model
        ]


{-| Gets a view showing the upcoming shape in the game.
-}
upcomingShapeView : Model -> Element msg
upcomingShapeView model =
    let
        upcomingShape =
            case model of
                Initialising ->
                    Nothing

                Playing { game } ->
                    Just <| Game.upcomingShape game
    in
    shapePreview Element.alignLeft "Coming next..." upcomingShape


{-| Gets a view showing the upcoming shape in the game.
-}
holdShapeView : Model -> Element msg
holdShapeView model =
    let
        holdShape =
            case model of
                Initialising ->
                    Nothing

                Playing { game } ->
                    Game.holdShape game
    in
    shapePreview Element.alignRight "Hold" holdShape


{-| Gets a rectangle showing a preview of a shape (e.g. the next shape to drop, or the shape currently on hold).
-}
shapePreview : Element.Attribute msg -> String -> Maybe Shape -> Element msg
shapePreview align caption maybeShape =
    let
        blocks =
            case maybeShape of
                Just shape ->
                    Shape.clippedBlocks shape |> List.map (\coord -> { coord = coord, colour = Shape.data shape |> .colour, opacity = 1 })

                Nothing ->
                    []

        rowCount =
            blocks |> List.map (.coord >> Tuple.second) |> List.maximum |> Maybe.withDefault 0 |> (+) 1

        colCount =
            blocks |> List.map (.coord >> Tuple.first) |> List.maximum |> Maybe.withDefault 0 |> (+) 1
    in
    Element.column
        [ Element.padding 14
        , Element.spacing 20
        , Element.Background.color <| Element.rgb255 0 0 0
        , Element.height <| Element.px 140
        , Element.width <| Element.px 180
        , Element.Border.color <| Element.rgb255 100 100 100
        , Element.Border.width 2
        , Element.Border.rounded 20
        , Element.Border.glow (Element.rgb255 200 200 200) 0.2
        , align
        ]
        [ Element.el [ Element.centerX, Element.Font.color <| Element.rgb255 100 100 100, Element.Font.semiBold ] <|
            Element.text caption
        , Element.el [ Element.centerX, Element.centerY, Element.centerX ] <|
            BoardView.view
                { cellSize = cellSize, rowCount = rowCount, colCount = colCount, borderStyle = BoardView.None, showGridLines = False }
                blocks
                []
                Nothing
        ]


{-| The configuration required to render the game.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = cellSize
    , rowCount = GameBoard.rowCount
    , colCount = GameBoard.colCount
    , borderStyle = BoardView.Solid
    , showGridLines = True
    }


{-| The size of the cells in a normal game.
-}
cellSize : Int
cellSize =
    30



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialising ->
            Sub.none

        Playing { game, gameControl, timerDropDelay, timerDropMessageId, highlightAnimation } ->
            let
                animationSubscription =
                    highlightAnimation
                        |> Maybe.map
                            (\animation -> [ HighlightAnimation.subscriptions animation |> Sub.map HighlightAnimationMsg ])
                        |> Maybe.withDefault []
            in
            Sub.batch <|
                [ UserGameControl.subscriptions gameControl |> Sub.map GotGameControlMsg ]
                    ++ animationSubscription
