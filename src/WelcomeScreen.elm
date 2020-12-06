module WelcomeScreen exposing (Model, Msg, init, subscriptions, update, view)

import BlockColour exposing (BlockColour)
import BoardView
import Coord exposing (Coord)
import Element exposing (Element)
import HighlightAnimation
import Random
import RandomShapeGenerator
import Shape exposing (Shape)
import Time
import UIHelpers



-- MODEL


type alias Letter =
    { blocks : List Coord, colour : BlockColour, gridCoord : Coord }


type Model
    = DroppingLetters { dropped : List Letter, dropping : Letter, next : List Letter }
    | PulsingLetters { letters : List Letter, animation : HighlightAnimation.Model }
      -- TODO: droppingShape below is same as DroppingShape alias in Game. Put somewhere common and reuse definition here?
    | DroppingRandomShapes { letters : List Letter, droppingShape : Maybe { shape : Shape, gridCoord : Coord } }


init : Model
init =
    DroppingLetters
        { dropped = []
        , dropping = { blocks = tBlocks, colour = BlockColour.Blue, gridCoord = ( 35, initialLetterYCoord ) }
        , next =
            [ { blocks = eBlocks, colour = BlockColour.Red, gridCoord = ( 41, initialLetterYCoord ) }
            , { blocks = tBlocks, colour = BlockColour.Orange, gridCoord = ( 46, initialLetterYCoord ) }
            , { blocks = rBlocks, colour = BlockColour.Yellow, gridCoord = ( 52, initialLetterYCoord ) }
            , { blocks = iBlocks, colour = BlockColour.Purple, gridCoord = ( 57, initialLetterYCoord ) }
            , { blocks = sBlocks, colour = BlockColour.Green, gridCoord = ( 59, initialLetterYCoord ) }
            ]
        }


initialLetterYCoord : Int
initialLetterYCoord =
    20


lastLetterYCoord : Int
lastLetterYCoord =
    7



-- UPDATE


type Msg
    = ProgressLetterDropAnimationRequested
    | GotHighlightAnimationMsg HighlightAnimation.Msg
    | RandomShapeGenerated { shape : Shape, xCoord : Int }
    | ShapeDropDelayElapsed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ProgressLetterDropAnimationRequested, DroppingLetters data ) ->
            ( progressLetterDropAnimation data, Cmd.none )

        ( ProgressLetterDropAnimationRequested, _ ) ->
            ( model, Cmd.none )

        ( GotHighlightAnimationMsg highlightAnimationMsg, PulsingLetters data ) ->
            progressPulsingLettersAnimation model highlightAnimationMsg data

        ( GotHighlightAnimationMsg _, _ ) ->
            ( model, Cmd.none )

        ( RandomShapeGenerated { shape, xCoord }, DroppingRandomShapes data ) ->
            -- TODO: rename initialLetterYCoord to be less letter-specific? Also used for name below
            ( DroppingRandomShapes
                { data | droppingShape = Just { shape = shape, gridCoord = ( xCoord, initialLetterYCoord ) } }
            , Cmd.none
            )

        ( RandomShapeGenerated _, _ ) ->
            ( model, Cmd.none )

        ( ShapeDropDelayElapsed, DroppingRandomShapes data ) ->
            handleShapeDropDelayElapsed data

        ( ShapeDropDelayElapsed, _ ) ->
            ( model, Cmd.none )


progressLetterDropAnimation : { dropped : List Letter, dropping : Letter, next : List Letter } -> Model
progressLetterDropAnimation { dropped, dropping, next } =
    let
        ( gridX, gridY ) =
            dropping.gridCoord
    in
    if gridY == lastLetterYCoord then
        -- The currently dropping letter has reached the bottom - start the next letter
        case next of
            nextLetter :: restLetters ->
                -- We have more letters to drop
                DroppingLetters { dropped = dropping :: dropped, dropping = nextLetter, next = restLetters }

            [] ->
                -- All letters now dropped
                let
                    letters =
                        dropping :: dropped
                in
                PulsingLetters
                    { letters = letters
                    , animation =
                        HighlightAnimation.startNewAnimation HighlightAnimation.initialId
                            HighlightAnimation.ShapeLanding
                            1000
                            (lettersToBoardBlocks letters)
                    }

    else
        -- The currently dropping letter can drop one more row
        DroppingLetters { dropped = dropped, dropping = { dropping | gridCoord = ( gridX, gridY - 1 ) }, next = next }


progressPulsingLettersAnimation : Model -> HighlightAnimation.Msg -> { letters : List Letter, animation : HighlightAnimation.Model } -> ( Model, Cmd Msg )
progressPulsingLettersAnimation model msg pulsingLettersData =
    case HighlightAnimation.update msg pulsingLettersData.animation of
        HighlightAnimation.IgnoreMsg ->
            ( model, Cmd.none )

        HighlightAnimation.Continue nextAnimationModel ->
            ( PulsingLetters { pulsingLettersData | animation = nextAnimationModel }, Cmd.none )

        HighlightAnimation.Complete ->
            ( DroppingRandomShapes { letters = pulsingLettersData.letters, droppingShape = Nothing }, generateRandomShape )


handleShapeDropDelayElapsed : { letters : List Letter, droppingShape : Maybe { shape : Shape, gridCoord : Coord } } -> ( Model, Cmd Msg )
handleShapeDropDelayElapsed data =
    case data.droppingShape of
        Just droppingShape ->
            let
                ( x, y ) =
                    droppingShape.gridCoord
            in
            if y == 0 then
                -- Reached the bottom - kill this shape and request another.
                ( DroppingRandomShapes { data | droppingShape = Nothing }, generateRandomShape )

            else
                ( DroppingRandomShapes
                    { data | droppingShape = Just { shape = droppingShape.shape, gridCoord = ( x, y - 1 ) } }
                , Cmd.none
                )

        Nothing ->
            -- We don't have a shape so can't drop anything
            ( DroppingRandomShapes data, Cmd.none )


generateRandomShape : Cmd Msg
generateRandomShape =
    Random.map2 (\shape xCoord -> { shape = shape, xCoord = xCoord })
        RandomShapeGenerator.generator
        (Random.int 20 60)
        |> Random.generate RandomShapeGenerated



-- VIEW


view : Model -> msg -> Element msg
view model startGameMsg =
    let
        ( letters_, animation_, droppingShapeBlocks ) =
            case model of
                DroppingLetters { dropped, dropping } ->
                    ( dropping :: dropped, Nothing, [] )

                PulsingLetters { animation } ->
                    ( [], Just animation, [] )

                DroppingRandomShapes { letters, droppingShape } ->
                    ( letters, Nothing, Maybe.map droppingShapeToBoardBlocks droppingShape |> Maybe.withDefault [] )
    in
    Element.column [ Element.spacingXY 0 25 ]
        [ BoardView.view boardViewConfig (droppingShapeBlocks ++ lettersToBoardBlocks letters_) animation_
        , Element.row [ Element.centerX ] [ UIHelpers.button "Start Game" startGameMsg ]
        ]


{-| The configuration required to render the board in the welcome screen.
-}
boardViewConfig : BoardView.Config
boardViewConfig =
    { cellSize = 15, rowCount = 20, colCount = 100, borderStyle = BoardView.Fade UIHelpers.mainBackgroundColour }


lettersToBoardBlocks : List Letter -> List ( Coord, BlockColour )
lettersToBoardBlocks =
    List.map letterToBoardBlocks >> List.concat


letterToBoardBlocks : Letter -> List ( Coord, BlockColour )
letterToBoardBlocks { blocks, colour, gridCoord } =
    let
        ( gridX, gridY ) =
            gridCoord
    in
    -- TODO: this is v similar to Game.calcShapeBlocksBoardCoords - poss create shared module for stuff related to drawing blocks on the board?
    -- There's more in this module that's similar, e.g. dropping a shape one row, etc. Could also go into a shared module.
    blocks
        |> List.map (\( x, y ) -> ( x + gridX, y + gridY ))
        |> List.map (\coord -> ( coord, colour ))


droppingShapeToBoardBlocks : { shape : Shape, gridCoord : Coord } -> List ( Coord, BlockColour )
droppingShapeToBoardBlocks { shape, gridCoord } =
    -- TODO: is this just a duplicate of Game.calcShapeBlocksBoardCoords? Put somewhere common.
    let
        ( shapeX, shapeY ) =
            gridCoord

        { blocks, colour } =
            Shape.data shape
    in
    blocks
        |> List.map (\( x, y ) -> ( ( x + shapeX, y + shapeY ), colour ))


shapeDropDelayMs : Int
shapeDropDelayMs =
    500



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        DroppingLetters _ ->
            Time.every 50 <| always ProgressLetterDropAnimationRequested

        PulsingLetters { animation } ->
            HighlightAnimation.subscriptions animation |> Sub.map GotHighlightAnimationMsg

        DroppingRandomShapes { droppingShape } ->
            case droppingShape of
                Nothing ->
                    Sub.none

                Just _ ->
                    Time.every (toFloat shapeDropDelayMs) <| always ShapeDropDelayElapsed



-- LETTERS


tBlocks : List Coord
tBlocks =
    [ ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 4, 6 ), ( 2, 6 ), ( 2, 5 ), ( 2, 4 ), ( 2, 3 ), ( 2, 2 ), ( 2, 1 ), ( 2, 0 ) ]


eBlocks : List Coord
eBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 6 ), ( 1, 3 ), ( 2, 3 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]


rBlocks : List Coord
rBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ), ( 1, 6 ), ( 2, 6 ), ( 3, 5 ), ( 3, 4 ), ( 2, 3 ), ( 1, 3 ), ( 1, 2 ), ( 2, 1 ), ( 3, 0 ) ]


iBlocks : List Coord
iBlocks =
    [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 0, 6 ) ]


sBlocks : List Coord
sBlocks =
    [ ( 0, 1 ), ( 1, 0 ), ( 2, 0 ), ( 3, 1 ), ( 3, 2 ), ( 2, 3 ), ( 1, 3 ), ( 0, 4 ), ( 0, 5 ), ( 1, 6 ), ( 2, 6 ), ( 3, 5 ) ]
