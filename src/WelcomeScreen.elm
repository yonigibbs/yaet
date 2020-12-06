module WelcomeScreen exposing (Model, Msg, init, subscriptions, update, view)

import BlockColour exposing (BlockColour)
import BoardView
import Browser.Events
import Coord exposing (Coord)
import Element exposing (Element)
import HighlightAnimation
import Time
import UIHelpers



-- MODEL


type alias Letter =
    { blocks : List Coord, colour : BlockColour, gridCoord : Coord }


type Model
    = DroppingLetters { dropped : List Letter, dropping : Letter, next : List Letter }
    | Pulse
        { letters : List Letter

        -- , animation : HighlightAnimation.Model
        }


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


initialLetterYCoord =
    20


lastLetterYCoord =
    7



-- UPDATE


type Msg
    = ProgressLetterDropAnimationRequested


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( ProgressLetterDropAnimationRequested, DroppingLetters data ) ->
            progressLetterDropAnimation data

        ( ProgressLetterDropAnimationRequested, _ ) ->
            -- TODO: implement pulse animation
            model


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
                -- TODO: the 1000 below is same value as in UserGame.startNewGame - create common constant somewhere?
                let
                    letters =
                        dropping :: dropped
                in
                Pulse
                    { letters = letters

                    --, animation =
                    --    HighlightAnimation.startNewAnimation HighlightAnimation.initialId
                    --        HighlightAnimation.ShapeLanding
                    --        1000
                    --        (lettersToBoardBlocks letters)
                    }

    else
        -- The currently dropping letter can drop one more row
        DroppingLetters { dropped = dropped, dropping = { dropping | gridCoord = ( gridX, gridY - 1 ) }, next = next }



-- VIEW


view : Model -> msg -> Element msg
view model startGameMsg =
    let
        letters_ =
            case model of
                DroppingLetters { dropped, dropping } ->
                    dropping :: dropped

                Pulse { letters } ->
                    letters
    in
    Element.column [ Element.spacingXY 0 25 ]
        [ BoardView.view boardViewConfig (lettersToBoardBlocks letters_) Nothing
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        DroppingLetters _ ->
            Time.every 50 <| always ProgressLetterDropAnimationRequested

        _ ->
            Sub.none



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
