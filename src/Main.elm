module Main exposing (main)

import Array exposing (Array)
import Block exposing (BlockColour)
import Browser
import Browser.Events
import Game exposing (Game)
import GameRender
import Html exposing (Html)
import Html.Events
import Keyboard
import Random
import Shape exposing (Shape)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Instructions, Cmd.none )



-- MODEL


type Model
    = Instructions
    | Initialising
    | Playing Game
    | Ended



-- UPDATE


type Msg
    = StartGameRequested
    | Initialised Game.InitialisationInfo
    | RandomShapeGenerated Shape
    | MoveShapeRequested Game.Direction
    | RotateShapeRequested Shape.RotationDirection
    | TimerDropDelayElapsed


keyMessages : Keyboard.KeyMessages Msg
keyMessages =
    { moveLeft = MoveShapeRequested Game.Left
    , moveRight = MoveShapeRequested Game.Right
    , dropOneRow = MoveShapeRequested Game.Down
    , rotateClockwise = RotateShapeRequested Shape.Clockwise
    , rotateAnticlockwise = RotateShapeRequested Shape.Anticlockwise
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Instructions, StartGameRequested ) ->
            initialiseGame

        ( Ended, StartGameRequested ) ->
            initialiseGame

        ( _, StartGameRequested ) ->
            ( model, Cmd.none )

        ( Initialising, Initialised initialisationInfo ) ->
            ( Playing <| Game.new initialisationInfo, Cmd.none )

        ( _, Initialised _ ) ->
            ( model, Cmd.none )

        ( Playing game, RandomShapeGenerated shape ) ->
            ( Playing <| Game.shapeGenerated game shape, Cmd.none )

        ( _, RandomShapeGenerated _ ) ->
            ( model, Cmd.none )

        ( Playing game, MoveShapeRequested direction ) ->
            ( Playing <| Game.moveShape game direction, Cmd.none )

        ( _, MoveShapeRequested _ ) ->
            ( model, Cmd.none )

        ( Playing game, RotateShapeRequested direction ) ->
            ( Playing <| Game.rotateShape game direction, Cmd.none )

        ( _, RotateShapeRequested _ ) ->
            ( model, Cmd.none )

        ( Playing game, TimerDropDelayElapsed ) ->
            let
                ( nextGame, needNewRandomShape ) =
                    Game.timerDrop game

                nextCmd =
                    if needNewRandomShape then
                        generateRandomShape

                    else
                        Cmd.none
            in
            ( Playing nextGame, nextCmd )

        ( _, TimerDropDelayElapsed ) ->
            ( model, Cmd.none )


initialiseGame : ( Model, Cmd Msg )
initialiseGame =
    ( Initialising, Random.generate Initialised initialGameDataGenerator )


{-| All the possible colours, in an array so that one can be randomly chosen from it.
-}
allColours : Array BlockColour
allColours =
    Array.fromList [ Block.Blue, Block.Red, Block.Orange, Block.Yellow, Block.Purple, Block.Green ]


{-| Functions for generating each of the possible colours, in an array so that one can be randomly chosen from it.
-}
allShapeBuilders : Array Shape.ShapeBuilder
allShapeBuilders =
    let
        ( first, rest ) =
            Shape.builders
    in
    Array.fromList (first :: rest)


randomColourGenerator : Random.Generator BlockColour
randomColourGenerator =
    Random.int 0 (Array.length allColours - 1)
        |> Random.map (\index -> Array.get index allColours |> Maybe.withDefault Block.Blue)


randomShapeBuilderGenerator : Random.Generator Shape.ShapeBuilder
randomShapeBuilderGenerator =
    Random.int 0 (Array.length allShapeBuilders - 1)
        |> Random.map (\index -> Array.get index allShapeBuilders |> Maybe.withDefault (Tuple.first Shape.builders))


randomShapeGenerator : Random.Generator Shape
randomShapeGenerator =
    Random.pair randomColourGenerator randomShapeBuilderGenerator
        |> Random.map (\( colour, shapeBuilder ) -> shapeBuilder colour)


shapeBufferGenerator : Random.Generator (List Shape)
shapeBufferGenerator =
    Random.map5
        (\s1 s2 s3 s4 s5 -> [ s1, s2, s3, s4, s5 ])
        randomShapeGenerator
        randomShapeGenerator
        randomShapeGenerator
        randomShapeGenerator
        randomShapeGenerator


initialGameDataGenerator : Random.Generator Game.InitialisationInfo
initialGameDataGenerator =
    Random.map4 Game.InitialisationInfo randomShapeGenerator randomShapeGenerator randomShapeGenerator shapeBufferGenerator


generateRandomShape : Cmd Msg
generateRandomShape =
    Random.generate RandomShapeGenerated randomShapeGenerator



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Instructions ->
            Html.div [] [ Html.text "TODO: Instructions", Html.button [ Html.Events.onClick StartGameRequested ] [ Html.text "Start game" ] ]

        Initialising ->
            Html.text "TODO: Initialising"

        Playing game ->
            GameRender.render game

        Ended ->
            Html.text "TODO: Game ended"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Instructions ->
            Sub.none

        Initialising ->
            Sub.none

        Playing game ->
            Sub.batch
                [ Time.every (Game.timerDropDelay game |> toFloat) <| always TimerDropDelayElapsed
                , Browser.Events.onKeyDown <| Keyboard.keyEventDecoder keyMessages
                ]

        Ended ->
            Sub.none
