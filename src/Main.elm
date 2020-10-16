module Main exposing (main)

import Array exposing (Array)
import Block
import Browser
import Game exposing (Game)
import Html exposing (Html)
import Html.Events
import Random
import Shape exposing (Shape)



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
    | NextShapeGenerated Shape


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Instructions, StartGameRequested ) ->
            startNewGame

        ( Ended, StartGameRequested ) ->
            startNewGame

        ( _, StartGameRequested ) ->
            ( model, Cmd.none )

        ( Initialising, NextShapeGenerated shape ) ->
            ( Playing <| Game.new shape, Cmd.none )

        ( Playing game, NextShapeGenerated shape ) ->
            ( Playing <| Game.nextShapeGenerated game shape, Cmd.none )

        ( _, NextShapeGenerated _ ) ->
            ( model, Cmd.none )


startNewGame : ( Model, Cmd Msg )
startNewGame =
    ( Initialising, Random.generate NextShapeGenerated generateRandomShape )


{-| All the possible colours, in an array so that one can be randomly chosen from it.
-}
allColours : Array Block.Colour
allColours =
    Array.fromList [ Block.Blue, Block.Red, Block.Orange, Block.Yellow, Block.Purple ]


{-| Functions for generating each of the possible colours, in an array so that one can be randomly chosen from it.
-}
allShapeBuilders : Array Shape.ShapeBuilder
allShapeBuilders =
    let
        ( first, rest ) =
            Shape.builders
    in
    Array.fromList (first :: rest)


generateRandomColour : Random.Generator Block.Colour
generateRandomColour =
    Random.int 0 (Array.length allColours - 1)
        |> Random.map (\index -> Array.get index allColours |> Maybe.withDefault Block.Blue)


generateRandomShapeBuilder : Random.Generator Shape.ShapeBuilder
generateRandomShapeBuilder =
    Random.int 0 (Array.length allShapeBuilders - 1)
        |> Random.map (\index -> Array.get index allShapeBuilders |> Maybe.withDefault (Tuple.first Shape.builders))


{-| Generates the next random shape.
-}
generateRandomShape : Random.Generator Shape
generateRandomShape =
    Random.pair generateRandomColour generateRandomShapeBuilder
        |> Random.map (\( colour, shapeBuilder ) -> shapeBuilder colour)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Instructions ->
            Html.div [] [ Html.text "TODO: Instructions", Html.button [ Html.Events.onClick StartGameRequested ] [ Html.text "Start game" ] ]

        Initialising ->
            Html.text "TODO: Initialising"

        Playing game ->
            Html.text "TODO: Game"

        Ended ->
            Html.text "TODO: Game ended"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
