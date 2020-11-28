module Main exposing (main)

{-| This is the main module of the application. It renders the main UI (the instructions etc), delegating the rendering
of the actual game to `GameView`. It's responsible for passing user/timer events to the `Game` module for it to update
the game accordingly.
-}

import BoardView
import Browser
import Element exposing (Element)
import Element.Background
import Element.Input
import Game exposing (Game)
import Html exposing (Html)
import UIHelpers exposing (edges)
import UserGame
import WelcomeScreen



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
    ( Welcome, Cmd.none )



-- MODEL


type Model
    = Welcome -- No game being played - showing the user some welcome/introductory info
    | Playing UserGame.Model -- Game is currently being played
    | GameOver Game -- Game has ended


type Msg
    = StartGameRequested -- User asked to start the game
    | PlayingGameMsg UserGame.Msg



-- UPDATE


updateSubModel : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateSubModel subModelMapper subMsgMapper ( subModel, subCmd ) =
    ( subModelMapper subModel, Cmd.map subMsgMapper subCmd )


startNewGame =
    UserGame.init |> updateSubModel Playing PlayingGameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Welcome, StartGameRequested ) ->
            startNewGame

        ( GameOver _, StartGameRequested ) ->
            startNewGame

        ( _, StartGameRequested ) ->
            ( model, Cmd.none )

        ( Playing playingModel, PlayingGameMsg playingMsg ) ->
            case UserGame.update playingMsg playingModel of
                UserGame.Continue nextPlayingModel nextPlayingCmd ->
                    ( nextPlayingModel, nextPlayingCmd ) |> updateSubModel Playing PlayingGameMsg

                UserGame.GameOver game ->
                    ( GameOver game, Cmd.none )

        ( _, PlayingGameMsg _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        contents : Element Msg
        contents =
            case model of
                Welcome ->
                    WelcomeScreen.view backgroundColour StartGameRequested |> Element.el []

                Playing playingModel ->
                    UserGame.view playingModel |> Element.map PlayingGameMsg |> wrapBoardView

                GameOver game ->
                    -- TODO: the below assumes there are no highlighted blocks when the game ends, but the type system doesn't
                    -- currently guarantee that (Game.handleDroppingShapeLanded can result in GameOver even when its state is
                    -- RowRemovalGameState, even though it's not currently ever called like that). Revisit maybe.
                    Element.column []
                        [ BoardView.view UserGame.boardViewConfig (Game.blocks game).normal Nothing
                        , Element.text "TODO: Game over"
                        , startGameButton
                        ]
                        |> wrapBoardView
    in
    Element.layout
        [ Element.width Element.fill
        , Element.Background.color backgroundColour
        , Element.height Element.fill
        ]
    <|
        Element.column [ Element.centerX, Element.paddingEach { edges | top = 20 } ] [ contents ]


wrapBoardView : Element msg -> Element msg
wrapBoardView boardView =
    Element.column [ Element.paddingEach { edges | top = 25 } ] [ boardView ]


backgroundColour : Element.Color
backgroundColour =
    Element.rgb255 30 30 30


startGameButton : Element Msg
startGameButton =
    Element.Input.button [] { onPress = Just StartGameRequested, label = Element.text "Start game" }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing playingModel ->
            UserGame.subscriptions playingModel |> Sub.map PlayingGameMsg

        _ ->
            Sub.none
