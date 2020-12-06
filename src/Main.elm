module Main exposing (main)

{-| This is the main module of the application. It renders the main UI (the instructions etc), delegating the rendering
of the actual game to `GameView`. It's responsible for passing user/timer events to the `Game` module for it to update
the game accordingly.
-}

import Browser
import Element exposing (Element)
import Element.Background
import GameOver
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
    -- TODO: remove this temp code - useful when testing the Game Over UI
    --let
    --    newGame =
    --        Game.new
    --            { initialShape = BlockColour.Blue |> (Shape.builders |> Tuple.first)
    --            , nextShape = BlockColour.Red |> (Shape.builders |> Tuple.first)
    --            , shapeBuffer = []
    --            }
    --in
    --( GameOver <| GameOver.init newGame, Cmd.none )
    ( Welcome WelcomeScreen.init, Cmd.none )



-- MODEL


type Model
    = Welcome WelcomeScreen.Model -- No game being played - showing the user some welcome/introductory info
    | Playing UserGame.Model -- Game is currently being played
    | GameOver GameOver.Model -- Game has ended


type Msg
    = StartGameRequested -- User asked to start the game
    | GotWelcomeScreenMsg WelcomeScreen.Msg
    | GotPlayingGameMsg UserGame.Msg
    | GotGameOverMsg GameOver.Msg



-- UPDATE


updateSubModel : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateSubModel subModelMapper subMsgMapper ( subModel, subCmd ) =
    ( subModelMapper subModel, Cmd.map subMsgMapper subCmd )


startNewGame =
    UserGame.init |> updateSubModel Playing GotPlayingGameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Welcome _, StartGameRequested ) ->
            startNewGame

        ( _, StartGameRequested ) ->
            ( model, Cmd.none )

        ( Welcome welcomeModel, GotWelcomeScreenMsg welcomeScreenMsg ) ->
            ( Welcome <| WelcomeScreen.update welcomeScreenMsg welcomeModel, Cmd.none )

        ( _, GotWelcomeScreenMsg _ ) ->
            ( model, Cmd.none )

        ( Playing playingModel, GotPlayingGameMsg playingMsg ) ->
            case UserGame.update playingMsg playingModel of
                UserGame.Continue nextPlayingModel nextPlayingCmd ->
                    ( nextPlayingModel, nextPlayingCmd ) |> updateSubModel Playing GotPlayingGameMsg

                UserGame.GameOver game ->
                    ( GameOver <| GameOver.init game, Cmd.none )

        ( _, GotPlayingGameMsg _ ) ->
            ( model, Cmd.none )

        ( GameOver gameOverModel, GotGameOverMsg gameOverMsg ) ->
            case GameOver.update gameOverMsg gameOverModel of
                GameOver.Continue nextGameOverModel ->
                    ( GameOver nextGameOverModel, Cmd.none )

                GameOver.Done ->
                    ( Welcome WelcomeScreen.init, Cmd.none )

        ( _, GotGameOverMsg _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        contents : Element Msg
        contents =
            case model of
                Welcome welcomeModel ->
                    WelcomeScreen.view welcomeModel StartGameRequested |> Element.el []

                Playing playingModel ->
                    UserGame.view playingModel |> Element.map GotPlayingGameMsg |> wrapBoardView

                GameOver game ->
                    -- TODO: the below assumes there are no highlighted blocks when the game ends, but the type system doesn't
                    -- currently guarantee that (Game.handleDroppingShapeLanded can result in GameOver even when its state is
                    -- RowRemovalGameState, even though it's not currently ever called like that). Revisit maybe.
                    GameOver.view game |> wrapBoardView
    in
    Element.layout
        [ Element.width Element.fill
        , Element.Background.color UIHelpers.mainBackgroundColour
        , Element.height Element.fill
        ]
    <|
        Element.column [ Element.centerX, Element.paddingEach { edges | top = 20 } ] [ contents ]


wrapBoardView : Element msg -> Element msg
wrapBoardView boardView =
    Element.column [ Element.paddingEach { edges | top = 25 } ] [ boardView ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Welcome welcomeModel ->
            WelcomeScreen.subscriptions welcomeModel |> Sub.map GotWelcomeScreenMsg

        Playing playingModel ->
            UserGame.subscriptions playingModel |> Sub.map GotPlayingGameMsg

        GameOver _ ->
            GameOver.subscriptions |> Sub.map GotGameOverMsg
