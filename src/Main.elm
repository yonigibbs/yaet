module Main exposing (main)

{-| This is the main module of the application. It delegates control to a number of submodules and handles transitioning
between them. The main modules are:

  - `WelcomeScreen`: used when the value of this module's model is `Welcome`. Shows the Welcome screen to the user.
  - `UserGame`: used when the value of this module's model is `Playing`. Shows the actual game.
  - `GameOver`: used when the value of this module's model is `GameOver`. Shows the "Game Over" message to the user for
    a few seconds then moves back to the Welcome screen.

-}

import Browser
import Element exposing (Element)
import Element.Background
import GameOver
import HighScores exposing (HighScores)
import Html exposing (Html)
import Json.Encode as JE
import Settings exposing (Settings)
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


{-| The flags passed into the application from the hosting JS. Contains the `settings` (which define such things as the
key bindings, and the stored high scores).
-}
type alias Flags =
    { settings : JE.Value, highScores : JE.Value }


init : Flags -> ( Model, Cmd Msg )
init { settings, highScores } =
    initAtWelcomeScreen (HighScores.fromJson highScores) (Settings.fromJson settings)


initAtWelcomeScreen : HighScores -> Settings -> ( Model, Cmd Msg )
initAtWelcomeScreen highScores settings =
    let
        ( subModel, subCmd ) =
            WelcomeScreen.init settings
    in
    ( Welcome { model = subModel, highScores = highScores }
    , Cmd.map GotWelcomeScreenMsg subCmd
    )



-- MODEL


{-| The model for this app. There is some state which is persisted in local storage (namely the high scores and settings).
This is read in at the start of the app, then retained in memory thereafter for the duration of the app. In order to retain
it, we keep that data against every variant below as a separate field (and don't pass it into the models of the variants
that don't need that data). For variants whose associated model _does_ need access to that data, the variant's model
(e.g. `WelcomeScreen.Model`) stores it, and we then don't store it as a separate field.
-}
type Model
    = Welcome { model : WelcomeScreen.Model, highScores : HighScores } -- No game being played - showing the user some welcome/introductory info.
    | Playing { model : UserGame.Model, highScores : HighScores } -- Game is currently being played
    | GameOver { model : GameOver.Model, settings : Settings } -- Game has ended


type Msg
    = GotWelcomeScreenMsg WelcomeScreen.Msg
    | GotPlayingGameMsg UserGame.Msg
    | GotGameOverMsg GameOver.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Welcome welcome, GotWelcomeScreenMsg welcomeMsg ) ->
            handleWelcomeScreenMsg welcomeMsg welcome

        ( _, GotWelcomeScreenMsg _ ) ->
            ( model, Cmd.none )

        ( Playing playing, GotPlayingGameMsg playingMsg ) ->
            handlePlayingGameMsg playingMsg playing

        ( _, GotPlayingGameMsg _ ) ->
            ( model, Cmd.none )

        ( GameOver gameOver, GotGameOverMsg gameOverMsg ) ->
            handleGameOverMsg gameOverMsg gameOver

        ( _, GotGameOverMsg _ ) ->
            ( model, Cmd.none )


handleWelcomeScreenMsg : WelcomeScreen.Msg -> { model : WelcomeScreen.Model, highScores : HighScores } -> ( Model, Cmd Msg )
handleWelcomeScreenMsg welcomeMsg welcome =
    let
        ( welcomeModel, welcomeCmd, welcomeUpdateResult ) =
            WelcomeScreen.update welcomeMsg welcome.model
    in
    case welcomeUpdateResult of
        WelcomeScreen.Stay ->
            ( Welcome { welcome | model = welcomeModel }
            , Cmd.map GotWelcomeScreenMsg welcomeCmd
            )

        WelcomeScreen.StartGame ->
            WelcomeScreen.getSettings welcomeModel
                |> UserGame.init
                |> (\( gameModel, gameCmd ) ->
                        ( Playing { model = gameModel, highScores = welcome.highScores }
                        , Cmd.map GotPlayingGameMsg gameCmd
                        )
                   )


handlePlayingGameMsg : UserGame.Msg -> { model : UserGame.Model, highScores : HighScores } -> ( Model, Cmd Msg )
handlePlayingGameMsg gameMsg playing =
    case UserGame.update gameMsg playing.model of
        UserGame.Continue ( subModel, subCmd ) ->
            ( Playing { playing | model = subModel }
            , Cmd.map GotPlayingGameMsg subCmd
            )

        UserGame.GameOver game ->
            ( GameOver { model = GameOver.init playing.highScores game, settings = UserGame.getSettings playing.model }
            , Cmd.none
            )


handleGameOverMsg : GameOver.Msg -> { model : GameOver.Model, settings : Settings } -> ( Model, Cmd Msg )
handleGameOverMsg gameOverMsg gameOver =
    case GameOver.update gameOverMsg gameOver.model of
        GameOver.Continue subModel ->
            ( GameOver { gameOver | model = subModel }, Cmd.none )

        GameOver.Done ->
            initAtWelcomeScreen (GameOver.getHighScores gameOver.model) gameOver.settings



-- VIEW


view : Model -> Html Msg
view model =
    let
        contents : Element Msg
        contents =
            case model of
                Welcome welcome ->
                    WelcomeScreen.view welcome.model |> Element.map GotWelcomeScreenMsg

                Playing playing ->
                    UserGame.view playing.model |> Element.map GotPlayingGameMsg |> wrapBoardView

                GameOver gameOver ->
                    -- TODO: the below assumes there are no highlighted blocks when the game ends, but the type system doesn't
                    -- currently guarantee that (Game.handleDroppingShapeLanded can result in GameOver even when its state is
                    -- RowRemovalGameState, even though it's not currently ever called like that). Revisit maybe.
                    GameOver.view gameOver.model |> wrapBoardView
    in
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Just UIHelpers.mainForegroundColour
                , backgroundColor = Nothing
                , shadow =
                    Just
                        { color = Element.rgba255 198 195 195 0.6
                        , offset = ( 0, 0 )
                        , blur = 3
                        , size = 2
                        }
                }
            ]
        }
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color UIHelpers.mainBackgroundColour
        , Element.scrollbarY
        ]
        contents


wrapBoardView : Element msg -> Element msg
wrapBoardView boardView =
    Element.el [ Element.centerX, Element.paddingEach { edges | top = 25 } ] boardView


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Welcome welcome ->
            WelcomeScreen.subscriptions welcome.model |> Sub.map GotWelcomeScreenMsg

        Playing playing ->
            UserGame.subscriptions playing.model |> Sub.map GotPlayingGameMsg

        GameOver _ ->
            GameOver.subscriptions |> Sub.map GotGameOverMsg
