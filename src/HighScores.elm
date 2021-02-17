module HighScores exposing
    ( HighScores
    , HighScoresModel
    , HighScoresMsg
    , HighScoresUpdateResult(..)
    , NewHighScoreModel
    , NewHighScoreMsg
    , NewHighScoreUpdateResult(..)
    , fromJson
    , highScoresDialogSubscriptions
    , highScoresView
    , initHighScoresDialog
    , initNewHighScoreDialog
    , newHighScoreDialogSubscriptions
    , newHighScoreView
    , updateHighScoresDialog
    , updateNewHighScoreDialog
    )

-- TODO: document this when all implemented

import Browser.Dom
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input
import Html.Attributes
import Json.Decode as JD
import Json.Encode as JE
import Modal
import Ports
import Task



-- HIGH SCORES


type HighScores
    = HighScores (List Entry)


type alias Entry =
    { name : String, score : Int }


empty : HighScores
empty =
    HighScores []


isEmpty : HighScores -> Bool
isEmpty (HighScores entries) =
    List.isEmpty entries



-- JSON


fromJson : JE.Value -> HighScores
fromJson json =
    json |> JD.decodeValue (JD.list entryDecoder) |> Result.map fromEntries |> Result.withDefault empty


entryDecoder : JD.Decoder Entry
entryDecoder =
    JD.map2 Entry (JD.field "name" JD.string) (JD.field "score" JD.int)


toJson : HighScores -> JE.Value
toJson (HighScores entries) =
    entries |> JE.list encodeEntry


encodeEntry : Entry -> JE.Value
encodeEntry { name, score } =
    JE.object [ ( "name", JE.string name ), ( "score", JE.int score ) ]


fromEntries : List Entry -> HighScores
fromEntries entries =
    entries |> List.take maxItems |> List.sortBy .score |> List.reverse |> HighScores


maxItems : Int
maxItems =
    5



-- FUNCTIONS SHARED BETWEEN HIGH-SCORES-VIEW AND NEW-HIGH-SCORE-VIEW


view : String -> Modal.Config msg -> List (Element msg) -> Element msg
view caption modalDialogConfig rows =
    Element.column
        [ Element.spacing 15
        , Element.centerX
        , Element.width <| Element.px 240
        , Element.Font.color <| Element.rgb255 200 200 200
        ]
        [ Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24 ] <| Element.text caption
        , Element.column [ Element.spacingXY 0 5, Element.Font.size 18, Element.width Element.fill ] rows
        ]
        |> Modal.dialog modalDialogConfig


emptyHighScoreRow : Int -> Element msg
emptyHighScoreRow index =
    highScoreRow index Element.none Element.none


highScoreRow : Int -> Element msg -> Element msg -> Element msg
highScoreRow index nameElement scoreElement =
    Element.row [ Element.width Element.fill, Element.spacing 5, Element.height <| Element.px 25 ]
        [ index + 1 |> String.fromInt |> Element.text |> Element.el [ Element.alignLeft, Element.width <| Element.px 15 ]
        , Element.el [ Element.alignLeft, Element.width <| Element.px 150, Element.clip ] nameElement
        , Element.el [ Element.alignRight ] scoreElement
        ]


existingHighScoreRow : Int -> Entry -> Element msg
existingHighScoreRow index { name, score } =
    highScoreRow index (Element.text name) (populatedScoreElement score)


populatedScoreElement : Int -> Element msg
populatedScoreElement score =
    Element.text <| String.fromInt score


appendEmptyEntries : (Int -> a) -> List a -> List a
appendEmptyEntries emptyEntryMapper list =
    List.range (List.length list) (maxItems - 1)
        |> List.map emptyEntryMapper
        |> (++) list



-- HIGH SCORES VIEW


type HighScoresModel
    = EmptyHighScores
    | PopulatedHighScores HighScores
    | ResetHighScores


type HighScoresMsg
    = HighScoresResetRequested
    | HighScoresSubmitted
    | HighScoresCancelled


initHighScoresDialog : HighScores -> HighScoresModel
initHighScoresDialog highScores =
    if isEmpty highScores then
        EmptyHighScores

    else
        PopulatedHighScores highScores


type HighScoresUpdateResult
    = KeepOpen_ HighScoresModel
    | Close_ (Maybe ( HighScores, Cmd HighScoresMsg ))


updateHighScoresDialog : HighScoresMsg -> HighScoresModel -> HighScoresUpdateResult
updateHighScoresDialog msg model =
    case ( msg, model ) of
        ( HighScoresResetRequested, _ ) ->
            KeepOpen_ ResetHighScores

        ( HighScoresSubmitted, ResetHighScores ) ->
            Close_ <| Just ( empty, empty |> toJson |> Ports.persistHighScores )

        ( HighScoresSubmitted, _ ) ->
            -- UI shouldn't allow user to submit the dialog if the high scores haven't been reset
            Close_ Nothing

        ( HighScoresCancelled, _ ) ->
            Close_ Nothing


highScoresView : HighScoresModel -> Element HighScoresMsg
highScoresView model =
    let
        entries =
            case model of
                PopulatedHighScores (HighScores entries_) ->
                    entries_ |> List.indexedMap existingHighScoreRow

                _ ->
                    []
    in
    entries
        |> appendEmptyEntries emptyHighScoreRow
        |> view "High Scores" (highScoresModalConfig model)


highScoresModalConfig : HighScoresModel -> Modal.Config HighScoresMsg
highScoresModalConfig model =
    case model of
        EmptyHighScores ->
            { closeButton = Modal.Close { onPress = HighScoresCancelled }
            , submitButton = Modal.None
            , customButtons = []
            }

        PopulatedHighScores highScores ->
            { closeButton = Modal.Close { onPress = HighScoresCancelled }
            , submitButton = Modal.Save { onPress = Nothing }
            , customButtons = [ { caption = "Reset", onPress = Just HighScoresResetRequested } ]
            }

        ResetHighScores ->
            { closeButton = Modal.Cancel { onPress = HighScoresCancelled }
            , submitButton = Modal.Save { onPress = Just HighScoresSubmitted }
            , customButtons = [ { caption = "Reset", onPress = Nothing } ]
            }



-- NEW HIGH SCORE


type NewHighScoreModel
    = NewHighScoreModel { above : List Entry, new : Entry, below : List Entry }


toHighScores : NewHighScoreModel -> HighScores
toHighScores (NewHighScoreModel { above, new, below }) =
    List.concat [ above, [ new ], below ] |> HighScores


withPossibleNewHighScore : Int -> HighScores -> Maybe NewHighScoreModel
withPossibleNewHighScore score (HighScores entries) =
    let
        ( above, below ) =
            entries |> List.partition (\entry -> entry.score >= score)

        aboveCount =
            List.length above
    in
    if aboveCount >= maxItems then
        Nothing

    else
        Just <|
            NewHighScoreModel
                { above = above
                , new = { name = "", score = score }
                , below = List.take (maxItems - aboveCount - 1) below
                }


map : (Int -> Entry -> a) -> (Int -> Entry -> a) -> (Int -> a) -> NewHighScoreModel -> List a
map existingEntriesMapper newEntryMapper emptyEntryMapper (NewHighScoreModel { above, new, below }) =
    List.concat
        [ List.indexedMap existingEntriesMapper above
        , [ newEntryMapper (List.length above) new ]
        , List.indexedMap existingEntriesMapper below
        ]
        |> appendEmptyEntries emptyEntryMapper


isNamePopulated : NewHighScoreModel -> Bool
isNamePopulated (NewHighScoreModel { new }) =
    new.name |> String.trim |> String.isEmpty


setName : String -> NewHighScoreModel -> NewHighScoreModel
setName newName (NewHighScoreModel data) =
    NewHighScoreModel { data | new = data.new |> withName newName }


withName : String -> Entry -> Entry
withName name entry =
    { entry | name = String.left 10 <| name }



-- NEW HIGH SCORES MODAL DIALOG


type NewHighScoreMsg
    = NewHighScoreNameChanged String
    | NewHighScoreSubmitted
    | NewHighScoreCancelled
    | NewHighScoreNameFocused


initNewHighScoreDialog : Int -> HighScores -> Maybe ( NewHighScoreModel, Cmd NewHighScoreMsg )
initNewHighScoreDialog score highScores =
    withPossibleNewHighScore score highScores
        |> Maybe.map
            (\model ->
                ( model
                , Browser.Dom.focus newHighScoreNameInputId |> Task.attempt (always NewHighScoreNameFocused)
                )
            )


newHighScoreNameInputId : String
newHighScoreNameInputId =
    "high-score-name"


type NewHighScoreUpdateResult
    = KeepOpen NewHighScoreModel
    | Close (Maybe ( HighScores, Cmd NewHighScoreMsg ))


updateNewHighScoreDialog : NewHighScoreMsg -> NewHighScoreModel -> NewHighScoreUpdateResult
updateNewHighScoreDialog msg model =
    case msg of
        NewHighScoreNameChanged name ->
            KeepOpen <| setName name model

        NewHighScoreSubmitted ->
            let
                highScores =
                    toHighScores model
            in
            Close <| Just ( highScores, highScores |> toJson |> Ports.persistHighScores )

        NewHighScoreCancelled ->
            Close Nothing

        NewHighScoreNameFocused ->
            KeepOpen model


newHighScoreView : NewHighScoreModel -> Element NewHighScoreMsg
newHighScoreView model =
    model
        |> map existingHighScoreRow newHighScoreRow emptyHighScoreRow
        |> view "New High Score" (newHighScoreModalConfig model)


newHighScoreModalConfig : NewHighScoreModel -> Modal.Config NewHighScoreMsg
newHighScoreModalConfig model =
    let
        onSubmit =
            if isNamePopulated model then
                Nothing

            else
                Just NewHighScoreSubmitted
    in
    Modal.defaultConfig NewHighScoreCancelled onSubmit


newHighScoreRow : Int -> Entry -> Element NewHighScoreMsg
newHighScoreRow index { name, score } =
    let
        nameElement =
            Element.Input.text
                [ Element.Background.color <| Element.rgb255 100 100 100
                , Element.padding 6
                , Element.Font.semiBold
                , Element.Font.size 16
                , Element.htmlAttribute <| Html.Attributes.id newHighScoreNameInputId
                ]
                { text = name
                , label = Element.Input.labelHidden "Name"
                , onChange = NewHighScoreNameChanged
                , placeholder = Nothing
                }
    in
    highScoreRow index nameElement (populatedScoreElement score)



-- SUBSCRIPTIONS


highScoresDialogSubscriptions : HighScoresModel -> Sub HighScoresMsg
highScoresDialogSubscriptions model =
    Modal.subscriptions <| highScoresModalConfig model


newHighScoreDialogSubscriptions : NewHighScoreModel -> Sub NewHighScoreMsg
newHighScoreDialogSubscriptions model =
    Modal.subscriptions <| newHighScoreModalConfig model
