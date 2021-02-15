module HighScores exposing
    ( EditableHighScores
    , HighScores
    , NewHighScoreDialogUpdateResult(..)
    , NewHighScoreMsg
    , fromJson
    , initNewHighScoreDialog
    , newHighScoreDialogSubscriptions
    , newHighScoreView
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



-- JSON


fromJson : JE.Value -> HighScores
fromJson json =
    json |> JD.decodeValue (JD.list entryDecoder) |> Result.withDefault [] |> fromEntries


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



-- EDITABLE HIGH SCORES


type
    EditableHighScores
    -- TODO: rename this as NewHighScoreDialogModel maybe?
    = EditableHighScores { above : List Entry, new : Entry, below : List Entry }


fromEditable : EditableHighScores -> HighScores
fromEditable (EditableHighScores { above, new, below }) =
    List.concat [ above, [ new ], below ] |> HighScores


withPossibleNewHighScore : Int -> HighScores -> Maybe EditableHighScores
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
            EditableHighScores
                { above = above
                , new = { name = "", score = score }
                , below = List.take (maxItems - aboveCount - 1) below
                }


map : (Int -> Entry -> a) -> (Int -> Entry -> a) -> (Int -> a) -> EditableHighScores -> List a
map existingEntriesMapper newEntryMapper emptyEntryMapper (EditableHighScores { above, new, below }) =
    List.concat
        [ List.indexedMap existingEntriesMapper above
        , [ newEntryMapper (List.length above) new ]
        , List.indexedMap existingEntriesMapper below
        ]
        |> appendEmptyEntries emptyEntryMapper


appendEmptyEntries : (Int -> a) -> List a -> List a
appendEmptyEntries emptyEntryMapper list =
    List.range (List.length list) (maxItems - 1)
        |> List.map emptyEntryMapper
        |> (++) list


isNamePopulated : EditableHighScores -> Bool
isNamePopulated (EditableHighScores { new }) =
    new.name |> String.trim |> String.isEmpty


setName : String -> EditableHighScores -> EditableHighScores
setName newName (EditableHighScores data) =
    EditableHighScores { data | new = data.new |> withName newName }


withName : String -> Entry -> Entry
withName name entry =
    { entry | name = String.left 10 <| name }



-- NEW HIGH SCORES MODAL DIALOG


type NewHighScoreMsg
    = NewHighScoreNameChanged String
    | NewHighScoreSubmitted
    | NewHighScoreCancelled
    | NewHighScoreNameFocused


initNewHighScoreDialog : Int -> HighScores -> Maybe ( EditableHighScores, Cmd NewHighScoreMsg )
initNewHighScoreDialog score highScores =
    withPossibleNewHighScore score highScores
        |> Maybe.map
            (\editableHighScores ->
                ( editableHighScores
                , Browser.Dom.focus "high-score-name" |> Task.attempt (always NewHighScoreNameFocused)
                )
            )


type NewHighScoreDialogUpdateResult
    = KeepOpen EditableHighScores
    | Close (Maybe ( HighScores, Cmd NewHighScoreMsg ))


updateNewHighScoreDialog : NewHighScoreMsg -> EditableHighScores -> NewHighScoreDialogUpdateResult
updateNewHighScoreDialog msg editableHighScores =
    case msg of
        NewHighScoreNameChanged name ->
            KeepOpen <| setName name editableHighScores

        NewHighScoreSubmitted ->
            let
                highScores =
                    fromEditable editableHighScores
            in
            Close <| Just ( highScores, highScores |> toJson |> Ports.persistHighScores )

        NewHighScoreCancelled ->
            Close Nothing

        NewHighScoreNameFocused ->
            KeepOpen editableHighScores


newHighScoreView : EditableHighScores -> Element NewHighScoreMsg
newHighScoreView editableHighScores =
    Element.column
        [ Element.spacing 15
        , Element.centerX
        , Element.paddingXY 4 0
        , Element.width <| Element.px 240
        , Element.Font.color <| Element.rgb255 200 200 200
        ]
        [ Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24 ] <| Element.text "New High Score"
        , Element.el [ Element.Font.size 18 ] <| newHighScoresTable editableHighScores
        ]
        |> Modal.dialog (newHighScoreModalDialogConfig editableHighScores)


newHighScoreModalDialogConfig : EditableHighScores -> Modal.Config NewHighScoreMsg
newHighScoreModalDialogConfig editableHighScores =
    let
        onSubmit =
            if isNamePopulated editableHighScores then
                Nothing

            else
                Just NewHighScoreSubmitted
    in
    Modal.defaultConfig NewHighScoreCancelled onSubmit


newHighScoresTable : EditableHighScores -> Element NewHighScoreMsg
newHighScoresTable editableHighScores =
    editableHighScores
        |> map existingHighScoreRow newHighScoreRow emptyHighScoreRow
        |> Element.column [ Element.spacingXY 0 5 ]


existingHighScoreRow : Int -> Entry -> Element msg
existingHighScoreRow index { name, score } =
    highScoreRow index (Element.text name) (populatedScoreElement score)


populatedScoreElement : Int -> Element msg
populatedScoreElement score =
    Element.text <| String.fromInt score


newHighScoreRow : Int -> Entry -> Element NewHighScoreMsg
newHighScoreRow index { name, score } =
    let
        nameElement =
            Element.Input.text
                [ Element.Background.color <| Element.rgb255 100 100 100
                , Element.padding 6
                , Element.Font.semiBold
                , Element.Font.size 16
                , Element.htmlAttribute <| Html.Attributes.id "high-score-name"
                ]
                { text = name
                , label = Element.Input.labelHidden "Name"
                , onChange = NewHighScoreNameChanged
                , placeholder = Nothing
                }
    in
    highScoreRow index nameElement (populatedScoreElement score)


emptyHighScoreRow : Int -> Element msg
emptyHighScoreRow index =
    highScoreRow index Element.none Element.none


highScoreRow : Int -> Element msg -> Element msg -> Element msg
highScoreRow index nameElement scoreElement =
    Element.row [ Element.width Element.fill, Element.spacing 20 ]
        [ Element.row [ Element.alignLeft, Element.width Element.fill, Element.height <| Element.px 25 ]
            [ index + 1 |> String.fromInt |> Element.text |> Element.el [ Element.width <| Element.px 25 ]
            , nameElement
            ]
        , Element.el [ Element.alignRight ] scoreElement
        ]


newHighScoreDialogSubscriptions : EditableHighScores -> Sub NewHighScoreMsg
newHighScoreDialogSubscriptions editableHighScores =
    Modal.subscriptions <| newHighScoreModalDialogConfig editableHighScores
