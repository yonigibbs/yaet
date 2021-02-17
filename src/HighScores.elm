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

{-| This module contains all functionality related to high scores. Includes decoding/encoding the JSON representation
stored in local storage, the `HighScores` type which contains those values so they can be shown to the user, and
handling a new high score at the end of a game.

It also provides the UI for the two ways the high scores are shown to the user: from the Welcome screen (where the user
is shown a read-only copy of the high scores, and can choose to reset them, i.e. delete them all), and at the end of a
game if the new score is high enough to go on the leaderboard, in which case the user is prompted for a name to store
against this high score.

-}

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


{-| The high scores (exposed as an opaque type). Stores the high scores as an ordered list.
-}
type HighScores
    = HighScores (List Entry)


{-| An entry in the high scores list, i.e. the name of the person who got the high score, and the score itself.
-}
type alias Entry =
    { name : String, score : Int }


empty : HighScores
empty =
    HighScores []


isEmpty : HighScores -> Bool
isEmpty (HighScores entries) =
    List.isEmpty entries



-- JSON


{-| Decodes the supplied JSON into a `HighScores` value, defaulting to using an empty set of high scores if the decoding
fails.
-}
fromJson : JE.Value -> HighScores
fromJson json =
    json |> JD.decodeValue (JD.list entryDecoder) |> Result.map fromEntries |> Result.withDefault empty


entryDecoder : JD.Decoder Entry
entryDecoder =
    JD.map2 Entry (JD.field "name" JD.string) (JD.field "score" JD.int)


{-| Encodes the supplied `HighScores` to JSON.
-}
toJson : HighScores -> JE.Value
toJson (HighScores entries) =
    entries |> JE.list encodeEntry


encodeEntry : Entry -> JE.Value
encodeEntry { name, score } =
    JE.object [ ( "name", JE.string name ), ( "score", JE.int score ) ]


{-| From the supplied list, returns a `HighScores` value. This ensures that the data in the returned value is valid,
i.e. has no more than `maxItems` in it, and is sorted (descending by score).
-}
fromEntries : List Entry -> HighScores
fromEntries entries =
    entries
        |> List.take maxItems
        |> List.map (\entry -> { entry | name = String.left maxNameLength entry.name })
        |> List.sortBy .score
        |> List.reverse
        |> HighScores


{-| The maximum number of items to retain in the high scores.
-}
maxItems : Int
maxItems =
    5


{-| The maximum number of characters allowed in a name in the high scores.
-}
maxNameLength : Int
maxNameLength =
    8



-- FUNCTIONS SHARED BETWEEN HIGH-SCORES-VIEW AND NEW-HIGH-SCORE-VIEW


{-| The common view function which displays the high scores in a modal dialog. Used both when showing the high scores,
and when handling adding a new high score.
-}
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


{-| A row in the list of high scores, where the name and score are displayed based on the supplied `nameElement` and
`valueElement` parameters.
-}
highScoreRow : Int -> Element msg -> Element msg -> Element msg
highScoreRow index nameElement scoreElement =
    Element.row [ Element.width Element.fill, Element.spacing 5, Element.height <| Element.px 25 ]
        [ index + 1 |> String.fromInt |> Element.text |> Element.el [ Element.alignLeft, Element.width <| Element.px 15 ]
        , Element.el [ Element.alignLeft, Element.width <| Element.px 150 ] nameElement
        , Element.el [ Element.alignRight ] scoreElement
        ]


{-| A row in the list of high scores for when there isn't yet an entry at this index.
-}
emptyHighScoreRow : Int -> Element msg
emptyHighScoreRow index =
    highScoreRow index Element.none Element.none


{-| A row in the list of high scores representing an existing high score.
-}
existingHighScoreRow : Int -> Entry -> Element msg
existingHighScoreRow index { name, score } =
    highScoreRow index (Element.text name) (populatedScoreElement score)


populatedScoreElement : Int -> Element msg
populatedScoreElement score =
    Element.text <| String.fromInt score


{-| Appends the required number of empty entries to the supplied list to ensure that it has `maxItems` in it (i.e.
pads the list).
-}
appendEmptyEntries : (Int -> a) -> List a -> List a
appendEmptyEntries emptyEntryMapper list =
    List.range (List.length list) (maxItems - 1)
        |> List.map emptyEntryMapper
        |> (++) list



-- HIGH SCORES VIEW


{-| The model of the dialog which shows the high scores in a read-only mode to the user, but lets the user reset (i.e.
delete) all the high scores. This dialog is shown from the High Scores button on the Welcome screen.
-}
type HighScoresModel
    = EmptyHighScores -- There are no high scores
    | PopulatedHighScores HighScores -- There are some high scores
    | ResetHighScores -- There were some high scores but the user chose to reset them - this hasn't been "committed" yet though.


{-| The messages in the dialog which shows the high scores in a read-only mode to the user, but lets the user reset (i.e.
delete) all the high scores.
-}
type HighScoresMsg
    = HighScoresResetRequested
    | HighScoresSubmitted
    | HighScoresCancelled


{-| Initialises the the dialog which shows the high scores in a read-only mode to the user, but lets the user reset (i.e.
delete) all the high scores.
-}
initHighScoresDialog : HighScores -> HighScoresModel
initHighScoresDialog highScores =
    if isEmpty highScores then
        EmptyHighScores

    else
        PopulatedHighScores highScores


{-| The value returned from `updateHighScoresDialog`, informing the caller how to proceed:

  - `KeepOpen_`: The modal should stay open. Its model is the data associated with this variant.
  - `Close_`: The modal should be closed. The associated data is a `Maybe` which, if it isn't `Nothing`, contains the
    newly updated (i.e. reset) high scores, and a command to run (which will be persisting those new high scores to
    local storage via a port).

-}
type HighScoresUpdateResult
    = KeepOpen_ HighScoresModel
    | Close_ (Maybe ( HighScores, Cmd HighScoresMsg ))


{-| The standard `update` function when the high scores are shown to the user in read-only mode. See
`HighScoresUpdateResult` for more info.
-}
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


{-| The view for when the high scores are shown to the user in read-only mode.
-}
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


{-| The config for the modal dialog when showing the high scores in read-only mode. Depending on whether there are any
high scores, and whether the user has chosen to reset them or not, different buttons are shown/enabled.
-}
highScoresModalConfig : HighScoresModel -> Modal.Config HighScoresMsg
highScoresModalConfig model =
    case model of
        EmptyHighScores ->
            { closeButton = Modal.Close { onPress = HighScoresCancelled }
            , submitButton = Modal.None
            , customButtons = []
            }

        PopulatedHighScores _ ->
            { closeButton = Modal.Close { onPress = HighScoresCancelled }
            , submitButton = Modal.Save { onPress = Nothing }
            , customButtons = [ { caption = "Reset", onPress = Just HighScoresResetRequested } ]
            }

        ResetHighScores ->
            { closeButton = Modal.Cancel { onPress = HighScoresCancelled }
            , submitButton = Modal.Save { onPress = Just HighScoresSubmitted }
            , customButtons = [ { caption = "Reset", onPress = Nothing } ]
            }



-- NEW HIGH SCORE VIEW


{-| The model of the dialog which lets the user add a new high score at the end of a game.

This acts a bit like a zipper: there is the (ordered) list of entries above the new one, and the same for below, and
a single entry for the new one being added (for which the user is prompted to supply a name).

-}
type NewHighScoreModel
    = NewHighScoreModel { above : List Entry, new : Entry, below : List Entry }


toHighScores : NewHighScoreModel -> HighScores
toHighScores (NewHighScoreModel { above, new, below }) =
    List.concat [ above, [ new ], below ] |> fromEntries


{-| The messages in the dialog which lets the user add a new high score at the end of a game.
-}
type NewHighScoreMsg
    = NewHighScoreNameChanged String
    | NewHighScoreSubmitted
    | NewHighScoreCancelled
    | NewHighScoreNameFocused


{-| Possibly initialises the the dialog which lets the user add a new high score at the end of a game. This depends on
whether the supplied score is high enough to be a new score. If it isn't, returns `Nothing`. Otherwise returns a tuple
containing the `NewHighScoreModel`, and a command to run (used to set focus to the Name control in the UI).
-}
initNewHighScoreDialog : Int -> HighScores -> Maybe ( NewHighScoreModel, Cmd NewHighScoreMsg )
initNewHighScoreDialog score highScores =
    withPossibleNewHighScore score highScores
        |> Maybe.map
            (\model ->
                ( model
                , Browser.Dom.focus newHighScoreNameInputId |> Task.attempt (always NewHighScoreNameFocused)
                )
            )


{-| The ID of the input asking the user to enter a name for a new high score.
-}
newHighScoreNameInputId : String
newHighScoreNameInputId =
    "high-score-name"


{-| The value returned from `updateNewHighScoreDialog`, informing the caller how to proceed:

  - `KeepOpen`: The modal should stay open. Its model is the data associated with this variant.
  - `Close`: The modal should be closed open. The associated data is a `Maybe` which, if it isn't `Nothing`, contains
    the updated high scores, including the new score just added, and a command to run (which will be persisting those
    new high scores to local storage via a port).

-}
type NewHighScoreUpdateResult
    = KeepOpen NewHighScoreModel
    | Close (Maybe ( HighScores, Cmd NewHighScoreMsg ))


{-| The standard `update` function when a new high score is being added. See `NewHighScoreUpdateResult` for more info.
-}
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


{-| Checks whether the supplied score is high enough to be added as a new high score. Returns `Nothing` if it isn't.
Otherwise returns a `NewHighScoreModel` used to them show the UI to let the user enter a name against this new score.
-}
withPossibleNewHighScore : Int -> HighScores -> Maybe NewHighScoreModel
withPossibleNewHighScore score (HighScores entries) =
    if score == 0 then
        Nothing

    else
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


{-| Maps the items in the `NewHighScoreModel`, using a different mapper for existing entries, the new entry, and empty
entries.
-}
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
    { entry | name = String.left maxNameLength <| name }


{-| The view for when a new high score is being added.
-}
newHighScoreView : NewHighScoreModel -> Element NewHighScoreMsg
newHighScoreView model =
    model
        |> map existingHighScoreRow newHighScoreRow emptyHighScoreRow
        |> view "New High Score" (newHighScoreModalConfig model)


{-| The config for the modal dialog when handling a new high score. Depending on whether or not the name is populated,
the Save button will be enabled/disabled.
-}
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
