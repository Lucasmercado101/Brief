module Page.EditNote exposing (..)

import Api exposing (SyncableID(..))
import Browser.Navigation as Nav
import Css exposing (alignItems, backgroundColor, bold, bolder, border, border3, borderBottom3, borderLeft3, borderRight3, borderTop, borderTop3, center, color, column, cursor, display, displayFlex, flexDirection, flexWrap, fontSize, fontWeight, height, hover, inline, inlineBlock, int, justifyContent, marginBottom, marginTop, maxHeight, maxWidth, minWidth, none, padding, paddingBottom, paddingLeft, paddingTop, pct, pointer, px, resize, solid, spaceBetween, stretch, textAlign, transparent, vertical, width, wrap)
import CssHelpers exposing (black, col, delaGothicOne, error, gap, padX, padY, primary, publicSans, row, secondary, textColor, white)
import DataTypes exposing (Label, Note)
import Dog exposing (dog2Svg)
import Helpers exposing (exclude, listFirst, maybeToBool, sameId)
import Html.Styled exposing (Html, button, div, input, li, p, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (autofocus, class, css, disabled, placeholder, title, value)
import Html.Styled.Events exposing (onClick, onInput)
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (Action(..))
import Ports exposing (receiveRandomValues, requestRandomValues)
import Random
import Route
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)


pureNoSignal : Model -> ( Model, Cmd msg, Maybe Signal )
pureNoSignal m =
    ( m, Cmd.none, Nothing )


pureWithSignal : Signal -> Model -> ( Model, Cmd msg, Maybe Signal )
pureWithSignal s m =
    ( m, Cmd.none, Just s )


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveRandomValues ReceivedRandomValues


getCurrentTime : (Posix -> Msg) -> Cmd Msg
getCurrentTime msg =
    Time.now
        |> Task.perform msg


type alias Model =
    { noteId : SyncableID

    -- global data
    , key : Nav.Key
    , seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    , noteData : Maybe EditingState
    }


type EditingState
    = Editing Note (Maybe String)
    | ConfirmDeletion Note


init :
    { notes : List Note
    , labels : List Label
    , noteId : SyncableID
    , seeds : List Random.Seed
    , key : Nav.Key
    , noteData : Maybe Note
    }
    -> Model
init { notes, labels, noteId, seeds, key, noteData } =
    { notes = notes
    , labels = labels
    , noteId = noteId
    , seeds = seeds
    , key = key
    , noteData = Maybe.map (\e -> Editing e Nothing) noteData
    }



-- UPDATE


type Msg
    = NoOp
    | FinishEditing
    | ChangePin Bool
    | ChangeTitle String
    | ChangeContent String
    | RequestDeletion
    | CancelDeletion
    | ConfirmNoteDeletion
    | ToggleEditLabels Bool
    | RequestTimeForNewLabelCreation
    | CreateNewLabel Posix
    | ReceivedRandomValues (List Int)
    | ChangeLabelSearchQuery String
    | AddLabel SyncableID
    | RemoveLabel SyncableID


type Signal
    = OfflineQueueAction OfflineQueue.Action


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Signal )
update msg model =
    let
        noteExists =
            listFirst (.id >> sameId model.noteId) model.notes
    in
    case noteExists of
        Nothing ->
            -- TODO: what now?
            model |> pureNoSignal

        Just originalNote ->
            let
                changeNoteData : (Note -> Note) -> Model
                changeNoteData fn =
                    case model.noteData of
                        Just editingState ->
                            case editingState of
                                ConfirmDeletion _ ->
                                    model

                                Editing noteData isEditingLabels ->
                                    { model
                                        | noteData =
                                            Just (Editing (fn noteData) isEditingLabels)
                                    }

                        Nothing ->
                            model

                changeEditingState fn =
                    { model
                        | noteData =
                            Maybe.map
                                (\noteState ->
                                    case noteState of
                                        ConfirmDeletion noteData ->
                                            ConfirmDeletion noteData

                                        Editing noteData isEditingLabels ->
                                            fn noteData isEditingLabels |> (\( e, v ) -> Editing e v)
                                )
                                model.noteData
                    }
            in
            case msg of
                NoOp ->
                    model |> pureNoSignal

                RequestDeletion ->
                    { model
                        | noteData =
                            Maybe.map
                                (\noteState ->
                                    case noteState of
                                        ConfirmDeletion noteData ->
                                            noteState

                                        Editing noteData _ ->
                                            ConfirmDeletion noteData
                                )
                                model.noteData
                    }
                        |> pureNoSignal

                CancelDeletion ->
                    { model
                        | noteData =
                            Maybe.map
                                (\noteState ->
                                    case noteState of
                                        ConfirmDeletion noteData ->
                                            Editing noteData Nothing

                                        Editing _ _ ->
                                            noteState
                                )
                                model.noteData
                    }
                        |> pureNoSignal

                ConfirmNoteDeletion ->
                    case model.noteData of
                        Just _ ->
                            ( model
                            , Route.replaceUrl model.key Route.Home
                            , Just (OfflineQueueAction (QDeleteNote originalNote.id))
                            )

                        Nothing ->
                            model |> pureNoSignal

                FinishEditing ->
                    case model.noteData of
                        Just noteState ->
                            case noteState of
                                ConfirmDeletion _ ->
                                    model |> pureNoSignal

                                Editing noteData _ ->
                                    let
                                        titleChanged =
                                            noteData.title /= originalNote.title

                                        contentChanged =
                                            noteData.content /= originalNote.content

                                        queueSignal =
                                            if titleChanged || contentChanged then
                                                Just
                                                    (OfflineQueueAction
                                                        (QEditNote
                                                            { id = originalNote.id
                                                            , title =
                                                                if titleChanged then
                                                                    noteData.title

                                                                else
                                                                    Nothing
                                                            , content =
                                                                if contentChanged then
                                                                    Just noteData.content

                                                                else
                                                                    Nothing
                                                            , pinned = Nothing
                                                            , labels = Nothing
                                                            }
                                                        )
                                                    )

                                            else
                                                Nothing
                                    in
                                    ( model, Route.replaceUrl model.key Route.Home, queueSignal )

                        Nothing ->
                            model |> pureNoSignal

                ChangeTitle s ->
                    changeNoteData (\e -> { e | title = Just s })
                        |> pureNoSignal

                ChangeContent s ->
                    changeNoteData (\e -> { e | content = s })
                        |> pureNoSignal

                ChangePin newVal ->
                    ( { model
                        | notes =
                            model.notes
                                |> List.map
                                    (\e ->
                                        if sameId e.id originalNote.id then
                                            { e | pinned = newVal }

                                        else
                                            e
                                    )
                      }
                    , Cmd.none
                    , Just (OfflineQueueAction (QToggleNotePin originalNote.id newVal))
                    )

                ToggleEditLabels newVal ->
                    changeEditingState
                        (\data _ ->
                            ( data
                            , if newVal then
                                Just ""

                              else
                                Nothing
                            )
                        )
                        |> pureNoSignal

                ChangeLabelSearchQuery newQuery ->
                    changeEditingState
                        (\data searchQuery ->
                            ( data, Just newQuery )
                        )
                        |> pureNoSignal

                AddLabel id ->
                    changeNoteData
                        (\e ->
                            { e
                                | labels =
                                    case e.labels of
                                        [] ->
                                            [ id ]

                                        labels ->
                                            id :: labels
                            }
                        )
                        |> pureNoSignal

                RemoveLabel id ->
                    changeNoteData
                        (\e ->
                            { e | labels = e.labels |> exclude (sameId id) }
                        )
                        |> pureNoSignal

                RequestTimeForNewLabelCreation ->
                    ( model
                    , Cmd.batch [ requestRandomValues (), getCurrentTime CreateNewLabel ]
                    , Nothing
                    )

                CreateNewLabel timeNow ->
                    case model.noteData of
                        Just noteState ->
                            case noteState of
                                ConfirmDeletion noteData ->
                                    model |> pureNoSignal

                                Editing noteData labelsSearchQuery ->
                                    case labelsSearchQuery of
                                        Just query ->
                                            let
                                                labelName : String
                                                labelName =
                                                    query |> String.trim

                                                offlineId : String
                                                offlineId =
                                                    generateUID model.seeds |> Tuple.first

                                                newLabel : Label
                                                newLabel =
                                                    { createdAt = timeNow
                                                    , updatedAt = timeNow
                                                    , id = OfflineID offlineId
                                                    , name = labelName
                                                    }
                                            in
                                            if String.length query /= 0 then
                                                ( { model
                                                    | labels = newLabel :: model.labels
                                                    , noteData =
                                                        Just (Editing noteData (Just ""))
                                                  }
                                                , requestRandomValues ()
                                                , Just (OfflineQueueAction (QNewLabel { offlineId = offlineId, name = labelName }))
                                                )

                                            else
                                                model |> pureNoSignal

                                        Nothing ->
                                            model |> pureNoSignal

                        Nothing ->
                            model |> pureNoSignal

                ReceivedRandomValues values ->
                    { model | seeds = List.map Random.initialSeed values }
                        |> pureNoSignal



-- VIEW


view : Model -> Html Msg
view model =
    case model.noteData of
        Nothing ->
            -- TODO: empty state when no note or just redirect?
            div [] [ text "Note Not found" ]

        Just editingStatus ->
            row [ css [ height (pct 100), justifyContent center, alignItems center ] ]
                [ case editingStatus of
                    ConfirmDeletion data ->
                        div
                            [ css
                                [ backgroundColor secondary
                                , border3 (px 5) solid black
                                , displayFlex
                                , flexDirection column
                                , maxWidth (px 480)
                                , minWidth (px 480)
                                ]
                            ]
                            [ p [ css [ delaGothicOne, fontSize (px 38), marginTop (px 12), marginBottom (px 16), textAlign center ] ] [ text "Really?" ]
                            , div [ css [ display inlineBlock, textAlign center ] ]
                                [ p [ css [ publicSans, fontSize (px 18), display inline ] ]
                                    [ text
                                        ("Are you sure you want to delete "
                                            ++ (case data.title of
                                                    Just _ ->
                                                        "note \""

                                                    Nothing ->
                                                        "this note?"
                                               )
                                        )
                                    ]
                                , case data.title of
                                    Just noteName ->
                                        strong [ css [ fontWeight (int 900), publicSans, fontSize (px 18), display inline ] ] [ text noteName ]

                                    Nothing ->
                                        text ""
                                , case data.title of
                                    Just _ ->
                                        p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "\"?" ]

                                    Nothing ->
                                        text ""
                                ]
                            , div [ css [ displayFlex, marginTop (px 16), backgroundColor white ] ]
                                [ button
                                    [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), borderTop3 (px 5) solid black, width (pct 100), padY (px 10), textAlign center ]
                                    , onClick CancelDeletion
                                    ]
                                    [ text "Cancel" ]
                                , button
                                    [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor error, textColor white, border (px 0), publicSans, fontSize (px 22), width (pct 100), borderLeft3 (px 5) solid black, borderTop3 (px 5) solid black, padY (px 10), textAlign center ]
                                    , onClick ConfirmNoteDeletion
                                    ]
                                    [ text "Confirm" ]
                                ]
                            ]

                    Editing val isEditingLabels ->
                        let
                            topActions =
                                row [ css [ justifyContent spaceBetween, alignItems center, backgroundColor white, borderBottom3 (px 3) solid black ] ]
                                    [ button
                                        [ css
                                            [ border (px 0)
                                            , if val.pinned then
                                                hover [ color black, backgroundColor white ]

                                              else
                                                hover [ backgroundColor black, color white ]
                                            , cursor pointer
                                            , displayFlex
                                            , justifyContent center
                                            , alignItems center
                                            , paddingTop (px 8)
                                            , paddingBottom (px 6)
                                            , padX (px 8)
                                            , borderRight3 (px 2) solid black
                                            , if val.pinned then
                                                Css.batch [ backgroundColor black, color white ]

                                              else
                                                Css.batch []
                                            ]
                                        , title
                                            (if val.pinned then
                                                "Unpin"

                                             else
                                                "pin"
                                            )
                                        , onClick (ChangePin (not val.pinned))
                                        ]
                                        [ Filled.push_pin 32 Inherit |> Svg.Styled.fromUnstyled ]
                                    , p [ css [ delaGothicOne, fontSize (px 24) ] ] [ text "Editing Note" ]
                                    , button
                                        [ css [ hover [ backgroundColor black, color white ], border (px 0), cursor pointer, displayFlex, justifyContent center, alignItems center, paddingTop (px 8), paddingBottom (px 6), padX (px 8), backgroundColor primary, borderLeft3 (px 2) solid black, color black ]
                                        , onClick FinishEditing
                                        ]
                                        [ Filled.check 32 Inherit |> Svg.Styled.fromUnstyled ]
                                    ]

                            bottomActions =
                                row [ css [ justifyContent spaceBetween, backgroundColor white, borderTop3 (px 3) solid black ] ]
                                    [ button
                                        [ css [ hover [ backgroundColor black ], border (px 0), cursor pointer, displayFlex, color white, justifyContent center, alignItems center, backgroundColor error, paddingTop (px 8), paddingBottom (px 6), padX (px 8), borderRight3 (px 2) solid black ]
                                        , onClick RequestDeletion
                                        ]
                                        [ Filled.delete 28 Inherit |> Svg.Styled.fromUnstyled ]
                                    , button
                                        [ css
                                            [ if maybeToBool isEditingLabels then
                                                Css.batch [ hover [ backgroundColor white, color black ], color white, backgroundColor black ]

                                              else
                                                Css.batch [ hover [ backgroundColor black, color white ], color black, backgroundColor white ]
                                            , border (px 0)
                                            , cursor pointer
                                            , displayFlex
                                            , justifyContent center
                                            , alignItems center
                                            , paddingTop (px 8)
                                            , paddingBottom (px 6)
                                            , padX (px 8)
                                            , borderLeft3 (px 2) solid black
                                            ]
                                        , onClick (ToggleEditLabels (not (maybeToBool isEditingLabels)))
                                        ]
                                        [ Filled.label 28 Inherit |> Svg.Styled.fromUnstyled ]
                                    ]

                            editNoteCard =
                                col [ css [ backgroundColor secondary, border3 (px 3) solid black, maxHeight (pct 70), width (pct 100), minWidth (px 400) ] ]
                                    [ topActions
                                    , textarea
                                        [ css [ delaGothicOne, fontSize (px 24), padding (px 16), backgroundColor transparent, border (px 0), resize vertical ]
                                        , autofocus True
                                        , placeholder "Note Title Here."
                                        , onInput ChangeTitle
                                        , value
                                            (case val.title of
                                                Just t ->
                                                    t

                                                Nothing ->
                                                    ""
                                            )
                                        ]
                                        []
                                    , textarea
                                        [ css [ publicSans, padding (px 16), backgroundColor transparent, border (px 0), fontSize (px 16), resize vertical ]
                                        , onInput ChangeContent
                                        , value val.content
                                        , placeholder "Note Content Here."
                                        ]
                                        []
                                    , bottomActions
                                    ]
                        in
                        -- TODO: add max title and content length
                        div
                            [ css
                                [ -- TODO: a better width
                                  height (pct 100)
                                , displayFlex
                                , alignItems center
                                , gap 32
                                ]
                            ]
                            [ editNoteCard
                            , case isEditingLabels of
                                Just searchQuery ->
                                    labelsCard val searchQuery model.labels

                                Nothing ->
                                    text ""
                            ]
                ]


labelsCard : Note -> String -> List Label -> Html Msg
labelsCard note labelsSearchQuery labels =
    let
        labelExists =
            (labelsSearchQuery |> String.trim)
                == ""
                || (case labels of
                        [] ->
                            False

                        x :: xs ->
                            List.any (\e -> (e.name |> String.toLower) == (labelsSearchQuery |> String.toLower |> String.trim)) labels
                   )

        header =
            row [ css [ backgroundColor white, justifyContent spaceBetween, alignItems center, paddingLeft (px 12), minWidth (px 400), borderBottom3 (px 3) solid black ] ]
                [ p [ css [ delaGothicOne, fontSize (px 24) ] ] [ text "Labels" ]
                , button
                    [ css [ hover [ backgroundColor black ], border (px 0), cursor pointer, displayFlex, color white, justifyContent center, alignItems center, backgroundColor error, padding (px 8), borderLeft3 (px 2) solid black ]
                    , onClick (ToggleEditLabels False)
                    ]
                    [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
                ]

        searchQueryInput =
            input
                [ css [ border3 (px 2) solid black, padding (px 6), publicSans, width (pct 100) ]
                , placeholder "Work"
                , class "input"
                , value labelsSearchQuery
                , onInput ChangeLabelSearchQuery
                ]
                []

        content =
            case labels of
                [] ->
                    col [ css [ padding (px 32), alignItems center, gap 12 ] ]
                        [ dog2Svg 100 |> Svg.Styled.fromUnstyled
                        , col [ css [ publicSans ] ]
                            [ p [ css [ textAlign center, fontSize (px 24) ] ] [ text "There are no labels!" ]
                            , p [ css [ textAlign center, fontSize (px 24) ] ] [ text "Create one!" ]
                            ]
                        , searchQueryInput
                        ]

                allLabels ->
                    col [ css [ padding (px 12), gap 32 ] ]
                        [ let
                            selectedLabels =
                                allLabels
                                    -- exclude unselected labels
                                    |> List.filter (\e -> List.any (\e2 -> sameId e.id e2) note.labels)
                          in
                          case selectedLabels of
                            [] ->
                                text ""

                            hasSelectedLabel ->
                                col [ css [ gap 12 ] ]
                                    [ p [ css [ publicSans, fontSize (px 18) ] ] [ text ("Selected (" ++ (List.length selectedLabels |> String.fromInt) ++ "):") ]
                                    , ul [ css [ displayFlex, flexWrap wrap, gap 10 ] ]
                                        (List.map
                                            (\e ->
                                                li
                                                    []
                                                    [ button
                                                        [ css [ fontSize (px 16), cursor pointer, hover [ backgroundColor black, color white, border3 (px 1) solid white ], backgroundColor primary, padding (px 3), publicSans, border3 (px 1) solid black ]
                                                        , onClick (RemoveLabel e.id)
                                                        ]
                                                        [ text e.name ]
                                                    ]
                                            )
                                            selectedLabels
                                        )
                                    ]
                        , col [ css [ gap 12 ] ]
                            (let
                                filteredAllLabels =
                                    allLabels
                                        -- exclude selected labels
                                        |> exclude (\e -> List.any (\e2 -> sameId e.id e2) note.labels)
                                        -- exclude labels that don't match the search query
                                        |> List.filter (\e -> (e.name |> String.toLower) |> String.contains (labelsSearchQuery |> String.toLower |> String.trim))
                             in
                             [ col [ css [ gap 8 ] ]
                                [ p [ css [ publicSans, fontSize (px 18) ] ] [ text ("All labels (" ++ (List.length filteredAllLabels |> String.fromInt) ++ "):") ]
                                , searchQueryInput
                                ]
                             , ul [ css [ displayFlex, flexWrap wrap, gap 10 ] ]
                                (List.map
                                    (\e ->
                                        li
                                            []
                                            [ button
                                                [ css [ fontSize (px 16), cursor pointer, hover [ backgroundColor black, color white, border3 (px 1) solid white ], backgroundColor primary, padding (px 3), publicSans, border3 (px 1) solid black ]
                                                , onClick (AddLabel e.id)
                                                ]
                                                [ text e.name ]
                                            ]
                                    )
                                    filteredAllLabels
                                )
                             ]
                            )
                        ]

        createNewLabelBtn =
            button
                [ css [ border (px 0), fontSize (px 16), publicSans, fontWeight (int 900), padding (px 18), borderTop3 (px 3) solid black, cursor pointer ]
                , onClick RequestTimeForNewLabelCreation
                ]
                [ text "CREATE NEW LABEL" ]
    in
    col [ css [ backgroundColor secondary, border3 (px 3) solid black, maxHeight (pct 70), width (pct 100) ] ]
        [ header
        , content
        , if labelExists then
            text ""

          else
            createNewLabelBtn
        ]
