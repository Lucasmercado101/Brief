module Page.Home exposing (..)

import Api exposing (SyncableID(..))
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (alignItems, auto, backgroundColor, bold, bolder, border, border3, borderBottom3, borderLeft3, borderRight3, boxShadow4, center, color, column, cursor, displayFlex, ellipsis, flexDirection, flexStart, flexWrap, fontSize, fontWeight, height, hex, hidden, hover, inherit, int, justifyContent, margin, margin2, marginLeft, marginTop, maxWidth, minHeight, minWidth, noWrap, overflow, overflowY, padding, padding2, paddingLeft, paddingTop, pct, pointer, position, px, rgb, row, solid, spaceBetween, start, sticky, textAlign, textOverflow, top, transparent, whiteSpace, width, wrap)
import CssHelpers exposing (black, delaGothicOne, error, fullWidth, gap, mx, padX, padY, primary, publicSans, secondary, textColor, userSelectNone, white)
import DataTypes exposing (Label, Note)
import Helpers exposing (exclude, idDiff, labelIDsSplitter, listFirst, or, partitionFirst, sameId)
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (OfflineQueueOps, addToQueue, emptyOfflineQueue, offlineQueueIsEmpty, qCreateNewNote, qDeleteNote, qEditNoteLabels, qNewLabel, qToggleNotePin, queueToOperations)
import Ports exposing (receiveRandomValues, requestRandomValues, updateLastSyncedAt)
import Random
import Route
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveRandomValues ReceivedRandomValues


getNewTimeAndCreateLabel : { id : String, name : String } -> Cmd Msg
getNewTimeAndCreateLabel data =
    Task.perform (CreateNewLabel data) Time.now


getNewTimeForCreateNewNote :
    { id : String
    , title : String
    , content : String
    , pinned : Bool
    , labels : List Api.SyncableID
    }
    -> Cmd Msg
getNewTimeForCreateNewNote data =
    Task.perform (GotCurrentTimeForNewNote data) Time.now


type alias LabelsColumnMenu =
    Maybe String


type alias Model =
    { key : Nav.Key
    , seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    , isWritingANewNote : Maybe NewNoteData
    , newLabelName : String
    , labelsMenu : LabelsColumnMenu
    , filters :
        { label : Maybe SyncableID
        , content : Maybe String
        }

    -- sync stuff
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    , lastSyncedAt : Posix
    }


type alias NewNoteData =
    { title : String
    , content : String
    , labels :
        Maybe
            { labels : List SyncableID
            , labelsSearchQuery : String
            }
    }


type Msg
    = ChangeNotePinned ( SyncableID, Bool )
    | RequestTimeForNewLabelCreation
    | ReceivedRandomValues (List Int)
      -- Labels menu
    | SelectLabelToFilterBy SyncableID
    | OpenLabelsMenu
    | CloseLabelsMenu
    | ChangeLabelsSearchQuery String
    | GoToEditLabelsScreen
      --
    | NewTitleChange String
    | NewNoteContentChange String
    | CreateNewLabel { id : String, name : String } Posix
    | DeleteNote SyncableID
    | BeginWritingNewNote
    | RequestTimeForCreateNewNote
    | GotCurrentTimeForNewNote
        { id : String
        , title : String
        , content : String
        , pinned : Bool
        , labels : List SyncableID
        }
        Posix
    | BeginAddingNewNoteLabels
    | SearchLabelsQueryChange String
    | AddLabelToNewNote SyncableID
    | RemoveLabelFromNewNote SyncableID
    | RemoveLabelFromNote { noteID : SyncableID, labelID : SyncableID }
    | ChangeNewLabelName String
    | ReceivedChangesResp (Result Http.Error Api.ChangesResponse)


update msg model =
    case msg of
        -- Labels column menu
        OpenLabelsMenu ->
            { model | labelsMenu = Just "" }
                |> pure

        CloseLabelsMenu ->
            { model | labelsMenu = Nothing }
                |> pure

        ChangeLabelsSearchQuery s ->
            { model | labelsMenu = Maybe.map (\_ -> s) model.labelsMenu }
                |> pure

        SelectLabelToFilterBy id ->
            let
                newFilter : Maybe SyncableID
                newFilter =
                    case model.filters.label of
                        Nothing ->
                            Just id

                        Just oldId ->
                            if sameId oldId id then
                                Nothing

                            else
                                Just id
            in
            { model
                | filters =
                    { label = newFilter
                    , content = model.filters.content
                    }
            }
                |> pure

        GoToEditLabelsScreen ->
            ( { model | labelsMenu = Nothing }, Route.replaceUrl model.key Route.EditLabels )

        ChangeNotePinned ( uid, newPinnedVal ) ->
            { model
                | notes =
                    List.map
                        (\n ->
                            if n.id == uid then
                                { n | pinned = newPinnedVal }

                            else
                                n
                        )
                        model.notes
            }
                |> pure
                |> addToQueue (qToggleNotePin uid (Just newPinnedVal)) ReceivedChangesResp

        RemoveLabelFromNote { noteID, labelID } ->
            let
                ( noteExists, restNotes ) =
                    partitionFirst (\n -> sameId n.id noteID) model.notes
            in
            case noteExists of
                Nothing ->
                    model |> pure

                Just noteData ->
                    let
                        newNotes =
                            noteData.labels |> exclude (sameId labelID)
                    in
                    { model | notes = { noteData | labels = newNotes } :: restNotes }
                        |> pure
                        |> addToQueue (qEditNoteLabels noteID (Just newNotes)) ReceivedChangesResp

        DeleteNote toDeleteNoteID ->
            { model | notes = model.notes |> exclude (.id >> sameId toDeleteNoteID) }
                |> pure
                |> addToQueue (qDeleteNote toDeleteNoteID) ReceivedChangesResp

        ChangeNewLabelName newName ->
            { model | newLabelName = newName }
                |> pure

        RequestTimeForNewLabelCreation ->
            if String.length model.newLabelName == 0 then
                model |> pure

            else if List.any (\l -> l.name == model.newLabelName) model.labels then
                -- TODO: make this visual to the user in the form of an error
                model |> pure

            else
                let
                    newLabelOfflineId : String
                    newLabelOfflineId =
                        generateUID model.seeds |> Tuple.first

                    newLabel =
                        { id = newLabelOfflineId
                        , name = model.newLabelName
                        }
                in
                ( model, Cmd.batch [ requestRandomValues (), getNewTimeAndCreateLabel newLabel ] )

        CreateNewLabel data time ->
            { model
                | newLabelName = ""
                , labels =
                    { id = OfflineID data.id
                    , name = data.name
                    , updatedAt = time
                    , createdAt = time
                    }
                        :: model.labels
            }
                |> pure
                |> addToQueue (qNewLabel { offlineId = data.id, name = data.name }) ReceivedChangesResp

        ReceivedRandomValues values ->
            { model | seeds = List.map Random.initialSeed values }
                |> pure

        BeginWritingNewNote ->
            { model
                | isWritingANewNote =
                    Just
                        { title = ""
                        , content = ""
                        , labels = Nothing
                        }
            }
                |> pure

        NewTitleChange s ->
            { model
                | isWritingANewNote =
                    Maybe.map
                        (\data ->
                            { data
                                | title = s
                            }
                        )
                        model.isWritingANewNote
            }
                |> pure

        NewNoteContentChange s ->
            { model
                | isWritingANewNote =
                    Maybe.map
                        (\data ->
                            { data | content = s }
                        )
                        model.isWritingANewNote
            }
                |> pure

        SearchLabelsQueryChange s ->
            { model
                | isWritingANewNote =
                    Maybe.map
                        (\data ->
                            { data
                                | labels =
                                    Maybe.map
                                        (\{ labels } ->
                                            { labels = labels
                                            , labelsSearchQuery = s
                                            }
                                        )
                                        data.labels
                            }
                        )
                        model.isWritingANewNote
            }
                |> pure

        AddLabelToNewNote newLabel ->
            { model
                | isWritingANewNote =
                    Maybe.map
                        (\data ->
                            { data
                                | labels =
                                    Maybe.map
                                        (\{ labelsSearchQuery, labels } ->
                                            { labels = newLabel :: labels
                                            , labelsSearchQuery = labelsSearchQuery
                                            }
                                        )
                                        data.labels
                            }
                        )
                        model.isWritingANewNote
            }
                |> pure

        RemoveLabelFromNewNote labelID ->
            { model
                | isWritingANewNote =
                    model.isWritingANewNote
                        |> Maybe.map
                            (\data ->
                                { data
                                    | labels =
                                        data.labels
                                            |> Maybe.map
                                                (\{ labelsSearchQuery, labels } ->
                                                    { labels = labels |> exclude (sameId labelID)
                                                    , labelsSearchQuery = labelsSearchQuery
                                                    }
                                                )
                                }
                            )
            }
                |> pure

        RequestTimeForCreateNewNote ->
            case model.isWritingANewNote of
                Nothing ->
                    model |> pure

                Just newNoteData ->
                    if String.length newNoteData.content == 0 then
                        model |> pure

                    else
                        let
                            newNoteOfflineId =
                                generateUID model.seeds |> Tuple.first

                            newNote =
                                { id = newNoteOfflineId
                                , title = newNoteData.title
                                , content = newNoteData.content
                                , pinned = False
                                , labels =
                                    case newNoteData.labels of
                                        Just { labels } ->
                                            labels

                                        Nothing ->
                                            []
                                }
                        in
                        ( model
                        , Cmd.batch [ requestRandomValues (), getNewTimeForCreateNewNote newNote ]
                        )

        GotCurrentTimeForNewNote noteData time ->
            let
                newNote : Note
                newNote =
                    { id = OfflineID noteData.id
                    , title =
                        if String.length noteData.title == 0 then
                            Nothing

                        else
                            Just noteData.title
                    , content = noteData.content
                    , pinned = noteData.pinned
                    , labels = noteData.labels
                    , createdAt = time
                    , updatedAt = time
                    }
            in
            { model
                | isWritingANewNote = Nothing
                , notes = newNote :: model.notes
            }
                |> pure
                |> addToQueue
                    (qCreateNewNote
                        { offlineId = noteData.id
                        , title = newNote.title
                        , content = newNote.content
                        , pinned = newNote.pinned
                        , labels = newNote.labels
                        }
                    )
                    ReceivedChangesResp

        BeginAddingNewNoteLabels ->
            case model.isWritingANewNote of
                Just data ->
                    { model
                        | isWritingANewNote =
                            Just
                                { data
                                    | labels =
                                        Just
                                            { labels = []
                                            , labelsSearchQuery = ""
                                            }
                                }
                    }
                        |> pure

                Nothing ->
                    model
                        |> pure

        ReceivedChangesResp resp ->
            case resp of
                Ok { deleted, failedToCreate, failedToEdit, justSyncedAt, downSyncedData, justCreatedData } ->
                    ( { model
                        | notes =
                            let
                                ( _, notOutdatedNotes ) =
                                    List.partition
                                        (\e -> List.any (\l -> sameId (DatabaseID l.id) e.id) downSyncedData.notes)
                                        model.notes

                                updatedNotes : List Note
                                updatedNotes =
                                    downSyncedData.notes
                                        |> List.map
                                            (\e ->
                                                { id = DatabaseID e.id
                                                , title = e.title
                                                , content = e.content
                                                , pinned = e.pinned
                                                , createdAt = e.createdAt
                                                , updatedAt = e.updatedAt
                                                , labels = e.labels |> List.map DatabaseID
                                                }
                                            )
                            in
                            notOutdatedNotes
                                -- remove the ones that were failed to create
                                |> exclude (\l -> List.any (\e -> sameId l.id (OfflineID e)) failedToCreate)
                                -- remove the ones that don't exist in DB
                                |> exclude (\l -> List.any (\e -> sameId l.id (DatabaseID e)) deleted.notes)
                                -- update just created
                                |> List.map
                                    (\l ->
                                        case listFirst (\( _, offlineId ) -> sameId l.id (OfflineID offlineId)) justCreatedData.notes of
                                            Just ( v, _ ) ->
                                                { id = DatabaseID v.id
                                                , title = v.title
                                                , content = v.content
                                                , pinned = v.pinned
                                                , createdAt = v.createdAt
                                                , updatedAt = v.updatedAt
                                                , labels = v.labels |> List.map DatabaseID
                                                }

                                            Nothing ->
                                                l
                                    )
                                |> (++) updatedNotes
                        , labels =
                            let
                                ( _, notOutdatedLabels ) =
                                    List.partition
                                        (\e -> List.any (\l -> sameId (DatabaseID l.id) e.id) downSyncedData.labels)
                                        model.labels

                                updatedLabels : List Label
                                updatedLabels =
                                    downSyncedData.labels
                                        |> List.map
                                            (\e ->
                                                { id = DatabaseID e.id
                                                , name = e.name
                                                , createdAt = e.createdAt
                                                , updatedAt = e.updatedAt
                                                }
                                            )
                            in
                            notOutdatedLabels
                                -- remove the ones that were failed to create
                                |> exclude (\l -> List.any (\e -> sameId l.id (OfflineID e)) failedToCreate)
                                -- remove the ones that don't exist in DB
                                |> exclude (\l -> List.any (\e -> sameId l.id (DatabaseID e)) deleted.labels)
                                -- update just created
                                |> List.map
                                    (\l ->
                                        case listFirst (\( _, offlineId ) -> sameId l.id (OfflineID offlineId)) justCreatedData.labels of
                                            Just ( v, _ ) ->
                                                { id = DatabaseID v.id
                                                , name = v.name
                                                , createdAt = v.createdAt
                                                , updatedAt = v.updatedAt
                                                }

                                            Nothing ->
                                                l
                                    )
                                |> (++) updatedLabels
                        , offlineQueue = emptyOfflineQueue
                        , runningQueueOn =
                            if offlineQueueIsEmpty model.offlineQueue then
                                Nothing

                            else
                                Just model.offlineQueue
                        , lastSyncedAt = justSyncedAt
                      }
                    , Cmd.batch
                        [ updateLastSyncedAt (Time.posixToMillis justSyncedAt)
                        , if offlineQueueIsEmpty model.offlineQueue then
                            Cmd.none

                          else
                            Api.sendChanges
                                { operations = queueToOperations model.offlineQueue
                                , lastSyncedAt = justSyncedAt
                                , currentData =
                                    { notes = model.notes |> List.map .id |> labelIDsSplitter |> Tuple.second
                                    , labels = model.labels |> List.map .id |> labelIDsSplitter |> Tuple.second
                                    }
                                }
                                ReceivedChangesResp
                        ]
                    )

                -- TODO: error handling here
                Err _ ->
                    model |> pure



-- FullSyncResp res ->
--     case res of
--         -- TODO:
--         Ok ( notes, labels ) ->
--             { homeModel
--                 | labels =
--                     List.map
--                         (\l ->
--                             { name = l.name
--                             , id = DatabaseID l.id
--                             , createdAt = l.createdAt
--                             , updatedAt = l.updatedAt
--                             }
--                         )
--                         labels
--                 , notes =
--                     List.map
--                         (\l ->
--                             { id = DatabaseID l.id
--                             , title = l.title
--                             , content = l.content
--                             , pinned = l.pinned
--                             , labels = List.map DatabaseID l.labels
--                             , createdAt = l.createdAt
--                             , updatedAt = l.updatedAt
--                             }
--                         )
--                         notes
--             }
--                 |> pure
--         Err v ->
--             -- TODO: handle 403
--             homeModel |> pure


view : Model -> Html Msg
view model =
    div [ css [ displayFlex, flexDirection column, height (pct 100), overflow auto ] ]
        [ nav
            [ css
                [ backgroundColor (rgb 140 20 254)
                , color (rgb 255 255 255)
                , publicSans
                , fontWeight bolder
                , borderBottom3 (px 3) solid (rgb 0 0 0)
                , position sticky
                , top (px 0)
                , displayFlex
                , justifyContent spaceBetween
                ]
            ]
            [ let
                labelsCount =
                    model.labels |> List.length |> String.fromInt
              in
              case model.labelsMenu of
                Just _ ->
                    openLabelsMenuBtn labelsCount

                Nothing ->
                    closedLabelsMenuBtn labelsCount
            , p
                [ css
                    [ fontSize (px 25)
                    ]
                ]
                [ text "Notes" ]
            , (case model.runningQueueOn of
                Nothing ->
                    Outlined.cloud

                Just _ ->
                    Outlined.sync
              )
                28
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        , div [ css [ displayFlex, height (pct 100), overflow hidden ] ]
            [ case model.labelsMenu of
                Just _ ->
                    labelsMenuColumn model

                Nothing ->
                    text ""
            , notesGrid model
            ]
        ]


labelsMenuWidth : Float
labelsMenuWidth =
    277


closedLabelsMenuBtn : String -> Html Msg
closedLabelsMenuBtn labelsCount =
    button
        [ css
            [ padY (px 8)
            , paddingLeft (px 8)
            , maxWidth (px labelsMenuWidth)
            , minWidth (px labelsMenuWidth)
            , backgroundColor white
            , textColor black
            , border (px 0)
            , borderRight3 (px 3) solid black
            , displayFlex
            , publicSans
            , alignItems center
            , justifyContent spaceBetween
            , cursor pointer
            , fontSize (px 16)
            , fontWeight bold
            ]
        , type_ "button"
        , onClick OpenLabelsMenu
        ]
        [ div
            [ css
                [ displayFlex
                , alignItems center
                ]
            ]
            [ Filled.label 32
                Inherit
                |> Svg.Styled.fromUnstyled
            , p [ css [ marginLeft (px 10) ] ] [ text "Labels" ]
            , p [ css [ marginLeft (px 10) ] ] [ text ("(" ++ labelsCount ++ ")") ]
            ]
        , div [ css [ width (px 50), height (px 32), displayFlex, justifyContent center, alignItems center ] ]
            [ Filled.arrow_drop_down 32
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        ]


openLabelsMenuBtn : String -> Html Msg
openLabelsMenuBtn labelsCount =
    div
        [ css
            [ paddingLeft (px 8)
            , maxWidth (px labelsMenuWidth)
            , minWidth (px labelsMenuWidth)
            , backgroundColor black
            , textColor white
            , border (px 0)
            , borderRight3 (px 3) solid black
            , displayFlex
            , publicSans
            , alignItems center
            , justifyContent spaceBetween
            , fontSize (px 16)
            , fontWeight bold
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , alignItems center
                , padY (px 8)
                ]
            ]
            [ Filled.label 32
                Inherit
                |> Svg.Styled.fromUnstyled
            , p [ css [ marginLeft (px 10) ] ] [ text "Labels" ]
            , p [ css [ marginLeft (px 10) ] ] [ text ("(" ++ labelsCount ++ ")") ]
            ]
        , button
            [ css
                [ width (px 50)
                , height (pct 100)
                , displayFlex
                , justifyContent center
                , alignItems center
                , border (px 0)
                , backgroundColor error
                , cursor pointer
                , textColor white
                ]
            , type_ "button"
            , onClick CloseLabelsMenu
            ]
            [ Filled.close 32
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        ]


labelsMenuColumn : Model -> Html Msg
labelsMenuColumn { labels, filters, labelsMenu } =
    div
        [ css
            [ maxWidth (px labelsMenuWidth)
            , minWidth (px labelsMenuWidth)
            , backgroundColor secondary
            , height (pct 100)
            , borderRight3 (px 3) solid black
            , displayFlex
            , flexDirection column
            ]
        ]
        [ div
            [ css
                [ height (px 48)
                , displayFlex
                , alignItems center
                , borderBottom3 (px 2) solid black
                , backgroundColor primary
                ]
            ]
            [ label
                [ css [ padX (px 10), height (pct 100), displayFlex, alignItems center ]
                , for "search-labels"
                ]
                [ Outlined.search 24 Inherit
                    |> Svg.Styled.fromUnstyled
                ]
            , input
                [ css
                    [ fontWeight bold
                    , publicSans
                    , height (pct 100)
                    , width (pct 100)
                    , border (px 0)
                    , fontSize (px 16)
                    , backgroundColor transparent
                    ]
                , value (Maybe.withDefault "" labelsMenu)
                , onInput ChangeLabelsSearchQuery
                , id "search-labels"
                , placeholder "Search labels..."
                ]
                []
            , button
                [ css
                    [ width (px 80)
                    , height (pct 100)
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , border (px 0)
                    , backgroundColor white
                    , cursor pointer
                    , textColor black
                    , borderLeft3 (px 3) solid black
                    ]
                , type_ "button"
                , onClick GoToEditLabelsScreen
                ]
                [ Filled.edit 28 Inherit
                    |> Svg.Styled.fromUnstyled
                ]
            ]
        , ul
            [ css
                [ fontWeight (int 600)
                , height (pct 100)
                , overflowY auto
                , displayFlex
                , flexDirection column
                ]
            ]
            (List.indexedMap
                (\i e ->
                    button
                        [ css
                            ([ paddingLeft (px 10)
                             , padY (px 5)
                             , cursor pointer
                             , hover [ textColor white, backgroundColor black ]
                             , publicSans
                             , border (px 0)
                             , fontSize (px 16)
                             , cursor pointer
                             , backgroundColor transparent
                             , fontWeight (int 600)
                             , textColor inherit
                             , userSelectNone
                             ]
                                ++ (case filters.label of
                                        Nothing ->
                                            []

                                        Just v ->
                                            if sameId v e.id then
                                                [ textColor white, backgroundColor black ]

                                            else
                                                []
                                   )
                                ++ (if i == 0 then
                                        [ paddingTop (px 10) ]

                                    else
                                        []
                                   )
                            )
                        , type_ "button"
                        , onClick (SelectLabelToFilterBy e.id)
                        ]
                        [ p
                            [ css
                                [ whiteSpace noWrap
                                , textOverflow ellipsis
                                , overflow hidden
                                , textAlign start
                                ]
                            ]
                            [ text e.name ]
                        ]
                )
                (labels
                    |> List.sortBy (.createdAt >> Time.posixToMillis)
                    |> (\l ->
                            case labelsMenu of
                                Just filterQuery ->
                                    l |> List.filter (\e -> String.contains (filterQuery |> String.toLower) (e.name |> String.toLower))

                                Nothing ->
                                    l
                       )
                )
            )
        ]


notesGrid : Model -> Html Msg
notesGrid model =
    div [ css [ width (pct 100), overflowY auto ] ]
        [ div []
            (case model.isWritingANewNote of
                Nothing ->
                    [ div [ css [ fullWidth, displayFlex, marginTop (px 30) ] ]
                        [ div [ css [ margin2 (px 0) auto, width (px 500) ] ]
                            [ input
                                [ onClick BeginWritingNewNote
                                , css
                                    [ border3 (px 3) solid (rgb 0 0 0)
                                    , publicSans
                                    , fontWeight bold
                                    , padding (px 8)
                                    , margin2 (px 0) auto
                                    , fullWidth
                                    , backgroundColor (rgb 255 203 127)
                                    ]
                                , placeholder "TAKE A NEW NOTE"
                                ]
                                []
                            ]
                        ]
                    ]

                Just data ->
                    [ div
                        [ css
                            [ displayFlex
                            , marginTop (px 30)
                            ]
                        ]
                        [ form
                            [ css
                                [ displayFlex
                                , margin2 auto auto
                                , flexDirection column
                                , border3 (px 3) solid (rgb 0 0 0)
                                , hover [ boxShadow4 (px 6) (px 6) (px 0) (rgb 0 0 0) ]
                                , margin2 (px 0) auto
                                , minWidth (px 500)
                                ]
                            , onSubmit RequestTimeForCreateNewNote
                            ]
                            [ -- TODO: Add focus on input task
                              input
                                [ css
                                    [ publicSans
                                    , border (px 0)
                                    , backgroundColor (rgb 255 203 127)
                                    , padding (px 8)
                                    , fontSize (px 16)
                                    , margin2 (px 0) auto
                                    , fullWidth
                                    ]
                                , placeholder "Grocery List"
                                , onInput NewTitleChange
                                , value data.title
                                ]
                                []
                            , textarea
                                [ css
                                    [ backgroundColor (rgb 255 203 127)
                                    , border (px 0)
                                    , publicSans
                                    , padding (px 8)
                                    , fontSize (px 16)
                                    , margin2 (px 0) auto
                                    , fullWidth
                                    , minWidth (px 494)
                                    , minHeight (px 150)
                                    ]
                                , placeholder "Milk, eggs, bread, and fruits."
                                , onInput NewNoteContentChange
                                , value data.content
                                ]
                                []
                            , case data.labels of
                                Just { labels, labelsSearchQuery } ->
                                    case model.labels of
                                        [] ->
                                            -- TODO: design empty state
                                            form
                                                [ css [ displayFlex, flexDirection column, publicSans, color (hex "fff"), padding (px 15) ]
                                                , onSubmit RequestTimeForNewLabelCreation
                                                ]
                                                [ text "No labels, create some"
                                                , label []
                                                    [ text "Label:"
                                                    , input
                                                        [ placeholder "School"
                                                        , onInput ChangeNewLabelName
                                                        ]
                                                        []
                                                    ]
                                                , button
                                                    [ type_ "submit"
                                                    , Html.Styled.Attributes.disabled (String.length model.newLabelName == 0)
                                                    ]
                                                    [ text "Create label" ]
                                                ]

                                        _ ->
                                            div [ css [ marginTop (px 8) ] ]
                                                [ label [ css [ color (hex "fff"), mx (px 15) ] ] [ text "Labels:" ]
                                                , input [ placeholder "Search label", value labelsSearchQuery, onInput SearchLabelsQueryChange ] []
                                                , div
                                                    []
                                                    -- TODO: fix styles
                                                    (List.map
                                                        (\l ->
                                                            button
                                                                [ css [ margin (px 5), padding2 (px 5) (px 10) ]
                                                                , onClick (AddLabelToNewNote l.id)
                                                                , type_ "button"
                                                                ]
                                                                [ text l.name ]
                                                        )
                                                        (model.labels
                                                            |> exclude (\l -> List.any (\j -> j == l.id) labels)
                                                            |> List.filter (.name >> String.toLower >> String.contains labelsSearchQuery)
                                                        )
                                                    )
                                                , div [ css [ color (hex "fff"), mx (px 15) ] ] [ text "Selected labels:" ]
                                                , div
                                                    []
                                                    (List.map
                                                        (\l ->
                                                            button
                                                                [ css
                                                                    [ margin (px 5), padding2 (px 5) (px 10) ]
                                                                , onClick (RemoveLabelFromNewNote l.id)
                                                                , type_ "button"
                                                                ]
                                                                [ text l.name ]
                                                        )
                                                        (List.filter (\r -> List.any (\e -> e == r.id) labels) model.labels)
                                                    )
                                                ]

                                Nothing ->
                                    div []
                                        [ button
                                            [ -- TODO: style
                                              css [ padding (px 15) ]
                                            , onClick BeginAddingNewNoteLabels
                                            , type_ "button"
                                            ]
                                            [ text "Add label" ]
                                        ]
                            , button
                                [ css
                                    [ padding (px 15)
                                    , fontSize (px 16)
                                    ]
                                , type_ "submit"
                                , Html.Styled.Attributes.disabled (String.length data.content == 0)
                                ]
                                [ text "Create note" ]
                            ]
                        ]
                    ]
            )
        , case model.notes of
            [] ->
                -- TODO: add no notes empty state design
                div [] []

            _ ->
                div
                    [ -- TODO: give the tiled effect of google keep
                      -- using translate and transitions
                      css
                        [ displayFlex
                        , flexDirection row
                        , flexWrap wrap
                        , marginTop (px 30)
                        , alignItems flexStart
                        ]
                    ]
                    (List.map (note model)
                        (model.notes
                            |> prioritizePinned
                            |> (\e ->
                                    case model.filters.label of
                                        Just label ->
                                            e |> List.filter (\l -> List.any (\j -> j == label) l.labels)

                                        Nothing ->
                                            e
                               )
                        )
                    )
        ]


note : Model -> Note -> Html Msg
note model data =
    let
        noteTitle =
            case data.title of
                Nothing ->
                    text ""

                Just title ->
                    div
                        [ css
                            [ delaGothicOne
                            , borderBottom3 (px 1) solid (rgb 0 0 0)
                            , padding (px 10)
                            ]
                        ]
                        [ text title ]

        content =
            p [ css [ publicSans, padding (px 10) ] ]
                (let
                    -- NOTE: \n doesn't break into a newline so I do this
                    makeParagraph : List (Html msg) -> List String -> List (Html msg)
                    makeParagraph total next =
                        case next of
                            [] ->
                                total

                            x :: xs ->
                                makeParagraph (total ++ [ br [] [], text x ]) xs
                 in
                 data.content
                    |> String.split "\n"
                    |> (\r ->
                            case r of
                                [] ->
                                    [ text data.content ]

                                h :: t ->
                                    if List.length t > 0 then
                                        makeParagraph [ text h ] t

                                    else
                                        [ text h ]
                       )
                )

        labelsFooter =
            case data.labels of
                [] ->
                    div [] []

                labels ->
                    div
                        [ css
                            [ padding (px 10)
                            , displayFlex
                            , flexWrap wrap
                            , gap 5
                            ]
                        ]
                        (List.map
                            (\l ->
                                div
                                    [ css
                                        [ backgroundColor (hex "#6ac0ff")
                                        , padding (px 2)
                                        , border3 (px 1) solid (rgb 0 0 0)
                                        , hover [ boxShadow4 (px 3) (px 3) (px 0) (rgb 0 0 0) ]
                                        , displayFlex
                                        ]
                                    , class "note-label"
                                    ]
                                    [ text l.name
                                    , button
                                        [ class "note-label-remove-button"
                                        , css
                                            [ border3 (px 1) solid (rgb 0 0 0)
                                            , padding2 (px 0) (px 2)
                                            , marginLeft (px 3)
                                            , backgroundColor (hex "ff0000")
                                            , color (hex "fff")
                                            , cursor pointer
                                            ]
                                        , type_ "button"
                                        , onClick
                                            (RemoveLabelFromNote
                                                { noteID = data.id
                                                , labelID = l.id
                                                }
                                            )
                                        ]
                                        [ text "X" ]
                                    ]
                            )
                            (model.labels |> List.filter (\e -> List.any (\r -> r == e.id) labels))
                        )
    in
    div
        [ css
            [ border3 (px 3) solid (rgb 0 0 0)
            , margin (px 10)
            , displayFlex
            , flexDirection column
            , maxWidth (px 240)
            , minWidth (px 240)
            , minHeight (px 120)
            , backgroundColor (rgb 255 203 127)
            , hover
                [ boxShadow4 (px 6) (px 6) (px 0) (rgb 0 0 0) ]
            ]
        ]
        [ noteTitle
        , content
        , labelsFooter
        ]


prioritizePinned : List Note -> List Note
prioritizePinned notes =
    let
        pinned =
            List.filter (\n -> n.pinned == True) notes

        unpinned =
            List.filter (\n -> n.pinned == False) notes
    in
    pinned ++ unpinned
