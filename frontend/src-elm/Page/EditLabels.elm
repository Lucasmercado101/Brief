module Page.EditLabels exposing (..)

import Api exposing (SyncableID(..))
import Cmd.Extra exposing (pure)
import DataTypes exposing (Label, Note)
import Helpers exposing (exclude, labelIDsSplitter, listFirst, sameId)
import Http
import OfflineQueue exposing (OfflineQueueOps, addToQueue, emptyOfflineQueue, offlineQueueIsEmpty, qDeleteLabel, qDeleteLabels, qEditLabelName, qNewLabel, queueToOperations)
import Ports exposing (requestRandomValues, updateLastSyncedAt)
import Random
import Task
import Time exposing (Posix)
import UID exposing (generateUID)



-- Subs


getNewTimeAndCreateLabel : { id : String, name : String } -> Cmd Msg
getNewTimeAndCreateLabel data =
    Time.now
        |> Task.perform (CreateNewLabel data)


type EditLabelKind
    = Selected SyncableID
    | ConfirmDelete SyncableID
    | Editing SyncableID String


type alias Model =
    { seeds : List Random.Seed
    , selected : List EditLabelKind
    , searchQuery : String
    , confirmDeleteAllSelectedLabels : Bool

    -- global data
    , labels : List Label
    , notes : List Note

    -- sync stuff
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    , lastSyncedAt : Posix
    }


type Msg
    = ExitEditingLabelsView
    | ChangeEditLabelsSearchQuery String
    | CreateNewLabelEditLabelsView
    | CreateNewLabel { id : String, name : String } Posix
    | SelectLabel SyncableID
    | ClearEditLabelsSelections
    | RequestDeleteLabel SyncableID
    | ConfirmDeleteLabel SyncableID
    | CancelDeleteLabel SyncableID
    | EditLabel ( SyncableID, String )
    | ChangeEditingLabelName ( SyncableID, String )
    | ConfirmEditingLabelName ( SyncableID, String )
    | CancelEditingLabelName SyncableID
    | RemoveLabelFromSelected SyncableID
    | RequestConfirmDeleteMultipleLabels
    | ConfirmDeleteMultipleLabels
    | CancelDeleteMultipleLabels
    | ReceivedChangesResp (Result Http.Error Api.ChangesResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExitEditingLabelsView ->
            -- TODO:
            model
                -- { model | editLabelsScreen = Nothing }
                |> pure

        ChangeEditLabelsSearchQuery newQuery ->
            { model | searchQuery = newQuery }
                |> pure

        CreateNewLabelEditLabelsView ->
            if String.length model.searchQuery == 0 then
                model |> pure

            else if List.any (\l -> l.name == model.searchQuery) model.labels then
                -- TODO: make this visual to the user in the form of an error
                model |> pure

            else
                let
                    newLabelOfflineId : String
                    newLabelOfflineId =
                        generateUID model.seeds |> Tuple.first

                    newLabel =
                        { id = newLabelOfflineId
                        , name = model.searchQuery
                        }
                in
                ( { model | searchQuery = "" }
                , Cmd.batch [ requestRandomValues (), getNewTimeAndCreateLabel newLabel ]
                )

        CreateNewLabel data time ->
            { model
                | labels =
                    { id = OfflineID data.id
                    , name = data.name
                    , updatedAt = time
                    , createdAt = time
                    }
                        :: model.labels
            }
                |> pure
                |> addToQueue (qNewLabel { offlineId = data.id, name = data.name }) ReceivedChangesResp

        SelectLabel id ->
            let
                sameIdOnSelected =
                    \e ->
                        case e of
                            Selected i ->
                                sameId id i

                            ConfirmDelete i ->
                                sameId id i

                            Editing i _ ->
                                sameId id i
            in
            { model
                | selected =
                    if List.any sameIdOnSelected model.selected then
                        model.selected |> exclude sameIdOnSelected

                    else
                        Selected id :: model.selected
            }
                |> pure

        ClearEditLabelsSelections ->
            { model | selected = [] }
                |> pure

        RequestDeleteLabel id ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        if sameId i id then
                                            ConfirmDelete i

                                        else
                                            e

                                    ConfirmDelete _ ->
                                        e

                                    Editing _ _ ->
                                        e
                            )
            }
                |> pure

        ConfirmDeleteLabel id ->
            { model
                | selected =
                    model.selected
                        |> List.filter
                            (\e ->
                                case e of
                                    Selected i ->
                                        True

                                    ConfirmDelete i ->
                                        if sameId i id then
                                            False

                                        else
                                            True

                                    Editing _ _ ->
                                        True
                            )
            }
                |> pure
                |> addToQueue (qDeleteLabel id) ReceivedChangesResp

        CancelDeleteLabel id ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        e

                                    ConfirmDelete i ->
                                        if sameId i id then
                                            Selected i

                                        else
                                            e

                                    Editing _ _ ->
                                        e
                            )
            }
                |> pure

        EditLabel ( id, newName ) ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        if sameId i id then
                                            Editing id newName

                                        else
                                            e

                                    ConfirmDelete i ->
                                        e

                                    Editing _ _ ->
                                        e
                            )
            }
                |> pure

        ChangeEditingLabelName ( id, newName ) ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        e

                                    ConfirmDelete i ->
                                        e

                                    Editing i _ ->
                                        if sameId i id then
                                            Editing id newName

                                        else
                                            e
                            )
            }
                |> pure

        ConfirmEditingLabelName ( id, newName ) ->
            if newName == "" then
                model |> pure

            else if List.any (\l -> (l.name |> String.toLower) == (newName |> String.toLower)) model.labels then
                -- TODO: show message to the user "This label already exists"
                -- and disable create button
                model |> pure

            else
                { model
                    | labels =
                        model.labels
                            |> List.map
                                (\l ->
                                    if sameId l.id id then
                                        { l | name = newName }

                                    else
                                        l
                                )
                    , selected =
                        model.selected
                            |> List.map
                                (\e ->
                                    case e of
                                        Selected i ->
                                            e

                                        ConfirmDelete i ->
                                            e

                                        Editing i _ ->
                                            if sameId i id then
                                                Selected i

                                            else
                                                e
                                )
                }
                    |> pure
                    |> addToQueue (qEditLabelName { name = newName, id = id }) ReceivedChangesResp

        CancelEditingLabelName id ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        e

                                    ConfirmDelete i ->
                                        e

                                    Editing i _ ->
                                        if sameId i id then
                                            Selected i

                                        else
                                            e
                            )
            }
                |> pure

        RemoveLabelFromSelected id ->
            { model
                | selected =
                    model.selected
                        |> exclude
                            (\e ->
                                case e of
                                    Selected i ->
                                        sameId id i

                                    ConfirmDelete i ->
                                        sameId id i

                                    Editing i _ ->
                                        sameId id i
                            )
            }
                |> pure

        RequestConfirmDeleteMultipleLabels ->
            { model | confirmDeleteAllSelectedLabels = True }
                |> pure

        CancelDeleteMultipleLabels ->
            { model | confirmDeleteAllSelectedLabels = False }
                |> pure

        ConfirmDeleteMultipleLabels ->
            let
                deletedIds : List SyncableID
                deletedIds =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        i

                                    ConfirmDelete i ->
                                        i

                                    Editing i _ ->
                                        i
                            )
            in
            { model
                | labels =
                    model.labels
                        |> exclude (\l -> List.any (sameId l.id) deletedIds)
                , selected = []
                , confirmDeleteAllSelectedLabels = False
            }
                |> pure
                |> addToQueue (qDeleteLabels deletedIds) ReceivedChangesResp

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
