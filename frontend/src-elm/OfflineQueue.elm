module OfflineQueue exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import DataTypes exposing (Label, Note)
import Helpers exposing (exclude, labelIDsSplitter, or, partitionFirst, sameId)
import Http
import Svg.Attributes exposing (order)
import Time exposing (Posix)


type alias OfflineQueueOps =
    { createLabels : List OQCreateLabel
    , deleteLabels : List OQDeleteLabel
    , createNotes : List OQCreateNote
    , deleteNotes : List OQDeleteNote
    , editNotes : List OQEditNote
    , changeLabelNames : List OQChangeLabelName
    }


emptyOfflineQueue : OfflineQueueOps
emptyOfflineQueue =
    { createLabels = []
    , deleteLabels = []
    , createNotes = []
    , deleteNotes = []
    , editNotes = []
    , changeLabelNames = []
    }


type alias OQCreateLabel =
    { offlineId : String, name : String }


type alias OQDeleteLabel =
    Api.SyncableID


type alias OQCreateNote =
    { id : String
    , title : Maybe String
    , content : String
    , pinned : Bool
    , labels : List SyncableID
    , order : Maybe Int
    }


type alias OQDeleteNote =
    SyncableID


type alias OQEditNote =
    { id : SyncableID
    , title : Maybe String
    , content : Maybe String
    , pinned : Maybe Bool
    , labels : Maybe (List SyncableID)
    , order : Maybe Int
    }


type alias OQChangeLabelName =
    { name : String
    , id : Api.SyncableID
    }


type Action
    = QToggleNotePin SyncableID Bool
    | QEditNoteLabels SyncableID (Maybe (List SyncableID))
    | QEditNote OQEditNote
    | QDeleteNote OQDeleteNote
    | QNewLabel OQCreateLabel
    | QCreateNewNote OQCreateNote
    | QDeleteLabel OQDeleteLabel
    | QEditLabelName OQChangeLabelName
    | QDeleteLabels (List OQDeleteLabel)


actionMapToFn : Action -> OfflineQueueOps -> OfflineQueueOps
actionMapToFn action =
    case action of
        QToggleNotePin uid newPinnedVal ->
            qToggleNotePin uid newPinnedVal

        QEditNoteLabels uid newLabels ->
            qEditNoteLabels uid newLabels

        QEditNote editData ->
            qEditNote editData

        QDeleteNote uid ->
            qDeleteNote uid

        QNewLabel newLabelData ->
            qNewLabel newLabelData

        QCreateNewNote newNoteData ->
            qCreateNewNote newNoteData

        QDeleteLabel labelId ->
            qDeleteLabel labelId

        QEditLabelName editData ->
            qEditLabelName editData

        QDeleteLabels labels ->
            qDeleteLabels labels


queueToOperations : OfflineQueueOps -> List Operation
queueToOperations { createLabels, deleteLabels, createNotes, deleteNotes, editNotes, changeLabelNames } =
    let
        ifNotEmpty l e =
            if List.isEmpty l then
                []

            else
                [ e l ]

        ifNotEmpty1 l e =
            if List.isEmpty l then
                []

            else
                e
    in
    ifNotEmpty deleteLabels DeleteLabels
        ++ ifNotEmpty deleteNotes DeleteNotes
        ++ ifNotEmpty createLabels CreateLabels
        ++ ifNotEmpty createNotes CreateNotes
        ++ ifNotEmpty1 editNotes (List.map EditNote editNotes)
        ++ ifNotEmpty1 changeLabelNames (List.map ChangeLabelName changeLabelNames)


offlineQueueIsEmpty : OfflineQueueOps -> Bool
offlineQueueIsEmpty { createLabels, deleteLabels, createNotes, deleteNotes, editNotes, changeLabelNames } =
    List.isEmpty createLabels
        && List.isEmpty deleteLabels
        && List.isEmpty createNotes
        && List.isEmpty deleteNotes
        && List.isEmpty editNotes
        && List.isEmpty changeLabelNames


qNewLabel : OQCreateLabel -> OfflineQueueOps -> OfflineQueueOps
qNewLabel { offlineId, name } queue =
    { queue | createLabels = { offlineId = offlineId, name = name } :: queue.createLabels }


qDeleteLabel : OQDeleteLabel -> OfflineQueueOps -> OfflineQueueOps
qDeleteLabel labelId queue =
    let
        ( toCreateLabelInQueue, restCreateLabels ) =
            queue.createLabels
                |> partitionFirst (\l -> sameId (OfflineID l.offlineId) labelId)
    in
    case toCreateLabelInQueue of
        -- hasn't created label yet
        Just _ ->
            { queue
                | createLabels = restCreateLabels
                , changeLabelNames = queue.changeLabelNames |> exclude (.id >> sameId labelId)
                , createNotes =
                    queue.createNotes
                        |> List.map
                            (\l ->
                                { l
                                    | labels = l.labels |> exclude (sameId labelId)
                                }
                            )
                , editNotes =
                    queue.editNotes
                        |> List.map
                            (\l ->
                                { l
                                    | labels = l.labels |> Maybe.map (exclude (sameId labelId))
                                }
                            )
            }

        -- has already created the label or creation is in progress
        Nothing ->
            { queue
                | deleteLabels = labelId :: queue.deleteLabels
            }


qDeleteLabels : List OQDeleteLabel -> OfflineQueueOps -> OfflineQueueOps
qDeleteLabels labels queue =
    List.foldl (\l q -> qDeleteLabel l q) queue labels


qCreateNewNote : OQCreateNote -> OfflineQueueOps -> OfflineQueueOps
qCreateNewNote data queue =
    { queue | createNotes = data :: queue.createNotes }


qEditNote : OQEditNote -> OfflineQueueOps -> OfflineQueueOps
qEditNote data queue =
    let
        ( toCreateNote, restCreateNotes ) =
            queue.createNotes
                |> partitionFirst (\l -> sameId (OfflineID l.id) data.id)
    in
    case toCreateNote of
        Just createData ->
            -- hasn't even been created so just combine edit as create
            { queue
                | createNotes =
                    { id = createData.id
                    , title = data.title |> or createData.title
                    , content = Maybe.withDefault createData.content data.content
                    , pinned = Maybe.withDefault createData.pinned data.pinned
                    , labels = Maybe.withDefault createData.labels data.labels
                    , order = data.order |> or createData.order
                    }
                        :: restCreateNotes
            }

        Nothing ->
            let
                ( toEditNote, restEditNotes ) =
                    queue.editNotes
                        |> partitionFirst (\l -> sameId l.id data.id)
            in
            case toEditNote of
                -- already a previous op to edit note
                Just prevEdit ->
                    { queue
                        | editNotes =
                            { id = prevEdit.id
                            , title = data.title |> or prevEdit.title
                            , content = data.content |> or prevEdit.content
                            , pinned = data.pinned |> or prevEdit.pinned
                            , labels = data.labels |> or prevEdit.labels
                            , order = data.order |> or prevEdit.order
                            }
                                :: restEditNotes
                    }

                Nothing ->
                    { queue | editNotes = data :: queue.editNotes }


qEditNoteLabels : SyncableID -> Maybe (List SyncableID) -> OfflineQueueOps -> OfflineQueueOps
qEditNoteLabels id labels =
    qEditNote
        { id = id
        , title = Nothing
        , content = Nothing
        , pinned = Nothing
        , labels = labels
        , order = Nothing
        }


qToggleNotePin : SyncableID -> Bool -> OfflineQueueOps -> OfflineQueueOps
qToggleNotePin id newPinnedVal =
    qEditNote
        { id = id
        , title = Nothing
        , content = Nothing
        , pinned = Just newPinnedVal
        , labels = Nothing
        , order = Nothing
        }


qDeleteNote : OQDeleteNote -> OfflineQueueOps -> OfflineQueueOps
qDeleteNote noteId queue =
    let
        ( toCreateNoteInQueue, restCreateNotes ) =
            queue.createNotes
                |> partitionFirst (\l -> sameId (OfflineID l.id) noteId)
    in
    case toCreateNoteInQueue of
        -- hasn't created note yet
        Just _ ->
            { queue
                | createNotes = restCreateNotes
                , editNotes =
                    queue.editNotes
                        |> exclude (.id >> sameId noteId)
            }

        -- has already created the note or creation is in progress
        Nothing ->
            { queue
                | deleteNotes = noteId :: queue.deleteNotes
            }


qEditLabelName : OQChangeLabelName -> OfflineQueueOps -> OfflineQueueOps
qEditLabelName editData queue =
    let
        ( toEditLabel, restEditLabel ) =
            queue.changeLabelNames
                |> partitionFirst (\l -> sameId l.id editData.id)
    in
    case toEditLabel of
        -- already a previous op to edit label name
        Just _ ->
            { queue | changeLabelNames = editData :: restEditLabel }

        Nothing ->
            { queue | changeLabelNames = editData :: queue.changeLabelNames }
