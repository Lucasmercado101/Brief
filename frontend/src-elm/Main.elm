port module Main exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import Browser
import Cmd.Extra exposing (pure)
import Css exposing (..)
import Either exposing (Either(..))
import Html.Styled exposing (Html, br, button, div, form, input, label, nav, p, text, textarea)
import Html.Styled.Attributes exposing (class, css, disabled, id, placeholder, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Random
import Random.Char
import Random.Extra
import Random.String
import Svg.Styled
import Task
import Time exposing (Posix)



-- PORTS


port requestRandomValues : () -> Cmd msg


port updateLastSyncedAt : Int -> Cmd msg


port receiveRandomValues : (List Int -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveRandomValues ReceivedRandomValues
        |> Sub.map LoggedInView


dummyNewNote : Maybe NewNoteData
dummyNewNote =
    Just
        { title = ""
        , content = ""
        , labels = Nothing
        }


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
        |> Cmd.map LoggedInView


getNewTimeAndCreateLabel : { id : String, name : String } -> Cmd Msg
getNewTimeAndCreateLabel data =
    Task.perform (CreateNewLabel data) Time.now
        |> Cmd.map LoggedInView



-- MODEL


idDiff : SyncableID -> SyncableID -> Bool
idDiff a b =
    case ( a, b ) of
        ( OfflineID c, OfflineID d ) ->
            c /= d

        ( DatabaseID c, DatabaseID d ) ->
            c /= d

        _ ->
            True


sameId : SyncableID -> SyncableID -> Bool
sameId a b =
    not (idDiff a b)


type alias UniqueStr =
    String


type alias Note =
    { id : SyncableID
    , title : Maybe String
    , content : String
    , pinned : Bool
    , createdAt : Posix
    , updatedAt : Posix
    , labels : List SyncableID
    }


type alias Label =
    { id : SyncableID
    , name : UniqueStr
    , createdAt : Posix
    , updatedAt : Posix
    }


type alias LoggedOutModel =
    { username : String
    , password : String
    }


type User
    = LoggedOut LoggedOutModel
    | CheckingSessionValidity
    | LoggedIn


type alias OQCreateLabel =
    { offlineId : String, name : String }


type alias OQDeleteLabel =
    Api.SyncableID


type alias OQCreateNote =
    { offlineId : String
    , title : Maybe String
    , content : String
    , pinned : Bool
    , labels : List SyncableID
    }


type alias OQDeleteNote =
    SyncableID


type alias OQEditNote =
    { id : SyncableID
    , title : Maybe String
    , content : Maybe String
    , pinned : Maybe Bool
    , labels : Maybe (List SyncableID)
    }


type alias OQChangeLabelName =
    { name : String
    , id : Api.SyncableID
    }


type alias OfflineQueueOps =
    { createLabels : List OQCreateLabel
    , deleteLabels : List OQDeleteLabel
    , createNotes : List OQCreateNote
    , deleteNotes : List OQDeleteNote
    , editNotes : List OQEditNote
    , changeLabelNames : List OQChangeLabelName
    }


type alias Model =
    { seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    , isWritingANewNote : Maybe NewNoteData
    , newLabelName : String
    , user : User
    , labelsMenu : Bool

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



-- MESSAGE


type LoggedOutMsg
    = UsernameChange String
    | PasswordChange String
    | Login
    | LoginRes (Result Http.Error ())


type LoggedInMsg
    = ChangeNotePinned ( SyncableID, Bool )
      -- Labels menu
    | OpenLabelsMenu Bool
      --
    | NewTitleChange String
    | NewNoteContentChange String
    | RequestTimeForNewLabelCreation
    | CreateNewLabel { id : String, name : String } Posix
    | ReceivedRandomValues (List Int)
    | DeleteNote SyncableID
    | DeleteLabel SyncableID
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


type Msg
    = LoggedOutView LoggedOutMsg
    | LoggedInView LoggedInMsg
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)



-- INIT


type alias Flags =
    { seeds : List Int, hasSessionCookie : Bool, lastSyncedAt : Int }


emptyOfflineQueue =
    { createLabels = []
    , deleteLabels = []
    , createNotes = []
    , deleteNotes = []
    , editNotes = []
    , changeLabelNames = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds =
            List.map Random.initialSeed flags.seeds
    in
    ( { seeds = seeds
      , notes = []
      , isWritingANewNote = Nothing
      , newLabelName = ""
      , labels = []
      , labelsMenu = False
      , user =
            if flags.hasSessionCookie then
                LoggedIn

            else
                LoggedOut
                    { username = ""
                    , password = ""
                    }

      -- sync stuff
      , offlineQueue = emptyOfflineQueue
      , runningQueueOn = Nothing
      , lastSyncedAt = Time.millisToPosix flags.lastSyncedAt
      }
      -- TODO: full-sync with regards to indexedDb
    , if flags.hasSessionCookie then
        Api.fullSync FullSyncResp

      else
        Cmd.none
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: sync with indexedDB
    case model.user of
        LoggedOut { username, password } ->
            case msg of
                LoggedOutView loggedOutMsg ->
                    case loggedOutMsg of
                        UsernameChange newUsername ->
                            { model
                                | user =
                                    LoggedOut
                                        { username = newUsername
                                        , password = password
                                        }
                            }
                                |> pure

                        PasswordChange newPassword ->
                            { model
                                | user =
                                    LoggedOut
                                        { username = username
                                        , password = newPassword
                                        }
                            }
                                |> pure

                        Login ->
                            if username == "" || password == "" then
                                model |> pure

                            else
                                ( model, Api.logIn username password LoginRes |> Cmd.map LoggedOutView )

                        LoginRes res ->
                            case res of
                                Ok v ->
                                    ( { model | user = LoggedIn }, Api.fullSync FullSyncResp )

                                Err v ->
                                    -- TODO: err handling
                                    model |> pure

                _ ->
                    model |> pure

        CheckingSessionValidity ->
            -- TODO:
            model |> pure

        LoggedIn ->
            case msg of
                LoggedInView loggedInMsg ->
                    case loggedInMsg of
                        OpenLabelsMenu open ->
                            { model | labelsMenu = open }
                                |> pure

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
                                |> addToQueue (qToggleNotePin uid (Just newPinnedVal))

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
                                        |> addToQueue (qEditNoteLabels noteID (Just newNotes))

                        DeleteNote toDeleteNoteID ->
                            { model | notes = model.notes |> exclude (.id >> sameId toDeleteNoteID) }
                                |> pure
                                |> addToQueue (qDeleteNote toDeleteNoteID)

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
                                |> addToQueue (qNewLabel { offlineId = data.id, name = data.name })

                        ReceivedRandomValues values ->
                            { model | seeds = List.map Random.initialSeed values }
                                |> pure

                        DeleteLabel labelId ->
                            { model | labels = model.labels |> exclude (.id >> sameId labelId) }
                                |> pure
                                |> addToQueue (qDeleteLabel labelId)

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
                                                |> Cmd.map LoggedInView
                                        ]
                                    )

                                -- TODO: error handling here
                                Err _ ->
                                    model |> pure

                -- TODO: Change later
                LoggedOutView _ ->
                    ( model, Cmd.none )

                FullSyncResp res ->
                    case res of
                        -- TODO:
                        Ok ( notes, labels ) ->
                            { model
                                | labels =
                                    List.map
                                        (\l ->
                                            { name = l.name
                                            , id = DatabaseID l.id
                                            , createdAt = l.createdAt
                                            , updatedAt = l.updatedAt
                                            }
                                        )
                                        labels
                                , notes =
                                    List.map
                                        (\l ->
                                            { id = DatabaseID l.id
                                            , title = l.title
                                            , content = l.content
                                            , pinned = l.pinned
                                            , labels = List.map DatabaseID l.labels
                                            , createdAt = l.createdAt
                                            , updatedAt = l.updatedAt
                                            }
                                        )
                                        notes
                            }
                                |> pure

                        Err v ->
                            -- TODO: handle 403
                            model |> pure



-- QUEUE


addToQueue : (OfflineQueueOps -> OfflineQueueOps) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToQueue fn ( model, cmds ) =
    let
        currentOperations =
            model.offlineQueue |> fn
    in
    case model.runningQueueOn of
        Nothing ->
            ( { model
                | offlineQueue = emptyOfflineQueue
                , runningQueueOn = Just currentOperations
              }
            , Cmd.batch
                [ Api.sendChanges
                    { operations = queueToOperations currentOperations
                    , lastSyncedAt = model.lastSyncedAt
                    , currentData =
                        { notes = model.notes |> List.map .id |> labelIDsSplitter |> Tuple.second
                        , labels = model.labels |> List.map .id |> labelIDsSplitter |> Tuple.second
                        }
                    }
                    ReceivedChangesResp
                    |> Cmd.map LoggedInView
                , cmds
                ]
            )

        Just _ ->
            ( { model | offlineQueue = currentOperations }, cmds )


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


qCreateNewNote : OQCreateNote -> OfflineQueueOps -> OfflineQueueOps
qCreateNewNote data queue =
    { queue | createNotes = data :: queue.createNotes }


qEditNote : OQEditNote -> OfflineQueueOps -> OfflineQueueOps
qEditNote data queue =
    let
        ( toCreateNote, restCreateNotes ) =
            queue.createNotes
                |> partitionFirst (\l -> sameId (OfflineID l.offlineId) data.id)
    in
    case toCreateNote of
        Just createData ->
            -- hasn't even been created so just combine edit as create
            { queue
                | createNotes =
                    { offlineId = createData.offlineId
                    , title = data.title |> or createData.title
                    , content = Maybe.withDefault createData.content data.content
                    , pinned = Maybe.withDefault createData.pinned data.pinned
                    , labels = Maybe.withDefault createData.labels data.labels
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
        }


qToggleNotePin : SyncableID -> Maybe Bool -> OfflineQueueOps -> OfflineQueueOps
qToggleNotePin id newPinnedVal =
    qEditNote
        { id = id
        , title = Nothing
        , content = Nothing
        , pinned = newPinnedVal
        , labels = Nothing
        }


qDeleteNote : OQDeleteNote -> OfflineQueueOps -> OfflineQueueOps
qDeleteNote noteId queue =
    let
        ( toCreateNoteInQueue, restCreateNotes ) =
            queue.createNotes
                |> partitionFirst (\l -> sameId (OfflineID l.offlineId) noteId)
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


labelIDsSplitterHelper : List SyncableID -> List String -> List Int -> ( List String, List Int )
labelIDsSplitterHelper ids offlineIds dbIds =
    case ids of
        [] ->
            ( offlineIds, dbIds )

        x :: xs ->
            case x of
                OfflineID offlineId ->
                    labelIDsSplitterHelper xs (offlineId :: offlineIds) dbIds

                DatabaseID dbID ->
                    labelIDsSplitterHelper xs offlineIds (dbID :: dbIds)


labelIDsSplitter : List SyncableID -> ( List String, List Int )
labelIDsSplitter ids =
    labelIDsSplitterHelper ids [] []



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "full-container"
        , css
            [ backgroundColor (rgb 24 129 106)
            , fullWidth
            , height (pct 100)
            ]
        ]
        [ case model.user of
            LoggedOut m ->
                Html.Styled.map LoggedOutView (logInView m)

            CheckingSessionValidity ->
                -- TODO:
                div [] [ text "TODO" ]

            LoggedIn ->
                Html.Styled.map LoggedInView (mainView model)
        ]


logInView : LoggedOutModel -> Html LoggedOutMsg
logInView { username, password } =
    -- TODO: no styling, add styling
    div [ css [ width (pct 100), height (pct 100), displayFlex ] ]
        [ form [ css [ margin auto, displayFlex, flexDirection column, publicSans ], onSubmit Login ]
            [ label [] [ text "Email: ", input [ placeholder "johnDoe@gmail.com", onInput UsernameChange, value username ] [] ]
            , label [] [ text "Password: ", input [ placeholder "password1234", onInput PasswordChange, value password ] [] ]
            , button [ type_ "submit" ] [ text "submit" ]
            ]
        ]


closedLabelsMenuBtn : Html LoggedInMsg
closedLabelsMenuBtn =
    button
        [ css
            [ padY (px 8)
            , paddingLeft (px 8)
            , maxWidth (px 277)
            , minWidth (px 277)
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
        , onClick (OpenLabelsMenu True)
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
            ]
        , div [ css [ width (px 50), height (px 32), displayFlex, justifyContent center, alignItems center ] ]
            [ Filled.arrow_drop_down 32
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        ]


mainView : Model -> Html LoggedInMsg
mainView model =
    div [ css [ height (pct 100), overflow auto ] ]
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
            [ closedLabelsMenuBtn
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

        -- TODO: placeholder
        , div [ css [ padding (px 15), color (hex "fff"), publicSans ] ]
            [ text "Labels (PLACEHOLDER):"
            , div []
                (List.map
                    (\l ->
                        div []
                            [ text l.name
                            , button
                                [ css
                                    [ border (px 0)
                                    , backgroundColor (hex "ff0000")
                                    , cursor pointer
                                    , color (hex "fff")
                                    ]
                                , onClick (DeleteLabel l.id)
                                ]
                                [ Filled.close 24 Inherit |> Svg.Styled.fromUnstyled ]
                            ]
                    )
                    model.labels
                )
            , form [ onSubmit RequestTimeForNewLabelCreation ]
                [ input [ placeholder "School", value model.newLabelName, onInput ChangeNewLabelName ] []
                , button [ type_ "submit" ] [ text "Create label" ]
                ]
            ]
        , div []
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

        -- TODO: add no notes empty state design
        , div
            [ -- TODO: give the tiled effect of google keep
              -- using translate and transitions
              css
                [ displayFlex
                , flexDirection row
                , flexWrap wrap
                , marginTop (px 30)
                ]
            ]
            (List.map (note model) (model.notes |> prioritizePinned))
        ]


note : Model -> Note -> Html LoggedInMsg
note model data =
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
        , class "note"
        ]
        [ div
            [ css
                [ backgroundColor (rgb 117 93 39)
                , color (hex "fff")
                , height (px 36)
                , justifyContent spaceBetween
                , borderBottom3 (px 3) solid (rgb 0 0 0)
                ]
            , class "note-top-actions"
            ]
            [ button
                [ css
                    [ border (px 0)
                    , borderRight3 (px 3) solid (rgb 11 14 17)
                    , backgroundColor
                        (if data.pinned == True then
                            hex "000"

                         else
                            rgb 117 93 39
                        )
                    , color (hex "fff")
                    , hover [ backgroundColor (hex "000"), cursor pointer ]
                    , paddingRight (px 4)
                    , paddingLeft (px 4)
                    , paddingTop (px 3)
                    ]
                , onClick (ChangeNotePinned ( data.id, not data.pinned ))
                ]
                [ Filled.push_pin 28 Inherit |> Svg.Styled.fromUnstyled ]
            , button
                [ css
                    [ border (px 0)
                    , borderLeft3 (px 3) solid (rgb 11 14 17)
                    , hover [ backgroundColor (hex "ff0000"), cursor pointer ]
                    , backgroundColor inherit
                    , color (hex "fff")
                    ]
                , onClick (DeleteNote data.id)
                ]
                [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
            ]
        , div []
            [ case data.title of
                Nothing ->
                    div [] []

                Just title ->
                    div
                        [ css
                            [ publicSans
                            , borderBottom3 (px 1) solid (rgb 0 0 0)
                            , padding (px 10)
                            ]
                        ]
                        [ text title ]
            , p [ css [ publicSans, padding (px 10) ] ]
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
            , case data.labels of
                [] ->
                    div [] []

                labels ->
                    div
                        [ css
                            [ borderTop3 (px 1) solid (rgb 0 0 0)
                            , padding (px 10)
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
            ]
        ]


publicSans : Style
publicSans =
    fontFamilies [ "Public Sans", .value sansSerif ]


fullWidth : Style
fullWidth =
    width (pct 100)


prioritizePinned : List Note -> List Note
prioritizePinned notes =
    let
        pinned =
            List.filter (\n -> n.pinned == True) notes

        unpinned =
            List.filter (\n -> n.pinned == False) notes
    in
    pinned ++ unpinned



-- UID Lib
-- https://github.com/elm/random/issues/2


generateUID : List Random.Seed -> ( String, List Random.Seed )
generateUID seeds =
    List.map (Random.step (alphaNumericGenerator 5)) seeds
        |> List.unzip
        |> Tuple.mapFirst (String.join "")


alphaNumericGenerator : Int -> Random.Generator String
alphaNumericGenerator strLength =
    Random.Extra.choices (Random.Char.char 48 57)
        [ Random.Char.char 97 122
        , Random.Char.char 65 90
        ]
        |> Random.String.string strLength



-- Helpers
-- It's just `filterNot` https://package.elm-lang.org/packages/elm-community/list-extra/8.7.0/List-Extra#filterNot


exclude : (a -> Bool) -> List a -> List a
exclude pred list =
    List.filter (not << pred) list


partitionFirstHelper : List a -> (a -> Bool) -> ( Maybe a, List a ) -> ( Maybe a, List a )
partitionFirstHelper arr pred ( first, checked ) =
    case arr of
        [] ->
            ( Nothing, checked )

        x :: xs ->
            if pred x then
                ( Just x, checked ++ xs )

            else
                partitionFirstHelper xs pred ( Nothing, checked ++ [ x ] )



{-
   Like List.partition but only returns the first element that matches the predicate
   and the rest of the elements with that first element missing

   or Nothing and the original array if it's not there
-}


partitionFirst : (a -> Bool) -> List a -> ( Maybe a, List a )
partitionFirst pred arr =
    partitionFirstHelper arr pred ( Nothing, [] )



{-
   Returns the first value that is present, like the boolean ||.
-}


or : Maybe a -> Maybe a -> Maybe a
or default new =
    case new of
        Just v ->
            Just v

        Nothing ->
            default


listFirst : (a -> Bool) -> List a -> Maybe a
listFirst pred list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just x

            else
                listFirst pred xs



-- CSS Helpers


displayGrid : Style
displayGrid =
    property "display" "grid"


gridTemplateColumns : String -> Style
gridTemplateColumns =
    property "grid-template-columns"


gap : Int -> Style
gap i =
    property "gap" <|
        String.fromInt i
            ++ "px"


gap2 : Int -> Int -> Style
gap2 i j =
    property "gap" <|
        String.fromInt i
            ++ "px "
            ++ String.fromInt j
            ++ "px"


mx : Css.LengthOrAuto a -> Style
mx l =
    Css.batch [ Css.marginLeft l, Css.marginRight l ]


my : Css.LengthOrAuto a -> Style
my l =
    Css.batch [ Css.marginTop l, Css.marginBottom l ]


padY : Css.Length compatible units -> Style
padY l =
    Css.batch [ Css.paddingTop l, Css.paddingBottom l ]


padX : Css.Length compatible units -> Style
padX l =
    Css.batch [ Css.paddingLeft l, Css.paddingRight l ]


white : Color
white =
    rgb 255 255 255


black : Color
black =
    rgb 0 0 0


textColor : ColorValue compatible -> Style
textColor =
    color
