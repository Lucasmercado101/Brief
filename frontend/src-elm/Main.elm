module Main exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (..)
import CssHelpers exposing (publicSans)
import Dog exposing (dogSvg)
import Either exposing (Either(..))
import Helpers exposing (exclude, idDiff, labelIDsSplitter, listFirst, or, partitionFirst, sameId)
import Html
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Page.EditLabels as EditLabels
import Page.LogIn as LogIn
import Ports exposing (..)
import Random
import Random.Char
import Random.Extra
import Random.String
import Route
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)
import Url exposing (Url)



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


type alias LabelsColumnMenu =
    Maybe String


type EditLabelKind
    = Selected SyncableID
    | ConfirmDelete SyncableID
    | Editing SyncableID String


type Model
    = LogIn (List Random.Seed) LogIn.Model
      -- TODO: check if session is valid on entering website,
      -- then go to either logIn or Home
    | Home HomeModel


type alias HomeModel =
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
    , editLabelsScreen :
        Maybe
            { selected : List EditLabelKind
            , searchQuery : String
            , confirmDeleteAllSelectedLabels : Bool
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



-- MESSAGE


type Msg
    = GotLogInMsg LogIn.Msg
    | LoggedInView LoggedInMsg
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)
    | ClickedLink UrlRequest
    | ChangedUrl Url


type LoggedInMsg
    = ChangeNotePinned ( SyncableID, Bool )
    | EditLabelsView EditLabelsViewMsg
    | RequestTimeForNewLabelCreation
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
    | ReceivedRandomValues (List Int)
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


type EditLabelsViewMsg
    = ExitEditingLabelsView
    | ChangeEditLabelsSearchQuery String
    | CreateNewLabelEditLabelsView
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


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        seeds =
            List.map Random.initialSeed flags.seeds
    in
    if flags.hasSessionCookie then
        ( Home
            { key = navKey
            , seeds = seeds
            , notes = []
            , isWritingANewNote = Nothing
            , newLabelName = ""
            , labels = []
            , labelsMenu = Nothing
            , editLabelsScreen = Nothing
            , filters =
                { label = Nothing
                , content = Nothing
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

    else
        ( LogIn seeds (LogIn.init navKey), Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \e ->
                let
                    -- TODO: make this nicer
                    bca : Html.Html Msg
                    bca =
                        (view >> Html.Styled.toUnstyled) e

                    abc : Document Msg
                    abc =
                        { title = "test", body = [ bca ] }
                in
                abc
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update topMsg topModel =
    case ( topMsg, topModel ) of
        -- TODO: fix this
        ( ClickedLink _, _ ) ->
            -- TODO:
            topModel |> pure

        ( ChangedUrl newUrl, _ ) ->
            -- TODO:
            (-- TODO: double check or change Route model
             case Route.fromUrl newUrl of
                Route.Home ->
                    case topModel of
                        Home _ ->
                            -- TODO: better
                            topModel |> pure

                        LogIn seeds { key } ->
                            ( Home
                                { -- TODO: better way
                                  key = key
                                , seeds = seeds
                                , notes = []
                                , isWritingANewNote = Nothing
                                , newLabelName = ""
                                , labels = []
                                , labelsMenu = Nothing
                                , editLabelsScreen = Nothing
                                , filters =
                                    { label = Nothing
                                    , content = Nothing
                                    }

                                -- sync stuff
                                , offlineQueue = emptyOfflineQueue
                                , runningQueueOn = Nothing

                                -- TODO:
                                , lastSyncedAt = Time.millisToPosix 1
                                }
                            , Cmd.batch [ Api.fullSync FullSyncResp, requestRandomValues () ]
                            )

                Route.LogIn ->
                    ( topModel, Cmd.none )
            )

        ( GotLogInMsg loginMsg, LogIn randSeeds logInModel ) ->
            LogIn.update loginMsg logInModel
                |> (\( m, c ) -> ( LogIn randSeeds m, Cmd.map GotLogInMsg c ))

        ( LoggedInView msg, Home homeModel ) ->
            (case topMsg of
                -- TODO: TEMP
                GotLogInMsg _ ->
                    homeModel |> pure

                -- TODO: fix this
                ClickedLink _ ->
                    -- TODO:
                    homeModel |> pure

                ChangedUrl _ ->
                    -- TODO:
                    homeModel |> pure

                LoggedInView loggedInMsg ->
                    case loggedInMsg of
                        EditLabelsView editLabelsViewMsg ->
                            case homeModel.editLabelsScreen of
                                Nothing ->
                                    homeModel |> pure

                                Just editLabelsScreenData ->
                                    case editLabelsViewMsg of
                                        ExitEditingLabelsView ->
                                            { homeModel | editLabelsScreen = Nothing }
                                                |> pure

                                        ChangeEditLabelsSearchQuery newQuery ->
                                            { homeModel | editLabelsScreen = Just { editLabelsScreenData | searchQuery = newQuery } }
                                                |> pure

                                        CreateNewLabelEditLabelsView ->
                                            if String.length editLabelsScreenData.searchQuery == 0 then
                                                homeModel |> pure

                                            else if List.any (\l -> l.name == editLabelsScreenData.searchQuery) homeModel.labels then
                                                -- TODO: make this visual to the user in the form of an error
                                                homeModel |> pure

                                            else
                                                let
                                                    newLabelOfflineId : String
                                                    newLabelOfflineId =
                                                        generateUID homeModel.seeds |> Tuple.first

                                                    newLabel =
                                                        { id = newLabelOfflineId
                                                        , name = editLabelsScreenData.searchQuery
                                                        }
                                                in
                                                ( { homeModel | editLabelsScreen = Just { editLabelsScreenData | searchQuery = "" } }, Cmd.batch [ requestRandomValues (), getNewTimeAndCreateLabel newLabel ] )

                                        SelectLabel id ->
                                            { homeModel
                                                | editLabelsScreen =
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
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                if List.any sameIdOnSelected editLabelsScreenData.selected then
                                                                    editLabelsScreenData.selected |> exclude sameIdOnSelected

                                                                else
                                                                    Selected id :: editLabelsScreenData.selected
                                                        }
                                            }
                                                |> pure

                                        ClearEditLabelsSelections ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just { editLabelsScreenData | selected = [] }
                                            }
                                                |> pure

                                        RequestDeleteLabel id ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure

                                        ConfirmDeleteLabel id ->
                                            { homeModel
                                                | labels = homeModel.labels |> exclude (.id >> sameId id)
                                                , editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure
                                                |> addToQueue (qDeleteLabel id)

                                        CancelDeleteLabel id ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure

                                        EditLabel ( id, newName ) ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure

                                        ChangeEditingLabelName ( id, newName ) ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure

                                        ConfirmEditingLabelName ( id, newName ) ->
                                            if newName == "" then
                                                homeModel |> pure

                                            else if List.any (\l -> (l.name |> String.toLower) == (newName |> String.toLower)) homeModel.labels then
                                                -- TODO: show message to the user "This label already exists"
                                                -- and disable create button
                                                homeModel |> pure

                                            else
                                                { homeModel
                                                    | labels =
                                                        homeModel.labels
                                                            |> List.map
                                                                (\l ->
                                                                    if sameId l.id id then
                                                                        { l | name = newName }

                                                                    else
                                                                        l
                                                                )
                                                    , editLabelsScreen =
                                                        Just
                                                            { editLabelsScreenData
                                                                | selected =
                                                                    editLabelsScreenData.selected
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
                                                }
                                                    |> pure
                                                    |> addToQueue (qEditLabelName { name = newName, id = id })

                                        CancelEditingLabelName id ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure

                                        RemoveLabelFromSelected id ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | selected =
                                                                editLabelsScreenData.selected
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
                                            }
                                                |> pure

                                        RequestConfirmDeleteMultipleLabels ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | confirmDeleteAllSelectedLabels = True
                                                        }
                                            }
                                                |> pure

                                        CancelDeleteMultipleLabels ->
                                            { homeModel
                                                | editLabelsScreen =
                                                    Just
                                                        { editLabelsScreenData
                                                            | confirmDeleteAllSelectedLabels = False
                                                        }
                                            }
                                                |> pure

                                        ConfirmDeleteMultipleLabels ->
                                            let
                                                deletedIds : List SyncableID
                                                deletedIds =
                                                    editLabelsScreenData.selected
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
                                            { homeModel
                                                | labels =
                                                    homeModel.labels
                                                        |> exclude (\l -> List.any (sameId l.id) deletedIds)
                                                , editLabelsScreen =
                                                    Just { editLabelsScreenData | selected = [], confirmDeleteAllSelectedLabels = False }
                                            }
                                                |> pure
                                                |> addToQueue (qDeleteLabels deletedIds)

                        -- Labels column menu
                        OpenLabelsMenu ->
                            { homeModel | labelsMenu = Just "" }
                                |> pure

                        CloseLabelsMenu ->
                            { homeModel | labelsMenu = Nothing }
                                |> pure

                        ChangeLabelsSearchQuery s ->
                            { homeModel | labelsMenu = Maybe.map (\_ -> s) homeModel.labelsMenu }
                                |> pure

                        SelectLabelToFilterBy id ->
                            let
                                newFilter : Maybe SyncableID
                                newFilter =
                                    case homeModel.filters.label of
                                        Nothing ->
                                            Just id

                                        Just oldId ->
                                            if sameId oldId id then
                                                Nothing

                                            else
                                                Just id
                            in
                            { homeModel
                                | filters =
                                    { label = newFilter
                                    , content = homeModel.filters.content
                                    }
                            }
                                |> pure

                        GoToEditLabelsScreen ->
                            { homeModel
                                | editLabelsScreen =
                                    Just
                                        { selected = []
                                        , searchQuery = ""
                                        , confirmDeleteAllSelectedLabels = False
                                        }
                                , labelsMenu = Nothing
                            }
                                |> pure

                        ChangeNotePinned ( uid, newPinnedVal ) ->
                            { homeModel
                                | notes =
                                    List.map
                                        (\n ->
                                            if n.id == uid then
                                                { n | pinned = newPinnedVal }

                                            else
                                                n
                                        )
                                        homeModel.notes
                            }
                                |> pure
                                |> addToQueue (qToggleNotePin uid (Just newPinnedVal))

                        RemoveLabelFromNote { noteID, labelID } ->
                            let
                                ( noteExists, restNotes ) =
                                    partitionFirst (\n -> sameId n.id noteID) homeModel.notes
                            in
                            case noteExists of
                                Nothing ->
                                    homeModel |> pure

                                Just noteData ->
                                    let
                                        newNotes =
                                            noteData.labels |> exclude (sameId labelID)
                                    in
                                    { homeModel | notes = { noteData | labels = newNotes } :: restNotes }
                                        |> pure
                                        |> addToQueue (qEditNoteLabels noteID (Just newNotes))

                        DeleteNote toDeleteNoteID ->
                            { homeModel | notes = homeModel.notes |> exclude (.id >> sameId toDeleteNoteID) }
                                |> pure
                                |> addToQueue (qDeleteNote toDeleteNoteID)

                        ChangeNewLabelName newName ->
                            { homeModel | newLabelName = newName }
                                |> pure

                        RequestTimeForNewLabelCreation ->
                            if String.length homeModel.newLabelName == 0 then
                                homeModel |> pure

                            else if List.any (\l -> l.name == homeModel.newLabelName) homeModel.labels then
                                -- TODO: make this visual to the user in the form of an error
                                homeModel |> pure

                            else
                                let
                                    newLabelOfflineId : String
                                    newLabelOfflineId =
                                        generateUID homeModel.seeds |> Tuple.first

                                    newLabel =
                                        { id = newLabelOfflineId
                                        , name = homeModel.newLabelName
                                        }
                                in
                                ( homeModel, Cmd.batch [ requestRandomValues (), getNewTimeAndCreateLabel newLabel ] )

                        CreateNewLabel data time ->
                            { homeModel
                                | newLabelName = ""
                                , labels =
                                    { id = OfflineID data.id
                                    , name = data.name
                                    , updatedAt = time
                                    , createdAt = time
                                    }
                                        :: homeModel.labels
                            }
                                |> pure
                                |> addToQueue (qNewLabel { offlineId = data.id, name = data.name })

                        ReceivedRandomValues values ->
                            { homeModel | seeds = List.map Random.initialSeed values }
                                |> pure

                        BeginWritingNewNote ->
                            { homeModel
                                | isWritingANewNote =
                                    Just
                                        { title = ""
                                        , content = ""
                                        , labels = Nothing
                                        }
                            }
                                |> pure

                        NewTitleChange s ->
                            { homeModel
                                | isWritingANewNote =
                                    Maybe.map
                                        (\data ->
                                            { data
                                                | title = s
                                            }
                                        )
                                        homeModel.isWritingANewNote
                            }
                                |> pure

                        NewNoteContentChange s ->
                            { homeModel
                                | isWritingANewNote =
                                    Maybe.map
                                        (\data ->
                                            { data | content = s }
                                        )
                                        homeModel.isWritingANewNote
                            }
                                |> pure

                        SearchLabelsQueryChange s ->
                            { homeModel
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
                                        homeModel.isWritingANewNote
                            }
                                |> pure

                        AddLabelToNewNote newLabel ->
                            { homeModel
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
                                        homeModel.isWritingANewNote
                            }
                                |> pure

                        RemoveLabelFromNewNote labelID ->
                            { homeModel
                                | isWritingANewNote =
                                    homeModel.isWritingANewNote
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
                            case homeModel.isWritingANewNote of
                                Nothing ->
                                    homeModel |> pure

                                Just newNoteData ->
                                    if String.length newNoteData.content == 0 then
                                        homeModel |> pure

                                    else
                                        let
                                            newNoteOfflineId =
                                                generateUID homeModel.seeds |> Tuple.first

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
                                        ( homeModel
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
                            { homeModel
                                | isWritingANewNote = Nothing
                                , notes = newNote :: homeModel.notes
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
                            case homeModel.isWritingANewNote of
                                Just data ->
                                    { homeModel
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
                                    homeModel
                                        |> pure

                        ReceivedChangesResp resp ->
                            case resp of
                                Ok { deleted, failedToCreate, failedToEdit, justSyncedAt, downSyncedData, justCreatedData } ->
                                    ( { homeModel
                                        | notes =
                                            let
                                                ( _, notOutdatedNotes ) =
                                                    List.partition
                                                        (\e -> List.any (\l -> sameId (DatabaseID l.id) e.id) downSyncedData.notes)
                                                        homeModel.notes

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
                                                        homeModel.labels

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
                                            if offlineQueueIsEmpty homeModel.offlineQueue then
                                                Nothing

                                            else
                                                Just homeModel.offlineQueue
                                        , lastSyncedAt = justSyncedAt
                                      }
                                    , Cmd.batch
                                        [ updateLastSyncedAt (Time.posixToMillis justSyncedAt)
                                        , if offlineQueueIsEmpty homeModel.offlineQueue then
                                            Cmd.none

                                          else
                                            Api.sendChanges
                                                { operations = queueToOperations homeModel.offlineQueue
                                                , lastSyncedAt = justSyncedAt
                                                , currentData =
                                                    { notes = homeModel.notes |> List.map .id |> labelIDsSplitter |> Tuple.second
                                                    , labels = homeModel.labels |> List.map .id |> labelIDsSplitter |> Tuple.second
                                                    }
                                                }
                                                ReceivedChangesResp
                                                |> Cmd.map LoggedInView
                                        ]
                                    )

                                -- TODO: error handling here
                                Err _ ->
                                    homeModel |> pure

                FullSyncResp res ->
                    case res of
                        -- TODO:
                        Ok ( notes, labels ) ->
                            { homeModel
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
                            homeModel |> pure
            )
                |> (\( m, c ) -> ( Home m, c ))

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( topModel, Cmd.none )


updateWith toModel toMsg ( m, c ) =
    ( toModel m, Cmd.map toMsg c )



-- QUEUE


addToQueue : (OfflineQueueOps -> OfflineQueueOps) -> ( HomeModel, Cmd Msg ) -> ( HomeModel, Cmd Msg )
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



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "full-container"
        , css
            [ fullWidth
            , height (pct 100)
            , backgroundColor (rgb 18 104 85)
            , backgroundImage (url "./media/bgr.png ")
            , backgroundSize contain
            , backgroundRepeat repeat
            ]
        ]
        [ case model of
            LogIn seeds logInModel ->
                Html.Styled.map GotLogInMsg (LogIn.logInView { username = logInModel.username, password = logInModel.password, key = logInModel.key })

            Home homeModel ->
                Html.Styled.map LoggedInView (mainView homeModel)
        ]



-- }


labelsMenuWidth : Float
labelsMenuWidth =
    277


closedLabelsMenuBtn : String -> Html LoggedInMsg
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


openLabelsMenuBtn : String -> Html LoggedInMsg
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


labelsMenuColumn : HomeModel -> Html LoggedInMsg
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


editLabelsView :
    HomeModel
    ->
        { selected : List EditLabelKind
        , searchQuery : String
        , confirmDeleteAllSelectedLabels : Bool
        }
    -> Html EditLabelsViewMsg
editLabelsView model { selected, searchQuery, confirmDeleteAllSelectedLabels } =
    let
        header =
            div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , alignItems flexStart
                    , marginBottom (px 18)
                    ]
                ]
                [ div
                    [ css
                        [ paddingLeft (px 25)
                        , displayFlex
                        , flexDirection column
                        , paddingTop (px 12)
                        ]
                    ]
                    [ p [ css [ delaGothicOne, fontSize (px 45) ] ] [ text "Labels" ]
                    , p [ css [ publicSans, fontSize (px 22) ] ] [ text ((List.length model.labels |> String.fromInt) ++ " Total") ]
                    ]
                , button
                    [ css
                        [ padding (px 12)
                        , backgroundColor transparent
                        , border (px 0)
                        , cursor pointer
                        ]
                    , onClick ExitEditingLabelsView
                    ]
                    [ Filled.close 32
                        Inherit
                        |> Svg.Styled.fromUnstyled
                    ]
                ]

        searchBar =
            div
                [ css
                    [ displayFlex
                    , backgroundColor white
                    , alignItems center
                    , borderBottom3 (px 3) solid black
                    , borderTop3 (px 3) solid black
                    ]
                ]
                [ label [ for "labels-edit-searchBar", css [ displayFlex, justifyContent center, alignItems center, paddingLeft (px 12), paddingRight (px 6) ] ] [ Outlined.search 18 Inherit |> Svg.Styled.fromUnstyled ]
                , input
                    [ id "labels-edit-searchBar"
                    , css
                        [ border (px 0)
                        , fontWeight (int 600)
                        , width (pct 100)
                        , height (pct 100)
                        , padY (px 7)
                        , publicSans
                        , fontSize (px 16)
                        ]
                    , placeholder "Search labels..."
                    , value searchQuery
                    , onInput ChangeEditLabelsSearchQuery
                    ]
                    []
                ]

        itemsList =
            ul [ css [ overflow auto, height (pct 100) ] ]
                (List.map
                    (\label ->
                        li
                            []
                            [ button
                                [ css
                                    ([ paddingLeft (px 12)
                                     , publicSans
                                     , padY (px 5)
                                     , width (pct 100)
                                     , textAlign start
                                     , backgroundColor transparent
                                     , border (px 0)
                                     , cursor pointer
                                     , fontSize (px 16)
                                     , fontWeight (int 600)
                                     ]
                                        ++ (if
                                                List.any
                                                    (\e ->
                                                        case e of
                                                            Selected i ->
                                                                sameId label.id i

                                                            ConfirmDelete i ->
                                                                sameId label.id i

                                                            Editing i _ ->
                                                                sameId label.id i
                                                    )
                                                    selected
                                            then
                                                [ textColor white, backgroundColor black ]

                                            else
                                                [ hover [ textColor white, backgroundColor black ] ]
                                           )
                                    )
                                , onClick (SelectLabel label.id)
                                ]
                                [ p
                                    [ css
                                        [ whiteSpace noWrap
                                        , textOverflow ellipsis
                                        , overflow hidden
                                        ]
                                    ]
                                    [ text label.name ]
                                ]
                            ]
                    )
                    (model.labels
                        |> List.filter (.name >> String.toLower >> String.contains (String.toLower searchQuery))
                    )
                )

        newLabelBtn =
            button
                [ css [ cursor pointer, fontWeight (int 800), publicSans, fontSize (px 16), backgroundColor white, padY (px 12), border (px 0), borderTop3 (px 3) solid black ]
                , onClick CreateNewLabelEditLabelsView
                ]
                [ text "CREATE NEW LABEL" ]

        labelCard { name, id } isFirst =
            div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 480)
                    , marginTop
                        (px
                            (if isFirst then
                                0

                             else
                                32
                            )
                        )
                    ]
                ]
                [ div [ css [ backgroundColor white, displayFlex, justifyContent spaceBetween, borderBottom3 (px 5) solid black ] ]
                    [ button
                        [ css
                            [ displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor error
                            , textColor white
                            , border (px 0)
                            , borderRight3 (px 5) solid black
                            , cursor pointer
                            , width (px 42)
                            ]
                        , onClick (RequestDeleteLabel id)
                        ]
                        [ Filled.delete 36 Inherit |> Svg.Styled.fromUnstyled ]
                    , div [ css [ displayFlex ] ]
                        [ button
                            [ css
                                [ displayFlex
                                , justifyContent center
                                , alignItems center
                                , textColor black
                                , border (px 0)
                                , backgroundColor transparent
                                , borderLeft3 (px 5) solid black
                                , cursor pointer
                                , hover [ backgroundColor black, textColor white ]
                                ]
                            , onClick (EditLabel ( id, name ))
                            ]
                            [ Filled.edit 42 Inherit |> Svg.Styled.fromUnstyled ]
                        , button
                            [ css
                                [ displayFlex
                                , justifyContent center
                                , alignItems center
                                , textColor black
                                , border (px 0)
                                , backgroundColor transparent
                                , borderLeft3 (px 5) solid black
                                , cursor pointer
                                , hover [ backgroundColor black, textColor white ]
                                ]
                            , onClick (RemoveLabelFromSelected id)
                            ]
                            [ Filled.close 42 Inherit |> Svg.Styled.fromUnstyled ]
                        ]
                    ]
                , div [ css [ padX (px 32), paddingTop (px 16), paddingBottom (px 32) ] ]
                    [ p [ css [ delaGothicOne, fontSize (px 38) ] ] [ text name ]
                    , p [ css [ publicSans, fontSize (px 18), marginBottom (px 26) ] ] [ text "Created on: July 30th at 12:05 PM" ]
                    , p [ css [ publicSans, fontSize (px 22) ] ] [ text "Last Updated on: July 30th at 12:05 PM" ]
                    , div [ css [ marginTop (px 12) ] ]
                        [ p [ css [ publicSans, textDecoration underline, fontSize (px 22), display inline ] ] [ text "50 Notes" ]
                        , p [ css [ publicSans, fontSize (px 22), display inline ] ] [ text " using this label" ]
                        ]
                    ]
                ]

        confirmDeleteCard { name, id } isFirst =
            div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 480)
                    , minWidth (px 480)
                    , marginTop
                        (px
                            (if isFirst then
                                0

                             else
                                32
                            )
                        )
                    ]
                ]
                [ p [ css [ delaGothicOne, fontSize (px 38), marginTop (px 12), marginBottom (px 16), textAlign center ] ] [ text "Really?" ]
                , div [ css [ display inlineBlock, textAlign center ] ]
                    [ p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "Are you sure you want to " ]
                    , p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "delete" ]
                    , p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text " label \"" ]
                    , strong [ css [ fontWeight (int 900), publicSans, fontSize (px 18), display inline ] ] [ text name ]
                    , p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "\"?" ]
                    ]
                , div [ css [ displayFlex, marginTop (px 16), backgroundColor white ] ]
                    [ button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), borderTop3 (px 5) solid black, width (pct 100), padY (px 10), textAlign center ]
                        , onClick (CancelDeleteLabel id)
                        ]
                        [ text "Cancel" ]
                    , button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), width (pct 100), borderLeft3 (px 5) solid black, borderTop3 (px 5) solid black, padY (px 10), textAlign center ]
                        , onClick (ConfirmDeleteLabel id)
                        ]
                        [ text "Confirm" ]
                    ]
                ]

        editLabelCard { name, id } isFirst newName =
            div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 480)
                    , minWidth (px 480)
                    , marginTop
                        (px
                            (if isFirst then
                                0

                             else
                                32
                            )
                        )
                    ]
                ]
                [ p [ css [ delaGothicOne, fontSize (px 38), marginTop (px 12), marginBottom (px 16), textAlign center ] ] [ text name ]
                , label [ css [ publicSans, fontSize (px 18), padX (px 16), displayFlex, alignItems center ] ]
                    [ text "Name:"
                    , input
                        [ css [ padY (px 12), marginLeft (px 12), border3 (px 3) solid black, publicSans, fontSize (px 18), width (pct 100) ]
                        , class "label-edit-input"
                        , placeholder name
                        , value newName
                        , onInput (\e -> ChangeEditingLabelName ( id, e ))
                        ]
                        []
                    ]
                , div [ css [ displayFlex, marginTop (px 16), backgroundColor white ] ]
                    [ button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), borderTop3 (px 5) solid black, width (pct 100), padY (px 10), textAlign center ]
                        , onClick (CancelEditingLabelName id)
                        ]
                        [ text "Cancel" ]
                    , button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), width (pct 100), borderLeft3 (px 5) solid black, borderTop3 (px 5) solid black, padY (px 10), textAlign center ]
                        , onClick (ConfirmEditingLabelName ( id, newName ))
                        ]
                        [ text "Confirm" ]
                    ]
                ]

        someLabelSelected =
            List.length selected /= 0

        selectedActions amount =
            let
                selectedAmountActions =
                    [ button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderRight3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor black, textColor white ]
                            , cursor pointer
                            ]
                        , title "Delete selected labels"
                        , onClick RequestConfirmDeleteMultipleLabels
                        ]
                        [ Filled.delete 32 Inherit |> Svg.Styled.fromUnstyled ]
                    , div [ css [ width (pct 100), padY (px 16) ] ]
                        [ p [ css [ publicSans, fontSize (px 42), fontWeight bold ] ] [ text amount ]
                        , p [ css [ publicSans, fontSize (px 42) ] ] [ text "Selected" ]
                        ]
                    , button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderLeft3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor error, textColor white ]
                            , cursor pointer
                            ]
                        , title "Clear selected labels"
                        , onClick ClearEditLabelsSelections
                        ]
                        [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
                    ]

                confirmDeleteMultiple =
                    [ button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderRight3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor black, textColor white ]
                            , cursor pointer
                            ]
                        , onClick CancelDeleteMultipleLabels
                        ]
                        [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
                    , div [ css [ width (pct 100), padY (px 16) ] ]
                        [ p [ css [ publicSans, fontSize (px 42) ] ] [ text "Delete" ]
                        , strong [ css [ publicSans, fontSize (px 42), display inline ] ] [ text amount ]
                        , p [ css [ display inline, publicSans, fontSize (px 42) ] ] [ text " labels?" ]
                        ]
                    , button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderLeft3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor error, textColor white ]
                            , cursor pointer
                            ]
                        , onClick ConfirmDeleteMultipleLabels
                        ]
                        [ Filled.check 32 Inherit |> Svg.Styled.fromUnstyled ]
                    ]
            in
            div [ css [ displayFlex, marginBottom (px 32), textAlign center, textColor white, border3 (px 3) solid white ] ]
                (if confirmDeleteAllSelectedLabels then
                    confirmDeleteMultiple

                 else
                    selectedAmountActions
                )
    in
    div [ css [ displayFlex, flexDirection row, height (pct 100) ] ]
        [ div [ css [ displayFlex, padY (px 45), height (pct 100) ] ]
            [ div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 345)
                    , minWidth (px 345)
                    , width (px 345)
                    , overflow hidden

                    -- TODO: more consistent spacing between them
                    , marginRight (px 200)
                    , marginLeft (px 200)
                    ]
                ]
                [ header
                , searchBar
                , itemsList
                , if (searchQuery /= "") && (List.filter (.name >> String.toLower >> (==) (String.toLower searchQuery)) model.labels |> List.isEmpty) then
                    newLabelBtn

                  else
                    text ""
                ]
            ]
        , div [ css [ overflowY auto, height (pct 100), padY (px 45), padX (px 32) ] ]
            [ if someLabelSelected then
                selectedActions (selected |> List.length |> String.fromInt)

              else
                div [ css [ displayFlex, alignItems center, flexDirection column, justifyContent center, height (pct 80) ] ]
                    [ dogSvg |> Svg.Styled.fromUnstyled
                    , p [ css [ publicSans, fontSize (px 42), fontWeight (int 300), textAlign center, textColor white ] ]
                        [ text "Select a label to edit"
                        , br [] []
                        , text "and it will appear here"
                        ]
                    ]
            , ul []
                (List.indexedMap
                    (\i ( label, labelKind ) ->
                        case labelKind of
                            Selected _ ->
                                labelCard { name = label.name, id = label.id } (i == 0)

                            ConfirmDelete _ ->
                                confirmDeleteCard { name = label.name, id = label.id } (i == 0)

                            Editing _ newName ->
                                editLabelCard { name = label.name, id = label.id } (i == 0) newName
                    )
                    (let
                        labelKinds : List Label -> List EditLabelKind -> List ( Label, Maybe EditLabelKind ) -> List ( Label, Maybe EditLabelKind )
                        labelKinds labels selection current =
                            case labels of
                                [] ->
                                    current

                                x :: xs ->
                                    let
                                        ( foundKind, restKind ) =
                                            selection
                                                |> partitionFirst
                                                    (\r ->
                                                        case r of
                                                            Selected i ->
                                                                sameId x.id i

                                                            ConfirmDelete i ->
                                                                sameId x.id i

                                                            Editing i _ ->
                                                                sameId x.id i
                                                    )
                                    in
                                    labelKinds xs restKind (( x, foundKind ) :: current)
                     in
                     labelKinds model.labels selected []
                        |> List.filterMap
                            (\( id, kind ) ->
                                case kind of
                                    Nothing ->
                                        Nothing

                                    Just k ->
                                        Just ( id, k )
                            )
                    )
                )
            ]
        ]


mainView : HomeModel -> Html LoggedInMsg
mainView model =
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
            , case model.editLabelsScreen of
                Nothing ->
                    mainViewNotesList model

                Just val ->
                    editLabelsView model val |> Html.Styled.map EditLabelsView
            ]
        ]


mainViewNotesList : HomeModel -> Html LoggedInMsg
mainViewNotesList model =
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


note : HomeModel -> Note -> Html LoggedInMsg
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


delaGothicOne : Style
delaGothicOne =
    fontFamilies [ "Dela Gothic One", .value sansSerif ]


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


error : Color
error =
    rgb 255 0 0


secondary : Color
secondary =
    rgb 255 203 127


transparent : Color
transparent =
    rgba 0 0 0 0


userSelectNone : Style
userSelectNone =
    property "user-select" "none"


primary : Color
primary =
    rgb 106 192 255
