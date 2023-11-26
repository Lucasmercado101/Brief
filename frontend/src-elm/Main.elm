module Main exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (alignItems, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, bold, border, border3, borderBottom3, borderLeft3, borderRight, borderRight3, center, color, column, contain, cursor, displayFlex, ellipsis, flex, flex3, flexDirection, flexGrow, flexShrink, fontSize, fontWeight, fullWidth, height, hidden, hover, inherit, int, justifyContent, marginLeft, maxWidth, minWidth, noWrap, overflow, overflowY, padding, paddingLeft, paddingTop, pct, pointer, position, property, px, repeat, rgb, solid, spaceBetween, start, sticky, textAlign, textOverflow, top, transparent, url, whiteSpace, width)
import CssHelpers exposing (black, col, error, mx, padX, padY, primary, publicSans, row, secondary, textColor, userSelectNone, white)
import DataTypes exposing (Label, Note)
import Dog exposing (dogSvg)
import Either exposing (Either(..))
import Helpers exposing (elIsIn, exclude, labelIDsSplitter, listFirst, mapToWithDefault, maybeToBool, sameId)
import Html
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (Action(..), OfflineQueueOps, actionMapToFn, emptyOfflineQueue, offlineQueueIsEmpty, qCreateNewNote, qDeleteNote, qEditNoteLabels, qNewLabel, qToggleNotePin, queueToOperations)
import Page.EditLabels as EditLabels
import Page.EditNote as EditNote
import Page.Home as Home exposing (Signal(..))
import Page.LogIn as LogIn
import Ports exposing (getWindowSize, isNowOffline, isNowOnline, requestRandomValues, updateLastSyncedAt, windowResized)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LoggedOff logInModel ->
            Sub.none

        LoggedIn { page } ->
            Sub.batch
                [ isNowOffline IsOffline
                , isNowOnline IsOnline
                , windowResized WindowResized
                , let
                    map msg =
                        Sub.map (\e -> GotPageMsg (msg e))
                  in
                  case page of
                    Home homeModel ->
                        Home.subscriptions homeModel
                            |> map GotHomeMsg

                    EditLabels editLabelsModel ->
                        EditLabels.subscriptions editLabelsModel
                            |> map GotEditLabelsMsg

                    EditNote editNoteModel ->
                        EditNote.subscriptions editNoteModel
                            |> map GotEditNoteMsg
                ]



-- MODEL


type Page
    = Home Home.Model
    | EditLabels EditLabels.Model
    | EditNote EditNote.Model


type alias LabelsColumnMenu =
    Maybe String


type alias LoggedInModel =
    { page : Page
    , windowRes : { width : Int, height : Int }
    , key : Nav.Key
    , searchBarQuery : String

    -- Labels menu
    , labelsMenu : LabelsColumnMenu
    , filters :
        { label : Maybe SyncableID
        , content : Maybe String
        }

    -- sync stuff
    , lastSyncedAt : Posix

    -- TODO:refactor this into a type
    , isOnline : Bool
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    }


type Model
    = LoggedOff LogIn.Model
      -- TODO: check if session is valid on entering website,
      -- then go to either logIn or Home
    | LoggedIn LoggedInModel



-- MESSAGE


type PageMsg
    = GotLogInMsg LogIn.Msg
    | GotHomeMsg Home.Msg
    | GotEditLabelsMsg EditLabels.Msg
    | GotEditNoteMsg EditNote.Msg


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
    | GotPageMsg PageMsg
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)
    | ReceivedChangesResp (Result Http.Error Api.ChangesResponse)
    | IsOffline
    | IsOnline
    | WindowResized { width : Int, height : Int }
      -- TODO: remove this msg, replace with Nav.pushUrl
    | ReturnHome
    | OnChangedSearchBarQuery String
    | ClearSearchBarQuery
      -- Labels menu
    | OpenLabelsMenu
    | CloseLabelsMenu
    | SelectLabelToFilterBy SyncableID
    | ChangeLabelsSearchQuery String
    | GoToEditLabelsScreen



-- INIT


type alias Flags =
    { seeds : List Int
    , online : Bool
    , hasSessionCookie : Bool
    , lastSyncedAt : Int
    , windowSize : { width : Int, height : Int }
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        seeds =
            List.map Random.initialSeed flags.seeds
    in
    if not flags.hasSessionCookie then
        ( LoggedOff (LogIn.init navKey seeds), Cmd.none )

    else
        ( LoggedIn
            { offlineQueue = emptyOfflineQueue
            , runningQueueOn = Nothing
            , lastSyncedAt = Time.millisToPosix flags.lastSyncedAt
            , isOnline = flags.online
            , windowRes = flags.windowSize
            , key = navKey
            , searchBarQuery = ""

            -- Labels menu
            , labelsMenu = Nothing
            , filters =
                { label = Nothing
                , content = Nothing
                }

            -- page
            , page =
                case Route.fromUrl url of
                    -- TODO: notes and labels should be combined into
                    -- "data" with NotLoaded and Loaded states
                    -- or https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/RemoteData
                    Route.EditLabels ->
                        EditLabels (EditLabels.init { seeds = seeds, labels = [], notes = [], key = navKey })

                    Route.Home ->
                        Home (Home.init { key = navKey, seeds = seeds, labels = [], notes = [] })

                    Route.LogIn ->
                        Home (Home.init { key = navKey, seeds = seeds, labels = [], notes = [] })

                    Route.EditNote noteId ->
                        EditNote (EditNote.init { noteId = noteId, key = navKey, seeds = seeds, labels = [], notes = [], noteData = Nothing })
            }
        , Api.fullSync FullSyncResp
        )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "Brief notes app"
                , body = [ view model |> Html.Styled.toUnstyled ]
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update topMsg topModel =
    let
        loggedInMap : (LoggedInModel -> LoggedInModel) -> Model
        loggedInMap fn =
            case topModel of
                LoggedIn loggedInModel ->
                    LoggedIn (fn loggedInModel)

                LoggedOff _ ->
                    topModel
    in
    case topMsg of
        IsOffline ->
            loggedInMap (\model -> { model | isOnline = False })
                |> pure

        IsOnline ->
            case topModel of
                LoggedIn loggedInModel ->
                    if offlineQueueIsEmpty loggedInModel.offlineQueue then
                        -- TODO: call fullRespSync to see if we're outdated?
                        -- or just leave it to the changes endpoint to retreat all new stuff?
                        LoggedIn { loggedInModel | isOnline = True } |> pure

                    else
                        let
                            ( labelsIds, notesIds ) =
                                (case loggedInModel.page of
                                    Home model ->
                                        ( model.labels, model.notes )

                                    EditNote model ->
                                        ( model.labels, model.notes )

                                    EditLabels model ->
                                        ( model.labels, model.notes )
                                )
                                    |> (\( l, n ) -> ( l |> List.map .id |> labelIDsSplitter |> Tuple.second, n |> List.map .id |> labelIDsSplitter |> Tuple.second ))
                        in
                        ( LoggedIn
                            { loggedInModel
                                | offlineQueue = emptyOfflineQueue
                                , runningQueueOn = Just loggedInModel.offlineQueue
                                , isOnline = True
                            }
                        , Api.sendChanges
                            { operations = queueToOperations loggedInModel.offlineQueue
                            , lastSyncedAt = loggedInModel.lastSyncedAt
                            , currentData =
                                { notes = notesIds
                                , labels = labelsIds
                                }
                            }
                            ReceivedChangesResp
                        )

                LoggedOff _ ->
                    topModel |> pure

        -- TODO: fix this
        ClickedLink _ ->
            -- TODO:
            topModel |> pure

        WindowResized dimensions ->
            case topModel of
                LoggedOff _ ->
                    topModel |> pure

                LoggedIn loggedInModel ->
                    LoggedIn { loggedInModel | windowRes = dimensions } |> pure

        ChangedUrl newUrl ->
            (-- TODO: double check or change Route model
             case topModel of
                LoggedOff logInViewModel ->
                    case Route.fromUrl newUrl of
                        Route.Home ->
                            -- TODO: more rigorous checking if succeeded in logging in
                            ( LoggedIn
                                { page =
                                    Home
                                        (Home.init
                                            { key = logInViewModel.key
                                            , seeds = logInViewModel.seeds
                                            , labels = []
                                            , notes = []
                                            }
                                        )
                                , searchBarQuery = ""
                                , labelsMenu = Nothing
                                , filters =
                                    { label = Nothing
                                    , content = Nothing
                                    }
                                , key = logInViewModel.key
                                , offlineQueue = emptyOfflineQueue
                                , runningQueueOn = Nothing
                                , lastSyncedAt = Time.millisToPosix 1
                                , isOnline = True
                                , windowRes = { width = 0, height = 0 }
                                }
                            , Cmd.batch
                                [ getWindowSize WindowResized
                                , Api.fullSync FullSyncResp
                                , requestRandomValues ()
                                ]
                            )

                        Route.EditLabels ->
                            topModel |> pure

                        Route.LogIn ->
                            topModel |> pure

                        Route.EditNote _ ->
                            topModel |> pure

                LoggedIn loggedInModel ->
                    case loggedInModel.page of
                        Home pageData ->
                            case Route.fromUrl newUrl of
                                Route.EditLabels ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                EditLabels
                                                    (EditLabels.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        }
                                                    )
                                        }
                                        |> pure

                                Route.EditNote noteIdToEdit ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                EditNote
                                                    (EditNote.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        , noteId = noteIdToEdit

                                                        -- TODO: handle empty state when no data has been loaded yet
                                                        , noteData = listFirst (.id >> sameId noteIdToEdit) pageData.notes
                                                        }
                                                    )
                                        }
                                        |> pure

                                Route.Home ->
                                    topModel |> pure

                                Route.LogIn ->
                                    topModel |> pure

                        EditLabels pageData ->
                            case Route.fromUrl newUrl of
                                Route.EditNote noteIdToEdit ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                EditNote
                                                    (EditNote.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        , noteId = noteIdToEdit

                                                        -- TODO: handle empty state when no data has been loaded yet
                                                        , noteData = listFirst (.id >> sameId noteIdToEdit) pageData.notes
                                                        }
                                                    )
                                        }
                                        |> pure

                                Route.Home ->
                                    ( LoggedIn
                                        { loggedInModel
                                            | page =
                                                Home
                                                    (Home.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        }
                                                    )
                                        }
                                    , getWindowSize WindowResized
                                    )

                                Route.EditLabels ->
                                    topModel |> pure

                                Route.LogIn ->
                                    topModel |> pure

                        EditNote pageData ->
                            case Route.fromUrl newUrl of
                                Route.Home ->
                                    ( LoggedIn
                                        { loggedInModel
                                            | page =
                                                Home
                                                    (Home.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        }
                                                    )
                                        }
                                    , getWindowSize WindowResized
                                    )

                                Route.EditLabels ->
                                    ( LoggedIn
                                        { loggedInModel
                                            | page =
                                                Home
                                                    (Home.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        }
                                                    )
                                        }
                                    , getWindowSize WindowResized
                                    )

                                Route.EditNote noteIdToEdit ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                EditNote
                                                    (EditNote.init
                                                        { seeds = pageData.seeds
                                                        , labels = pageData.labels
                                                        , notes = pageData.notes
                                                        , key = pageData.key
                                                        , noteId = noteIdToEdit

                                                        -- TODO: handle empty state when no data has been loaded yet
                                                        , noteData = listFirst (.id >> sameId noteIdToEdit) pageData.notes
                                                        }
                                                    )
                                        }
                                        |> pure

                                Route.LogIn ->
                                    topModel |> pure
            )

        GotPageMsg pageMsg ->
            case topModel of
                LoggedOff logInModel ->
                    case pageMsg of
                        GotLogInMsg loginMsg ->
                            LogIn.update loginMsg logInModel
                                |> (\( m, c ) -> ( LoggedOff m, Cmd.map GotPageMsg <| Cmd.map GotLogInMsg c ))

                        _ ->
                            -- Disregard messages that arrived for the wrong page.
                            topModel |> pure

                LoggedIn loggedInModel ->
                    case ( pageMsg, loggedInModel.page ) of
                        ( GotHomeMsg homeMsg, Home homeModel ) ->
                            Home.update homeMsg homeModel
                                |> updateHomeWithSignal loggedInModel

                        ( GotEditLabelsMsg editLabelsMsg, EditLabels editLabelsModel ) ->
                            EditLabels.update editLabelsMsg editLabelsModel
                                |> updateEditLabelsWithSignal EditLabels GotEditLabelsMsg loggedInModel

                        ( GotEditNoteMsg editNoteMsg, EditNote editNoteModel ) ->
                            EditNote.update editNoteMsg editNoteModel
                                |> updateEditNoteWithSignal EditNote GotEditNoteMsg loggedInModel

                        ( _, _ ) ->
                            -- Disregard messages that arrived for the wrong page.
                            topModel |> pure

        FullSyncResp res ->
            case topModel of
                LoggedOff _ ->
                    topModel |> pure

                LoggedIn loggedInModel ->
                    case res of
                        Ok ( notes, labels ) ->
                            let
                                updatedPageModel m =
                                    { m
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
                                                    , order = l.order
                                                    }
                                                )
                                                notes
                                    }
                            in
                            LoggedIn
                                { loggedInModel
                                    | page =
                                        case loggedInModel.page of
                                            Home homeModel ->
                                                Home (updatedPageModel homeModel)

                                            EditLabels editLabelsModel ->
                                                EditLabels (updatedPageModel editLabelsModel)

                                            EditNote editNoteModel ->
                                                EditNote (updatedPageModel editNoteModel)
                                }
                                |> pure

                        Err v ->
                            -- TODO: handle 403
                            topModel |> pure

        ReceivedChangesResp resp ->
            case topModel of
                LoggedOff _ ->
                    topModel |> pure

                LoggedIn loggedInModel ->
                    case resp of
                        Ok { deleted, failedToCreate, justSyncedAt, downSyncedData, justCreatedData } ->
                            let
                                updatePageModel m =
                                    { m
                                        | notes =
                                            let
                                                ( _, notOutdatedNotes ) =
                                                    m.notes
                                                        |> List.partition (elIsIn downSyncedData.notes (\a b -> sameId a.id (DatabaseID b.id)))

                                                updatedNotes : List Note
                                                updatedNotes =
                                                    downSyncedData.notes
                                                        |> List.map
                                                            (\e ->
                                                                let
                                                                    oldNote : Maybe Note
                                                                    oldNote =
                                                                        listFirst (\l -> sameId (DatabaseID e.id) l.id) m.notes
                                                                in
                                                                { id = DatabaseID e.id
                                                                , title = e.title
                                                                , content = e.content
                                                                , pinned = e.pinned
                                                                , createdAt = e.createdAt
                                                                , updatedAt = e.updatedAt
                                                                , labels = e.labels |> List.map DatabaseID
                                                                , order =
                                                                    -- NOTE: workaround to backend not having unique userID & noteOrder constraint
                                                                    -- essentially order can get mixed up and 2 notes can have the same order
                                                                    -- if i don't do this, as i could change notes order and backend could reply late
                                                                    -- with note data and if order gets replaced with backend new data then multiple notes
                                                                    -- could end up with the same order number.
                                                                    -- this case happens when moving notes around faster than backend replies in between
                                                                    case oldNote of
                                                                        Just v ->
                                                                            v.order

                                                                        Nothing ->
                                                                            e.order
                                                                }
                                                            )
                                            in
                                            notOutdatedNotes
                                                -- remove the ones that were failed to create
                                                |> exclude (elIsIn failedToCreate (\a b -> sameId a.id (OfflineID b)))
                                                -- remove the ones that don't exist in DB
                                                |> exclude (elIsIn deleted.notes (\a b -> sameId a.id (DatabaseID b)))
                                                -- update just created
                                                |> List.map
                                                    (\l ->
                                                        (justCreatedData.notes |> listFirst (\( _, offlineId ) -> sameId l.id (OfflineID offlineId)))
                                                            |> mapToWithDefault l
                                                                (\( v, _ ) ->
                                                                    { id = DatabaseID v.id
                                                                    , title = v.title
                                                                    , content = v.content
                                                                    , pinned = v.pinned
                                                                    , createdAt = v.createdAt
                                                                    , updatedAt = v.updatedAt
                                                                    , labels = v.labels |> List.map DatabaseID
                                                                    , order = v.order
                                                                    }
                                                                )
                                                    )
                                                |> (++) updatedNotes
                                        , labels =
                                            let
                                                ( _, notOutdatedLabels ) =
                                                    m.labels
                                                        |> List.partition (elIsIn downSyncedData.labels (\a b -> sameId a.id (DatabaseID b.id)))

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
                                                |> exclude (elIsIn failedToCreate (\a b -> sameId a.id (OfflineID b)))
                                                -- remove the ones that don't exist in DB
                                                |> exclude (elIsIn deleted.labels (\a b -> sameId a.id (DatabaseID b)))
                                                -- update just created
                                                |> List.map
                                                    (\l ->
                                                        (justCreatedData.labels |> listFirst (\( _, offlineId ) -> sameId l.id (OfflineID offlineId)))
                                                            |> mapToWithDefault l
                                                                (\( v, _ ) ->
                                                                    { id = DatabaseID v.id
                                                                    , name = v.name
                                                                    , createdAt = v.createdAt
                                                                    , updatedAt = v.updatedAt
                                                                    }
                                                                )
                                                    )
                                                |> (++) updatedLabels
                                    }

                                upPageModel1 =
                                    case loggedInModel.page of
                                        Home homeModel ->
                                            Home (updatePageModel homeModel)

                                        EditLabels editLabelsModel ->
                                            EditLabels (updatePageModel editLabelsModel)

                                        EditNote editNoteModel ->
                                            -- TODO: if note that was being edited is deleted, show message
                                            EditNote (updatePageModel editNoteModel)

                                ( labels, notes ) =
                                    case upPageModel1 of
                                        Home homeModel ->
                                            ( homeModel.labels, homeModel.notes )

                                        EditLabels editLabelsModel ->
                                            ( editLabelsModel.labels, editLabelsModel.notes )

                                        EditNote editNoteModel ->
                                            ( editNoteModel.labels, editNoteModel.notes )

                                -- TODO: in this same way also update editLabels
                                -- so that selected offlienIDs are now selected OnlineIDs
                                ( updatedPageModel, cmd1 ) =
                                    case upPageModel1 of
                                        EditNote editNoteModel ->
                                            case editNoteModel.noteId of
                                                OfflineID offlineId ->
                                                    -- NOTE: what i'm doing here is changing the editing from editing an offlineID note
                                                    -- to the one that was just created using a DatabaseID now
                                                    let
                                                        justCreatedTheNote =
                                                            listFirst (\( onlineId, prevOfflineId ) -> prevOfflineId == offlineId) justCreatedData.notes
                                                    in
                                                    case justCreatedTheNote of
                                                        Just ( note, _ ) ->
                                                            ( upPageModel1, Route.replaceUrl editNoteModel.key (Route.EditNote (DatabaseID note.id)) )

                                                        Nothing ->
                                                            ( upPageModel1, Cmd.none )

                                                DatabaseID _ ->
                                                    ( upPageModel1, Cmd.none )

                                        Home _ ->
                                            ( upPageModel1, Cmd.none )

                                        EditLabels _ ->
                                            ( upPageModel1, Cmd.none )

                                -- NOTE: What is being done here is that when we have the response with newly create IDs
                                -- we replace old offlineIDs in the queue with the newly created ones.
                                updatedOfflineQueue =
                                    { -- Create is always going to be created with an offlineID
                                      createLabels = loggedInModel.offlineQueue.createLabels
                                    , createNotes = loggedInModel.offlineQueue.createNotes

                                    --
                                    , deleteLabels =
                                        loggedInModel.offlineQueue.deleteLabels
                                            |> List.map
                                                (\deleteLabelId ->
                                                    let
                                                        labelHasBeenCreated =
                                                            listFirst (\( onlineId, prevOfflineId ) -> sameId (OfflineID prevOfflineId) deleteLabelId) justCreatedData.labels
                                                    in
                                                    case labelHasBeenCreated of
                                                        Just ( justCreatedLabelId, _ ) ->
                                                            DatabaseID justCreatedLabelId.id

                                                        Nothing ->
                                                            deleteLabelId
                                                )
                                            |> exclude (\en -> List.any (\failedId -> en == OfflineID failedId) failedToCreate)
                                    , deleteNotes =
                                        loggedInModel.offlineQueue.deleteNotes
                                            |> List.map
                                                (\deleteNoteId ->
                                                    let
                                                        noteHasBeenCreated =
                                                            listFirst (\( onlineId, prevOfflineId ) -> sameId (OfflineID prevOfflineId) deleteNoteId) justCreatedData.notes
                                                    in
                                                    case noteHasBeenCreated of
                                                        Just ( justCreatedNoteId, _ ) ->
                                                            DatabaseID justCreatedNoteId.id

                                                        Nothing ->
                                                            deleteNoteId
                                                )
                                            |> exclude (\en -> List.any (\failedId -> en == OfflineID failedId) failedToCreate)
                                    , editNotes =
                                        loggedInModel.offlineQueue.editNotes
                                            |> List.map
                                                (\editNote ->
                                                    let
                                                        noteHasBeenCreated =
                                                            listFirst (\( onlineId, prevOfflineId ) -> sameId (OfflineID prevOfflineId) editNote.id) justCreatedData.notes
                                                    in
                                                    case noteHasBeenCreated of
                                                        Just ( justCreatedNote, _ ) ->
                                                            { editNote | id = DatabaseID justCreatedNote.id }

                                                        Nothing ->
                                                            editNote
                                                )
                                            |> exclude (\en -> List.any (\failedId -> en.id == OfflineID failedId) failedToCreate)
                                    , changeLabelNames =
                                        loggedInModel.offlineQueue.changeLabelNames
                                            |> List.map
                                                (\changeLabelName ->
                                                    let
                                                        labelHasBeenCreated =
                                                            listFirst (\( onlineId, prevOfflineId ) -> sameId (OfflineID prevOfflineId) changeLabelName.id) justCreatedData.labels
                                                    in
                                                    case labelHasBeenCreated of
                                                        Just ( justCreatedLabelId, _ ) ->
                                                            { changeLabelName | id = DatabaseID justCreatedLabelId.id }

                                                        Nothing ->
                                                            changeLabelName
                                                )
                                            |> exclude (\en -> List.any (\failedId -> en.id == OfflineID failedId) failedToCreate)
                                    }
                            in
                            ( LoggedIn
                                { page = updatedPageModel
                                , offlineQueue = emptyOfflineQueue
                                , isOnline = loggedInModel.isOnline
                                , key = loggedInModel.key
                                , labelsMenu = loggedInModel.labelsMenu
                                , filters = loggedInModel.filters
                                , searchBarQuery = loggedInModel.searchBarQuery
                                , runningQueueOn =
                                    if offlineQueueIsEmpty updatedOfflineQueue then
                                        Nothing

                                    else
                                        Just updatedOfflineQueue
                                , lastSyncedAt = justSyncedAt
                                , windowRes = loggedInModel.windowRes
                                }
                            , Cmd.batch
                                [ cmd1
                                , updateLastSyncedAt (Time.posixToMillis justSyncedAt)
                                , if offlineQueueIsEmpty updatedOfflineQueue then
                                    Cmd.none

                                  else
                                    Api.sendChanges
                                        { operations = queueToOperations updatedOfflineQueue
                                        , lastSyncedAt = justSyncedAt
                                        , currentData =
                                            { notes = notes |> List.map .id |> labelIDsSplitter |> Tuple.second
                                            , labels = labels |> List.map .id |> labelIDsSplitter |> Tuple.second
                                            }
                                        }
                                        ReceivedChangesResp
                                ]
                            )

                        Err _ ->
                            -- TODO: error handling here
                            topModel |> pure

        OnChangedSearchBarQuery newQuery ->
            loggedInMap (\model -> { model | searchBarQuery = newQuery })
                |> pure

        ReturnHome ->
            case topModel of
                LoggedOff _ ->
                    topModel |> pure

                LoggedIn model ->
                    ( LoggedIn
                        { model
                            | page =
                                case model.page of
                                    Home _ ->
                                        model.page

                                    EditNote editingNoteModel ->
                                        Home.init
                                            { seeds = editingNoteModel.seeds
                                            , labels = editingNoteModel.labels
                                            , notes = editingNoteModel.notes
                                            , key = editingNoteModel.key
                                            }
                                            |> Home

                                    EditLabels editLabelsModel ->
                                        Home.init
                                            { seeds = editLabelsModel.seeds
                                            , labels = editLabelsModel.labels
                                            , notes = editLabelsModel.notes
                                            , key = editLabelsModel.key
                                            }
                                            |> Home
                        }
                    , Route.replaceUrl model.key Route.Home
                    )

        -- Labels column menu
        OpenLabelsMenu ->
            loggedInMap (\model -> { model | labelsMenu = Just "" })
                |> pure

        CloseLabelsMenu ->
            loggedInMap (\model -> { model | labelsMenu = Nothing })
                |> pure

        ChangeLabelsSearchQuery s ->
            loggedInMap (\model -> { model | labelsMenu = Maybe.map (\_ -> s) model.labelsMenu })
                |> pure

        ClearSearchBarQuery ->
            loggedInMap (\model -> { model | searchBarQuery = "" })
                |> pure

        SelectLabelToFilterBy id ->
            loggedInMap
                (\model ->
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
                )
                |> pure

        GoToEditLabelsScreen ->
            case topModel of
                LoggedOff _ ->
                    topModel |> pure

                LoggedIn loggedInModel ->
                    ( LoggedIn { loggedInModel | labelsMenu = Nothing }, Route.replaceUrl loggedInModel.key Route.EditLabels )


updateWith toModel toMsg topModel ( m, c ) =
    ( { topModel | page = toModel m }, Cmd.map GotPageMsg (Cmd.map toMsg c) )


updateHomeWithSignal : LoggedInModel -> ( Home.Model, Cmd Home.Msg, Maybe Home.Signal ) -> ( Model, Cmd Msg )
updateHomeWithSignal topModel ( model, cmd, maybeSignal ) =
    let
        ( mappedModel, mappedCmd ) =
            ( { topModel | page = Home model }, Cmd.map (GotHomeMsg >> GotPageMsg) cmd )
    in
    (case maybeSignal of
        Nothing ->
            ( mappedModel, mappedCmd )

        Just signal ->
            let
                ( labelIds, noteIds ) =
                    ( model.labels |> List.map .id |> labelIDsSplitter |> Tuple.second, model.notes |> List.map .id |> labelIDsSplitter |> Tuple.second )
            in
            case signal of
                Home.OfflineQueueAction action ->
                    ( mappedModel, mappedCmd )
                        |> addToQueue
                            (actionMapToFn action)
                            noteIds
                            labelIds
                            topModel.isOnline

                Home.ClearLabelFilters ->
                    ( { mappedModel
                        | filters =
                            { label = Nothing
                            , content = mappedModel.filters.content
                            }
                      }
                    , mappedCmd
                    )
    )
        |> (\( m1, c1 ) -> ( LoggedIn m1, c1 ))


updateEditLabelsWithSignal : (a -> Page) -> (c -> PageMsg) -> LoggedInModel -> ( a, Cmd c, Maybe EditLabels.Signal ) -> ( Model, Cmd Msg )
updateEditLabelsWithSignal toPageModel toPageMsg topModel ( m, c, maybeSignal ) =
    let
        ( mappedModel, mappedCmd ) =
            ( { topModel | page = toPageModel m }, Cmd.map GotPageMsg (Cmd.map toPageMsg c) )
    in
    (case maybeSignal of
        Nothing ->
            ( mappedModel, mappedCmd )

        Just signal ->
            let
                ( labelIds, noteIds ) =
                    (case topModel.page of
                        Home homeModel ->
                            ( homeModel.labels, homeModel.notes )

                        EditLabels editLabelsModel ->
                            ( editLabelsModel.labels, editLabelsModel.notes )

                        EditNote editNoteModel ->
                            ( editNoteModel.labels, editNoteModel.notes )
                    )
                        |> (\( l, n ) -> ( l |> List.map .id |> labelIDsSplitter |> Tuple.second, n |> List.map .id |> labelIDsSplitter |> Tuple.second ))
            in
            ( mappedModel, mappedCmd )
                |> addToQueue
                    (case signal of
                        EditLabels.OfflineQueueAction action ->
                            actionMapToFn action
                    )
                    noteIds
                    labelIds
                    topModel.isOnline
    )
        |> (\( m1, c1 ) -> ( LoggedIn m1, c1 ))


updateEditNoteWithSignal : (a -> Page) -> (c -> PageMsg) -> LoggedInModel -> ( a, Cmd c, Maybe EditNote.Signal ) -> ( Model, Cmd Msg )
updateEditNoteWithSignal toPageModel toPageMsg topModel ( m, c, maybeSignal ) =
    let
        ( mappedModel, mappedCmd ) =
            ( { topModel | page = toPageModel m }, Cmd.map GotPageMsg (Cmd.map toPageMsg c) )
    in
    (case maybeSignal of
        Nothing ->
            ( mappedModel, mappedCmd )

        Just signal ->
            let
                ( labelIds, noteIds ) =
                    (case topModel.page of
                        Home homeModel ->
                            ( homeModel.labels, homeModel.notes )

                        EditLabels editLabelsModel ->
                            ( editLabelsModel.labels, editLabelsModel.notes )

                        EditNote editNoteModel ->
                            ( editNoteModel.labels, editNoteModel.notes )
                    )
                        |> (\( l, n ) -> ( l |> List.map .id |> labelIDsSplitter |> Tuple.second, n |> List.map .id |> labelIDsSplitter |> Tuple.second ))
            in
            ( mappedModel, mappedCmd )
                |> addToQueue
                    (case signal of
                        EditNote.OfflineQueueAction action ->
                            actionMapToFn action
                    )
                    noteIds
                    labelIds
                    topModel.isOnline
    )
        |> (\( m1, c1 ) -> ( LoggedIn m1, c1 ))


addToQueue : (OfflineQueueOps -> OfflineQueueOps) -> List Api.DbID -> List Api.DbID -> Bool -> ( LoggedInModel, Cmd Msg ) -> ( LoggedInModel, Cmd Msg )
addToQueue operation notesIds labelsIds isOnline ( model, cmds ) =
    let
        currentOperations =
            model.offlineQueue |> operation
    in
    case model.runningQueueOn of
        Nothing ->
            if isOnline then
                ( { model
                    | offlineQueue = emptyOfflineQueue
                    , runningQueueOn = Just currentOperations
                  }
                , Cmd.batch
                    [ Api.sendChanges
                        { operations = queueToOperations currentOperations
                        , lastSyncedAt = model.lastSyncedAt
                        , currentData =
                            { notes = notesIds
                            , labels = labelsIds
                            }
                        }
                        ReceivedChangesResp
                    , cmds
                    ]
                )

            else
                ( { model | offlineQueue = currentOperations }, cmds )

        Just _ ->
            ( { model | offlineQueue = currentOperations }, cmds )



-- VIEW


navbar : Page -> Int -> Bool -> Maybe Bool -> String -> Html Msg
navbar page labelsAmount isLabelsMenuOpen isOnline searchBarQuery =
    let
        btnMxn =
            Css.batch
                [ backgroundColor white
                , border (px 0)
                , padding (px 8)
                , displayFlex
                , justifyContent center
                , alignItems center
                , cursor pointer
                , hover [ backgroundColor black, color white ]
                ]

        labelsMenuBtn =
            case page of
                EditNote _ ->
                    text ""

                Home _ ->
                    button
                        [ css
                            [ btnMxn
                            , borderRight3 (px 3) solid black
                            ]
                        , onClick OpenLabelsMenu
                        ]
                        [ Filled.label 32 Inherit |> Svg.Styled.fromUnstyled ]

                EditLabels _ ->
                    text ""

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

        homeBtn =
            let
                btn hasBorderLeft =
                    button
                        [ css
                            [ btnMxn
                            , if isLabelsMenuOpen || not hasBorderLeft then
                                Css.batch []

                              else
                                borderLeft3 (px 3) solid black
                            , borderRight3 (px 3) solid black
                            ]
                        , onClick ReturnHome
                        ]
                        [ Filled.home 32 Inherit |> Svg.Styled.fromUnstyled ]
            in
            case page of
                EditNote _ ->
                    btn False

                Home _ ->
                    text ""

                EditLabels _ ->
                    btn False

        changeViewBtn =
            case page of
                EditNote _ ->
                    text ""

                Home _ ->
                    button
                        [ css
                            [ btnMxn
                            , borderLeft3 (px 3) solid black
                            , borderRight3 (px 3) solid black
                            ]
                        ]
                        [ Filled.view_module 32 Inherit |> Svg.Styled.fromUnstyled ]

                EditLabels _ ->
                    text ""

        syncBtn =
            let
                btn hasBorderLeft =
                    button
                        [ css
                            [ btnMxn
                            , if hasBorderLeft then
                                borderLeft3 (px 3) solid black

                              else
                                Css.batch []
                            ]
                        ]
                        [ -- TODO: add icon for when offline and yet queued "cloud_queue"
                          (case isOnline of
                            Just isSyncing ->
                                if isSyncing then
                                    Outlined.cloud_upload

                                else
                                    Outlined.sync

                            Nothing ->
                                Outlined.wifi_off
                          )
                            28
                            Inherit
                            |> Svg.Styled.fromUnstyled
                        ]
            in
            case page of
                EditNote _ ->
                    btn True

                Home _ ->
                    btn False

                EditLabels _ ->
                    btn True
    in
    nav
        [ css
            [ backgroundColor secondary
            , borderBottom3 (px 3) solid (rgb 0 0 0)
            , position sticky
            , top (px 0)
            , displayFlex
            , alignItems center
            , justifyContent spaceBetween
            , width (pct 100)
            ]
        ]
        [ row []
            [ if isLabelsMenuOpen then
                openLabelsMenuBtn (labelsAmount |> String.fromInt)

              else
                labelsMenuBtn
            , homeBtn
            ]
        , row
            [ css
                [ mx (px 16)
                , border3 (px 2) solid black
                , property "flex" "1 1 auto"
                , maxWidth (px 683)
                ]
            ]
            [ input
                [ css
                    [ width (pct 100)
                    , maxWidth (pct 100)
                    , padding (px 6)
                    , publicSans
                    , fontWeight (int 400)
                    , fontSize (px 16)
                    , textAlign center
                    , borderRight (px 0)
                    , border (px 0)
                    ]
                , onInput OnChangedSearchBarQuery
                , value searchBarQuery
                , placeholder "Search"
                ]
                []
            , case searchBarQuery of
                "" ->
                    text ""

                _ ->
                    button
                        [ css [ displayFlex, alignItems center, cursor pointer, backgroundColor black, color white, border (px 0), borderLeft3 (px 2) solid black, hover [ color black, backgroundColor white ] ]
                        , onClick ClearSearchBarQuery
                        ]
                        [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
            ]
        , row [ css [ height (pct 100) ] ]
            [ changeViewBtn
            , syncBtn
            ]
        ]


labelsMenuWidth : number
labelsMenuWidth =
    277


labelsMenuColumn : { labels : List Label, filters : { c | label : Maybe SyncableID }, labelsMenu : Maybe String } -> Html Msg
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


pageView : { a | labels : List Label, page : Page, labelsMenu : Maybe String, isOnline : Bool, searchBarQuery : String, runningQueueOn : Maybe b, filters : { c | label : Maybe SyncableID }, windowRes : g } -> Html Msg -> Html Msg
pageView { labels, page, labelsMenu, isOnline, runningQueueOn, filters, searchBarQuery, windowRes } individualPageView =
    col
        [ css
            [ displayFlex
            , flexDirection column
            , height (pct 100)
            , overflow auto
            ]
        ]
        [ navbar page
            (labels |> List.length)
            (maybeToBool labelsMenu)
            (if isOnline then
                Just (maybeToBool runningQueueOn)

             else
                Nothing
            )
            searchBarQuery
        , row
            [ css
                [ displayFlex
                , height (pct 100)
                , overflow hidden
                ]
            ]
            [ if maybeToBool labelsMenu then
                labelsMenuColumn { labels = labels, filters = filters, labelsMenu = labelsMenu }

              else
                text ""
            , individualPageView
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ id "full-container"
        , css
            [ height (pct 100)
            , width (pct 100)
            , backgroundColor (rgb 18 104 85)
            , backgroundImage (url "/media/bgr.png ")
            , backgroundSize contain
            , backgroundRepeat repeat
            ]
        ]
        [ case model of
            LoggedOff logInModel ->
                Html.Styled.map (GotLogInMsg >> GotPageMsg) (LogIn.logInView logInModel)

            LoggedIn { page, runningQueueOn, windowRes, isOnline, filters, labelsMenu, searchBarQuery } ->
                case page of
                    Home homeModel ->
                        pageView { searchBarQuery = searchBarQuery, labels = homeModel.labels, page = page, labelsMenu = labelsMenu, isOnline = isOnline, runningQueueOn = runningQueueOn, filters = filters, windowRes = windowRes }
                            (Home.view homeModel windowRes filters (maybeToBool labelsMenu) |> Html.Styled.map (GotHomeMsg >> GotPageMsg))

                    EditLabels editLabelsModel ->
                        pageView { searchBarQuery = searchBarQuery, labels = editLabelsModel.labels, page = page, labelsMenu = labelsMenu, isOnline = isOnline, runningQueueOn = runningQueueOn, filters = filters, windowRes = windowRes }
                            (EditLabels.view editLabelsModel |> Html.Styled.map (GotEditLabelsMsg >> GotPageMsg))

                    EditNote editNoteModel ->
                        pageView { searchBarQuery = searchBarQuery, labels = editNoteModel.labels, page = page, labelsMenu = labelsMenu, isOnline = isOnline, runningQueueOn = runningQueueOn, filters = filters, windowRes = windowRes }
                            (EditNote.view editNoteModel |> Html.Styled.map (GotEditNoteMsg >> GotPageMsg))
        ]
