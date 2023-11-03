module Main exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, fullWidth, height, pct, repeat, rgb, url, width)
import DataTypes exposing (Label, Note)
import Dog exposing (dogSvg)
import Either exposing (Either(..))
import Helpers exposing (elIsIn, exclude, labelIDsSplitter, listFirst, mapToWithDefault, maybeToBool, sameId)
import Html
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
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


type alias LoggedInModel =
    { page : Page
    , isOnline : Bool
    , windowRes : { width : Int, height : Int }

    -- sync stuff
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    , lastSyncedAt : Posix
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
    case topMsg of
        IsOffline ->
            case topModel of
                LoggedIn m ->
                    LoggedIn { m | isOnline = False } |> pure

                LoggedOff _ ->
                    topModel |> pure

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
                                |> updateHomeWithSignal Home GotHomeMsg loggedInModel

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


updateWith toModel toMsg topModel ( m, c ) =
    ( { topModel | page = toModel m }, Cmd.map GotPageMsg (Cmd.map toMsg c) )


updateHomeWithSignal : (a -> Page) -> (c -> PageMsg) -> LoggedInModel -> ( a, Cmd c, Maybe Home.Signal ) -> ( Model, Cmd Msg )
updateHomeWithSignal toPageModel toPageMsg topModel ( m, c, maybeSignal ) =
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
                        Home.OfflineQueueAction action ->
                            actionMapToFn action
                    )
                    noteIds
                    labelIds
                    topModel.isOnline
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


view : Model -> Html Msg
view model =
    div
        [ id "full-container"
        , css
            [ height (pct 100)
            , width (pct 100)
            , backgroundColor (rgb 18 104 85)
            , backgroundImage (url "./media/bgr.png ")
            , backgroundSize contain
            , backgroundRepeat repeat
            ]
        ]
        [ (case model of
            LoggedOff logInModel ->
                Html.Styled.map GotLogInMsg (LogIn.logInView logInModel)

            LoggedIn { page, runningQueueOn, windowRes, isOnline } ->
                case page of
                    Home homeModel ->
                        Html.Styled.map GotHomeMsg
                            (Home.view homeModel
                                windowRes
                                (if isOnline then
                                    Just (maybeToBool runningQueueOn)

                                 else
                                    Nothing
                                )
                            )

                    EditLabels editLabelsModel ->
                        Html.Styled.map GotEditLabelsMsg (EditLabels.view editLabelsModel)

                    EditNote editNoteModel ->
                        Html.Styled.map GotEditNoteMsg (EditNote.view editNoteModel)
          )
            |> Html.Styled.map GotPageMsg
        ]
