module Main exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, fullWidth, height, pct, repeat, rgb, url, width)
import DataTypes exposing (Label, Note)
import Dog exposing (dogSvg)
import Either exposing (Either(..))
import Helpers exposing (exclude, labelIDsSplitter, listFirst, maybeToBool, sameId)
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
import Ports exposing (requestRandomValues, updateLastSyncedAt)
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
            let
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



-- MODEL


type Page
    = Home Home.Model
    | EditLabels EditLabels.Model
    | EditNote EditNote.Model


type alias LoggedInModel =
    { page : Page

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



-- INIT


type alias Flags =
    { seeds : List Int, hasSessionCookie : Bool, lastSyncedAt : Int }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        seeds =
            List.map Random.initialSeed flags.seeds
    in
    if flags.hasSessionCookie then
        ( LoggedOff (LogIn.init navKey seeds), Cmd.none )

    else
        ( LoggedIn
            { offlineQueue = emptyOfflineQueue
            , runningQueueOn = Nothing
            , lastSyncedAt = Time.millisToPosix flags.lastSyncedAt
            , page =
                case Route.fromUrl url of
                    Route.EditLabels ->
                        EditLabels (EditLabels.init { seeds = seeds, labels = [], notes = [], key = navKey })

                    Route.Home ->
                        Home (Home.init { key = navKey, seeds = seeds, labels = [], notes = [] })

                    Route.LogIn ->
                        Home (Home.init { key = navKey, seeds = seeds, labels = [], notes = [] })

                    Route.EditNote noteId ->
                        EditNote (EditNote.init { noteId = noteId, key = navKey, seeds = seeds, labels = [], notes = [] })
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
        -- TODO: fix this
        ClickedLink _ ->
            -- TODO:
            topModel |> pure

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
                                }
                            , Cmd.batch [ Api.fullSync FullSyncResp, requestRandomValues () ]
                            )

                        Route.EditLabels ->
                            topModel |> pure

                        Route.LogIn ->
                            topModel |> pure

                        Route.EditNote _ ->
                            topModel |> pure

                LoggedIn loggedInModel ->
                    case Route.fromUrl newUrl of
                        Route.Home ->
                            case loggedInModel.page of
                                Home _ ->
                                    -- TODO: better
                                    topModel |> pure

                                EditLabels data ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                Home
                                                    (Home.init
                                                        { seeds = data.seeds
                                                        , labels = data.labels
                                                        , notes = data.notes
                                                        , key = data.key
                                                        }
                                                    )
                                        }
                                        |> pure

                                EditNote data ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                Home
                                                    (Home.init
                                                        { seeds = data.seeds
                                                        , labels = data.labels
                                                        , notes = data.notes
                                                        , key = data.key
                                                        }
                                                    )
                                        }
                                        |> pure

                        Route.EditLabels ->
                            -- TODO:
                            case loggedInModel.page of
                                Home data ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                EditLabels
                                                    (EditLabels.init
                                                        { seeds = data.seeds
                                                        , labels = data.labels
                                                        , notes = data.notes
                                                        , key = data.key
                                                        }
                                                    )
                                        }
                                        |> pure

                                _ ->
                                    ( topModel, Cmd.none )

                        Route.EditNote noteId ->
                            case loggedInModel.page of
                                Home data ->
                                    LoggedIn
                                        { loggedInModel
                                            | page =
                                                EditNote
                                                    (EditNote.init
                                                        { seeds = data.seeds
                                                        , labels = data.labels
                                                        , notes = data.notes
                                                        , noteId = noteId
                                                        , key = data.key
                                                        }
                                                    )
                                        }
                                        |> pure

                                _ ->
                                    ( topModel, Cmd.none )

                        Route.LogIn ->
                            ( topModel, Cmd.none )
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
                        Ok { deleted, failedToCreate, failedToEdit, justSyncedAt, downSyncedData, justCreatedData } ->
                            let
                                updatePageModel m =
                                    { m
                                        | notes =
                                            let
                                                ( _, notOutdatedNotes ) =
                                                    List.partition
                                                        (\e -> List.any (\l -> sameId (DatabaseID l.id) e.id) downSyncedData.notes)
                                                        m.notes

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
                                                        m.labels

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
                                    }

                                updatedPageModel =
                                    case loggedInModel.page of
                                        Home homeModel ->
                                            Home (updatePageModel homeModel)

                                        EditLabels editLabelsModel ->
                                            EditLabels (updatePageModel editLabelsModel)

                                        EditNote editNoteModel ->
                                            EditNote (updatePageModel editNoteModel)

                                ( labels, notes ) =
                                    case updatedPageModel of
                                        Home homeModel ->
                                            ( homeModel.labels, homeModel.notes )

                                        EditLabels editLabelsModel ->
                                            ( editLabelsModel.labels, editLabelsModel.notes )

                                        EditNote editNoteModel ->
                                            ( editNoteModel.labels, editNoteModel.notes )
                            in
                            ( LoggedIn
                                { page = updatedPageModel
                                , offlineQueue = emptyOfflineQueue
                                , runningQueueOn =
                                    if offlineQueueIsEmpty loggedInModel.offlineQueue then
                                        Nothing

                                    else
                                        Just loggedInModel.offlineQueue
                                , lastSyncedAt = justSyncedAt
                                }
                            , Cmd.batch
                                [ updateLastSyncedAt (Time.posixToMillis justSyncedAt)
                                , if offlineQueueIsEmpty loggedInModel.offlineQueue then
                                    Cmd.none

                                  else
                                    Api.sendChanges
                                        { operations = queueToOperations loggedInModel.offlineQueue
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
    )
        |> (\( m1, c1 ) -> ( LoggedIn m1, c1 ))


addToQueue : (OfflineQueueOps -> OfflineQueueOps) -> List Api.DbID -> List Api.DbID -> ( LoggedInModel, Cmd Msg ) -> ( LoggedInModel, Cmd Msg )
addToQueue operation notesIds labelsIds ( model, cmds ) =
    let
        currentOperations =
            model.offlineQueue |> operation
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
                        { notes = notesIds
                        , labels = labelsIds
                        }
                    }
                    ReceivedChangesResp
                , cmds
                ]
            )

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

            LoggedIn { page, runningQueueOn } ->
                case page of
                    Home homeModel ->
                        Html.Styled.map GotHomeMsg (Home.view homeModel (maybeToBool runningQueueOn))

                    EditLabels editLabelsModel ->
                        Html.Styled.map GotEditLabelsMsg (EditLabels.view editLabelsModel)

                    EditNote editNoteModel ->
                        Html.Styled.map GotEditNoteMsg (EditNote.view editNoteModel)
          )
            |> Html.Styled.map GotPageMsg
        ]
