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
import OfflineQueue exposing (OfflineQueueOps, emptyOfflineQueue, offlineQueueIsEmpty, queueToOperations)
import Page.EditLabels as EditLabels
import Page.Home as Home
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
    case model.page of
        LogIn logInModel ->
            -- TODO:
            Sub.none

        -- LogIn.subscriptions logInModel |> Sub.map GotLogInMsg
        Home homeModel ->
            Home.subscriptions homeModel
                |> Sub.map (\e -> GotPageMsg (GotHomeMsg e))

        -- TODO:
        -- Home.subscriptions homeModel |> Sub.map GotHomeMsg
        EditLabels editLabelsModel ->
            -- TODO:
            -- EditLabels.subscriptions editLabelsModel |> Sub.map GotEditLabelsMsg
            Sub.none



-- MODEL


type Page
    = LogIn LogIn.Model
      -- TODO: check if session is valid on entering website,
      -- then go to either logIn or Home
    | Home Home.Model
    | EditLabels EditLabels.Model


type alias Model =
    { page : Page

    -- sync stuff
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    , lastSyncedAt : Posix
    }



-- MESSAGE


type PageMsg
    = GotLogInMsg LogIn.Msg
    | GotHomeMsg Home.Msg
    | GotEditLabelsMsg EditLabels.Msg


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
    ( { offlineQueue = emptyOfflineQueue
      , runningQueueOn = Nothing
      , lastSyncedAt = Time.millisToPosix flags.lastSyncedAt
      , page =
            let
                seeds =
                    List.map Random.initialSeed flags.seeds
            in
            if flags.hasSessionCookie then
                Home (Home.init { navKey = navKey, seeds = seeds })

            else
                LogIn (LogIn.init navKey seeds)
      }
    , Cmd.none
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
             case Route.fromUrl newUrl of
                Route.Home ->
                    case topModel.page of
                        Home _ ->
                            -- TODO: better
                            topModel |> pure

                        LogIn { seeds, key } ->
                            ( { topModel
                                | page =
                                    Home
                                        { -- TODO: do this in a better way
                                          key = key
                                        , seeds = seeds
                                        , notes = []
                                        , isWritingANewNote = Nothing
                                        , newLabelName = ""
                                        , labels = []
                                        , labelsMenu = Nothing
                                        , filters =
                                            { label = Nothing
                                            , content = Nothing
                                            }
                                        }
                              }
                            , Cmd.batch [ Api.fullSync FullSyncResp, requestRandomValues () ]
                            )

                        EditLabels _ ->
                            topModel |> pure

                -- TODO:
                -- case topModel of
                --     Home homeModel ->
                --         ( EditLabels
                --             (EditLabels.init
                --                 { seeds = homeModel.seeds
                --                 , labels = homeModel.labels
                --                 , notes = homeModel.notes
                --                 , offlineQueue = homeModel.offlineQueue
                --                 , runningQueueOn = homeModel.runningQueueOn
                --                 , lastSyncedAt = homeModel.lastSyncedAt
                --                 }
                --             )
                --         , Cmd.none
                --         )
                --     _ ->
                --         -- TODO:
                --         topModel |> pure
                Route.LogIn ->
                    -- TODO:
                    ( topModel, Cmd.none )

                Route.EditLabels ->
                    -- TODO:
                    ( topModel, Cmd.none )
            )

        GotPageMsg pageMsg ->
            case ( pageMsg, topModel.page ) of
                ( GotLogInMsg loginMsg, LogIn logInModel ) ->
                    LogIn.update loginMsg logInModel
                        |> updateWith LogIn GotLogInMsg topModel

                ( GotHomeMsg homeMsg, Home homeModel ) ->
                    Home.update homeMsg homeModel
                        |> updateWith Home GotHomeMsg topModel

                ( GotEditLabelsMsg editLabelsMsg, EditLabels editLabelsModel ) ->
                    EditLabels.update editLabelsMsg editLabelsModel
                        |> updateWith EditLabels GotEditLabelsMsg topModel

                ( _, _ ) ->
                    -- Disregard messages that arrived for the wrong page.
                    topModel |> pure

        FullSyncResp res ->
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
                    { topModel
                        | page =
                            case topModel.page of
                                Home homeModel ->
                                    Home (updatedPageModel homeModel)

                                EditLabels editLabelsModel ->
                                    EditLabels (updatedPageModel editLabelsModel)

                                LogIn logInModel ->
                                    -- TODO: separate LogIn from pages msg stuff
                                    LogIn logInModel
                    }
                        |> pure

                Err v ->
                    -- TODO: handle 403
                    topModel |> pure

        ReceivedChangesResp resp ->
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
                            case topModel.page of
                                Home homeModel ->
                                    Home (updatePageModel homeModel)

                                EditLabels editLabelsModel ->
                                    EditLabels (updatePageModel editLabelsModel)

                                LogIn logInModel ->
                                    -- TODO: separate page
                                    LogIn logInModel

                        labels : List Label
                        labels =
                            case updatedPageModel of
                                Home homeModel ->
                                    homeModel.labels

                                EditLabels editLabelsModel ->
                                    editLabelsModel.labels

                                LogIn logInModel ->
                                    -- TODO: separate page
                                    []

                        notes : List Note
                        notes =
                            case updatedPageModel of
                                Home homeModel ->
                                    homeModel.notes

                                EditLabels editLabelsModel ->
                                    editLabelsModel.notes

                                LogIn logInModel ->
                                    -- TODO: separate page
                                    []
                    in
                    ( { topModel
                        | page = updatedPageModel
                        , offlineQueue = emptyOfflineQueue
                        , runningQueueOn =
                            if offlineQueueIsEmpty topModel.offlineQueue then
                                Nothing

                            else
                                Just topModel.offlineQueue
                        , lastSyncedAt = justSyncedAt
                      }
                    , Cmd.batch
                        [ updateLastSyncedAt (Time.posixToMillis justSyncedAt)
                        , if offlineQueueIsEmpty topModel.offlineQueue then
                            Cmd.none

                          else
                            Api.sendChanges
                                { operations = queueToOperations topModel.offlineQueue
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
        [ (case model.page of
            LogIn logInModel ->
                Html.Styled.map GotLogInMsg (LogIn.logInView logInModel)

            Home homeModel ->
                Html.Styled.map GotHomeMsg (Home.view homeModel (maybeToBool model.runningQueueOn))

            EditLabels editLabelsModel ->
                -- TODO: fix this
                div [] []
          )
            |> Html.Styled.map GotPageMsg

        -- Html.Styled.map GotEditLabelsMsg (EditLabels.view editLabelsModel)
        ]
