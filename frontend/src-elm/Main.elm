module Main exposing (..)

import Api exposing (Operation(..), SyncableID(..))
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, fullWidth, height, pct, repeat, rgb, url, width)
import Dog exposing (dogSvg)
import Either exposing (Either(..))
import Html
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Page.EditLabels as EditLabels
import Page.Home as Home
import Page.LogIn as LogIn
import Ports exposing (requestRandomValues)
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
        LogIn _ logInModel ->
            -- TODO:
            Sub.none

        -- LogIn.subscriptions logInModel |> Sub.map GotLogInMsg
        Home homeModel ->
            Home.subscriptions homeModel |> Sub.map GotHomeMsg

        EditLabels editLabelsModel ->
            -- TODO:
            -- EditLabels.subscriptions editLabelsModel |> Sub.map GotEditLabelsMsg
            Sub.none



-- MODEL


type Model
    = LogIn (List Random.Seed) LogIn.Model
      -- TODO: check if session is valid on entering website,
      -- then go to either logIn or Home
    | Home Home.Model
    | EditLabels EditLabels.Model



-- MESSAGE


type Msg
    = GotLogInMsg LogIn.Msg
    | GotHomeMsg Home.Msg
    | GotEditLabelsMsg EditLabels.Msg
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)
    | ClickedLink UrlRequest
    | ChangedUrl Url



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

                        EditLabels _ ->
                            case topModel of
                                Home homeModel ->
                                    ( EditLabels
                                        (EditLabels.init
                                            { seeds = homeModel.seeds
                                            , labels = homeModel.labels
                                            , notes = homeModel.notes
                                            , offlineQueue = homeModel.offlineQueue
                                            , runningQueueOn = homeModel.runningQueueOn
                                            , lastSyncedAt = homeModel.lastSyncedAt
                                            }
                                        )
                                    , Cmd.none
                                    )

                                _ ->
                                    -- TODO:
                                    topModel |> pure

                Route.LogIn ->
                    -- TODO:
                    ( topModel, Cmd.none )

                Route.EditLabels ->
                    -- TODO:
                    ( topModel, Cmd.none )
            )

        ( GotLogInMsg loginMsg, LogIn randSeeds logInModel ) ->
            LogIn.update loginMsg logInModel
                |> (\( m, c ) -> ( LogIn randSeeds m, Cmd.map GotLogInMsg c ))

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( topModel, Cmd.none )


updateWith toModel toMsg ( m, c ) =
    ( toModel m, Cmd.map toMsg c )



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
        [ case model of
            LogIn seeds logInModel ->
                Html.Styled.map GotLogInMsg (LogIn.logInView { username = logInModel.username, password = logInModel.password, key = logInModel.key })

            Home homeModel ->
                Html.Styled.map GotHomeMsg (Home.view homeModel)

            EditLabels editLabelsModel ->
                Html.Styled.map GotEditLabelsMsg (EditLabels.view editLabelsModel)
        ]
