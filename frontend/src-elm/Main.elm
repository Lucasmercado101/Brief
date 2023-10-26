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
import OfflineQueue exposing (OfflineQueueOps, emptyOfflineQueue)
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
    case model.page of
        LogIn logInModel ->
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


type Msg
    = ClickedLink UrlRequest
    | ChangedUrl Url
      -- Pages Msg
    | GotLogInMsg LogIn.Msg
    | GotHomeMsg Home.Msg
    | GotEditLabelsMsg EditLabels.Msg
      --
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)



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
    case ( topMsg, topModel.page ) of
        -- TODO: fix this
        ( ClickedLink _, _ ) ->
            -- TODO:
            topModel |> pure

        ( ChangedUrl newUrl, _ ) ->
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

        ( GotLogInMsg loginMsg, LogIn logInModel ) ->
            LogIn.update loginMsg logInModel
                |> updateWith LogIn GotLogInMsg topModel

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg topModel

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            topModel |> pure


updateWith toModel toMsg topModel ( m, c ) =
    ( { topModel | page = toModel m }, Cmd.map toMsg c )



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
        [ case model.page of
            LogIn logInModel ->
                Html.Styled.map GotLogInMsg (LogIn.logInView logInModel)

            Home homeModel ->
                -- TODO: fix this
                div [] []

            -- Html.Styled.map GotHomeMsg (Home.view homeModel)
            EditLabels editLabelsModel ->
                -- TODO: fix this
                div [] []

        -- Html.Styled.map GotEditLabelsMsg (EditLabels.view editLabelsModel)
        ]
