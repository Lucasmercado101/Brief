port module Main exposing (..)

import Api
import Browser
import Cmd.Extra exposing (pure)
import Css exposing (..)
import Either exposing (Either(..))
import Html
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



-- PORTS


port requestRandomValues : () -> Cmd msg


port receiveRandomValues : (List Int -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- TODO: if case model is logged in?
    receiveRandomValues ReceivedRandomValues
        |> Sub.map LoggedInView


dummyNewNote : Maybe NewNoteData
dummyNewNote =
    Just
        { title = ""
        , content = ""
        , labels = Nothing
        }



-- MODEL


type ID
    = -- Not synced with DB yet,
      -- generated ID offline
      OfflineID String
      -- Synced with DB,
      -- using DB's ID
    | DatabaseID Api.ID


idDiff : ID -> ID -> Bool
idDiff a b =
    case ( a, b ) of
        ( OfflineID c, OfflineID d ) ->
            c /= d

        ( DatabaseID c, DatabaseID d ) ->
            c /= d

        _ ->
            True


sameId : ID -> ID -> Bool
sameId a b =
    not (idDiff a b)


type alias UniqueStr =
    String


type alias Note =
    { id : ID
    , title : String
    , content : String
    , pinned : Bool
    , labels : List ID
    }


type alias Label =
    { id : ID
    , name : UniqueStr
    }


type alias LoggedOutModel =
    { username : String
    , password : String
    }


type User
    = LoggedOut LoggedOutModel
    | CheckingSessionValidity
    | LoggedIn


type alias Model =
    { seeds : List Random.Seed
    , notes : List Note
    , isWritingANewNote : Maybe NewNoteData
    , newLabelName : String
    , labels : List Label
    , user : User
    , offlineQueue : OfflineQueue
    }


type alias NewNoteData =
    { title : String
    , content : String
    , labels :
        Maybe
            { labels : List ID
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
    = ChangeNotePinned ( ID, Bool )
    | NewTitleChange String
    | NewNotePlainTextContentChange String
    | ReceivedRandomValues (List Int)
    | DeleteNote ID
    | DeleteLabel ID
    | BeginWritingNewNote
    | CreateNewNote
    | BeginAddingNewNoteLabels
    | SearchLabelsQueryChange String
    | AddLabelToNewNote ID
    | RemoveLabelFromNewNote ID
    | RemoveLabelFromNote { noteID : ID, labelID : ID }
    | ChangeNewLabelName String
    | CreateNewLabel
    | PostNewNoteResp String (Result Http.Error Api.PostNewNoteResponse)
    | NewLabelResp String (Result Http.Error Api.NewLabelResponse)
    | ToggleNotePinResp (Result Http.Error Api.ToggleNotePinnedResp)
    | DeleteNoteResp (Result Http.Error ())
    | DeleteLabelResp (Result Http.Error ())


type Msg
    = LoggedOutView LoggedOutMsg
    | LoggedInView LoggedInMsg
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)



-- INIT


type alias Flags =
    { seeds : List Int, hasSessionCookie : Bool }


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
      , offlineQueue = []
      , user =
            if flags.hasSessionCookie then
                LoggedIn

            else
                LoggedOut
                    { username = ""
                    , password = ""
                    }
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
                                |> addToQueue (QPinNote ( uid, newPinnedVal ))

                        ToggleNotePinResp res ->
                            case res of
                                Ok _ ->
                                    -- NOTE: we don't need to change anything internal
                                    -- as it was already changed offline when first toggled pin
                                    model |> pure |> handleNextInQueue

                                Err err ->
                                    -- TODO: handle 403
                                    model |> pure

                        RemoveLabelFromNote { noteID, labelID } ->
                            -- TODO: online sync
                            { model
                                | notes =
                                    List.map
                                        (\n ->
                                            if sameId n.id noteID then
                                                { n | labels = n.labels |> List.filter (idDiff labelID) }

                                            else
                                                n
                                        )
                                        model.notes
                            }
                                |> pure

                        DeleteNote toDeleteNoteID ->
                            -- TODO: offline sync
                            { model
                                | notes =
                                    List.filter (\n -> idDiff n.id toDeleteNoteID) model.notes
                            }
                                |> pure
                                |> addToQueue (QDeleteNote toDeleteNoteID)

                        DeleteNoteResp res ->
                            case res of
                                Ok _ ->
                                    -- NOTE: already deleted the note when queued
                                    -- no need to delete it here
                                    model
                                        |> pure
                                        |> handleNextInQueue

                                Err err ->
                                    -- TODO: handle errors / 403
                                    model |> pure

                        ChangeNewLabelName newName ->
                            { model | newLabelName = newName }
                                |> pure

                        CreateNewLabel ->
                            if String.length model.newLabelName == 0 then
                                model |> pure

                            else if List.any (\l -> l.name == model.newLabelName) model.labels then
                                -- TODO: make this visual to the user
                                model |> pure

                            else
                                let
                                    newLabelOfflineId =
                                        generateUID model.seeds |> Tuple.first

                                    newLabel =
                                        { offlineID = newLabelOfflineId
                                        , name = model.newLabelName
                                        }
                                in
                                ( { model
                                    | newLabelName = ""
                                    , labels = { id = OfflineID newLabel.offlineID, name = newLabel.name } :: model.labels
                                  }
                                , requestRandomValues ()
                                )
                                    |> addToQueue (QNewLabel { name = model.newLabelName, offlineId = newLabelOfflineId })

                        NewLabelResp originalLabelOfflineId res ->
                            case res of
                                Ok resLabel ->
                                    ( { model
                                        | labels =
                                            model.labels
                                                |> List.map
                                                    (\e ->
                                                        if e.name == resLabel.name then
                                                            { id = DatabaseID resLabel.id, name = resLabel.name }

                                                        else
                                                            e
                                                    )
                                        , notes =
                                            model.notes
                                                |> List.map
                                                    (\l ->
                                                        { l
                                                            | labels =
                                                                l.labels
                                                                    |> List.map
                                                                        (\e ->
                                                                            if e == OfflineID originalLabelOfflineId then
                                                                                DatabaseID resLabel.id

                                                                            else
                                                                                e
                                                                        )
                                                        }
                                                    )
                                        , offlineQueue =
                                            model.offlineQueue
                                                |> List.map
                                                    (\l ->
                                                        case l of
                                                            QNewNote { offlineId, offlineLabelIds, data } ->
                                                                let
                                                                    containsOldOfflineLabelThatIJustCreated =
                                                                        List.any (\e -> e == originalLabelOfflineId) offlineLabelIds
                                                                in
                                                                if containsOldOfflineLabelThatIJustCreated then
                                                                    QNewNote
                                                                        { offlineId = offlineId
                                                                        , offlineLabelIds = offlineLabelIds |> List.filter (\e -> e /= originalLabelOfflineId)
                                                                        , data = { data | labels = resLabel.id :: data.labels }
                                                                        }

                                                                else
                                                                    l

                                                            QDeleteLabel id ->
                                                                if sameId id (OfflineID originalLabelOfflineId) then
                                                                    QDeleteLabel (DatabaseID resLabel.id)

                                                                else
                                                                    l

                                                            QNewLabel _ ->
                                                                l

                                                            QDeleteNote _ ->
                                                                l

                                                            QPinNote _ ->
                                                                l
                                                    )
                                      }
                                    , Cmd.none
                                    )
                                        |> handleNextInQueue

                                Err err ->
                                    -- TODO: handle 403
                                    model |> pure

                        ReceivedRandomValues values ->
                            { model | seeds = List.map Random.initialSeed values }
                                |> pure

                        DeleteLabel labelId ->
                            { model | labels = model.labels |> List.filter (\l -> idDiff l.id labelId) }
                                |> pure
                                |> addToQueue (QDeleteLabel labelId)

                        DeleteLabelResp res ->
                            case res of
                                Ok _ ->
                                    -- NOTE: already deleted the note when queued
                                    -- no need to delete it here
                                    model
                                        |> pure
                                        |> handleNextInQueue

                                Err _ ->
                                    -- TODO: handle 403 & 404
                                    model |> pure

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

                        NewNotePlainTextContentChange s ->
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
                            -- TODO: offline sync
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
                                                                    { labels = labels |> List.filter (idDiff labelID)
                                                                    , labelsSearchQuery = labelsSearchQuery
                                                                    }
                                                                )
                                                }
                                            )
                            }
                                |> pure

                        CreateNewNote ->
                            case model.isWritingANewNote of
                                Nothing ->
                                    model |> pure

                                Just newNoteData ->
                                    -- TODO: offline sync
                                    let
                                        newNoteOfflineId =
                                            generateUID model.seeds |> Tuple.first

                                        newNote =
                                            { id = OfflineID newNoteOfflineId
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
                                    if String.length newNoteData.content == 0 then
                                        model |> pure

                                    else
                                        ( { model
                                            | isWritingANewNote = Nothing
                                            , notes = newNote :: model.notes
                                          }
                                        , requestRandomValues ()
                                        )
                                            |> (let
                                                    ( offlineLabelIDs, dbLabelIDs ) =
                                                        labelIDsSplitter newNote.labels [] []
                                                in
                                                addToQueue
                                                    (QNewNote
                                                        { offlineId = newNoteOfflineId
                                                        , offlineLabelIds = offlineLabelIDs
                                                        , data =
                                                            { title =
                                                                if String.length newNoteData.title == 0 then
                                                                    Nothing

                                                                else
                                                                    Just newNoteData.title
                                                            , content = newNoteData.content
                                                            , pinned = Nothing
                                                            , labels =
                                                                -- NOTE: currently cannot create note WITH new labels in it
                                                                -- so labels are created separately beforehand
                                                                dbLabelIDs
                                                            }
                                                        }
                                                    )
                                               )

                        PostNewNoteResp originalOfflineID res ->
                            case res of
                                Ok newNote ->
                                    { model
                                        | notes =
                                            model.notes
                                                |> List.map
                                                    (\n ->
                                                        if sameId n.id (OfflineID originalOfflineID) then
                                                            { n | id = DatabaseID newNote.id }

                                                        else
                                                            n
                                                    )
                                        , offlineQueue =
                                            model.offlineQueue
                                                |> List.map
                                                    (\l ->
                                                        case l of
                                                            QDeleteNote id ->
                                                                if sameId id (OfflineID originalOfflineID) then
                                                                    QDeleteNote (DatabaseID newNote.id)

                                                                else
                                                                    l

                                                            QPinNote ( id, newPinStatus ) ->
                                                                if sameId id (OfflineID originalOfflineID) then
                                                                    QPinNote ( DatabaseID newNote.id, newPinStatus )

                                                                else
                                                                    l

                                                            QNewLabel _ ->
                                                                l

                                                            QDeleteLabel _ ->
                                                                l

                                                            QNewNote _ ->
                                                                l
                                                    )
                                    }
                                        |> pure
                                        |> handleNextInQueue

                                Err _ ->
                                    -- TODO: handle 403
                                    model |> pure

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

                -- TODO: Change later
                LoggedOutView _ ->
                    ( model, Cmd.none )

                FullSyncResp res ->
                    case res of
                        -- TODO:
                        Ok ( notes, labels ) ->
                            { model
                                | labels = List.map (\e -> { name = e.name, id = DatabaseID e.id }) labels
                                , notes =
                                    List.map
                                        (\l ->
                                            { id = DatabaseID l.id
                                            , title =
                                                case l.title of
                                                    Just val ->
                                                        val

                                                    Nothing ->
                                                        ""
                                            , content = l.content
                                            , pinned = l.pinned
                                            , labels = List.map DatabaseID l.labels
                                            }
                                        )
                                        notes
                            }
                                |> pure

                        Err v ->
                            -- TODO: handle 403
                            model |> pure



-- QUEUE


addToQueue : OfflineQueueAction -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToQueue action ( model, cmds ) =
    -- TODO: handle down-syncs
    -- have DB respond with db changes since last
    -- date updated and
    case model.offlineQueue of
        [] ->
            -- no queued actions?
            -- add and run the first one then
            ( { model | offlineQueue = [ action ] }
            , Cmd.batch [ cmds, runAction action ]
            )

        (currentActionInProgress :: queuedActions) as queue ->
            ( { model
                | offlineQueue =
                    case action of
                        QNewLabel _ ->
                            queue ++ [ action ]

                        QNewNote _ ->
                            queue ++ [ action ]

                        QDeleteLabel toDeleteLabelID ->
                            case toDeleteLabelID of
                                DatabaseID _ ->
                                    queue ++ [ action ]

                                OfflineID _ ->
                                    -- check if label is in queue and hasn't been created yet
                                    -- if so, remove it from queue and don't send delete request
                                    let
                                        labelHasNotBeenCreatedYet =
                                            queuedActions
                                                |> List.filter
                                                    (\l ->
                                                        case l of
                                                            QNewLabel { offlineId } ->
                                                                sameId (OfflineID offlineId) toDeleteLabelID

                                                            _ ->
                                                                False
                                                    )
                                                |> List.isEmpty
                                                |> not
                                    in
                                    if labelHasNotBeenCreatedYet then
                                        -- don't add to the queue, no need to add it
                                        -- just cancel every other action to create or modify
                                        -- the offline-only label
                                        currentActionInProgress
                                            :: (queuedActions
                                                    |> List.filter
                                                        (\ac ->
                                                            case ac of
                                                                QNewLabel { offlineId } ->
                                                                    idDiff (OfflineID offlineId) toDeleteLabelID

                                                                QNewNote _ ->
                                                                    True

                                                                QPinNote _ ->
                                                                    True

                                                                QDeleteNote _ ->
                                                                    True

                                                                QDeleteLabel _ ->
                                                                    True
                                                        )
                                                -- no need to List.map to update QNewNote to remove offline label if it
                                                -- may have been there, note will never be created with offline label either way
                                               )

                                    else
                                        queue ++ [ action ]

                        QDeleteNote toDeleteNoteID ->
                            case toDeleteNoteID of
                                DatabaseID _ ->
                                    queue ++ [ action ]

                                OfflineID _ ->
                                    -- check if note is in queue and hasn't been created yet
                                    -- if so, remove it from queue and don't send delete request
                                    let
                                        noteHasNotBeenCreatedYet =
                                            queuedActions
                                                |> List.filter
                                                    (\l ->
                                                        case l of
                                                            QNewNote { offlineId } ->
                                                                sameId (OfflineID offlineId) toDeleteNoteID

                                                            _ ->
                                                                False
                                                    )
                                                |> List.isEmpty
                                                |> not
                                    in
                                    if noteHasNotBeenCreatedYet then
                                        -- don't add to the queue, no need to add it
                                        -- just cancel every other action to create or modify
                                        -- the offline only note
                                        currentActionInProgress
                                            :: (queuedActions
                                                    |> List.filter
                                                        (\ac ->
                                                            case ac of
                                                                QNewNote { offlineId } ->
                                                                    idDiff (OfflineID offlineId) toDeleteNoteID

                                                                QPinNote ( pinNoteId, _ ) ->
                                                                    idDiff pinNoteId toDeleteNoteID

                                                                QNewLabel _ ->
                                                                    True

                                                                QDeleteNote _ ->
                                                                    True

                                                                QDeleteLabel _ ->
                                                                    True
                                                        )
                                               )

                                    else
                                        queue ++ [ action ]

                        QPinNote ( id, _ ) ->
                            -- only keeping the latest pin toggle request
                            (queue
                                |> List.filter
                                    (\act ->
                                        case act of
                                            QPinNote ( reqNoteId, _ ) ->
                                                idDiff id reqNoteId

                                            _ ->
                                                True
                                    )
                            )
                                ++ [ action ]
              }
            , cmds
            )


removeLastQueued : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
removeLastQueued ( model, cmd ) =
    case model.offlineQueue of
        [] ->
            ( model, cmd )

        _ :: restQueue ->
            ( { model | offlineQueue = restQueue }, cmd )


runNextInQueue : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runNextInQueue ( model, originalCmds ) =
    case model.offlineQueue of
        [] ->
            ( model, originalCmds )

        action :: restQueue ->
            ( { model | offlineQueue = action :: restQueue }, runAction action )


runAction : OfflineQueueAction -> Cmd Msg
runAction action =
    case action of
        QNewLabel { name, offlineId } ->
            Api.postNewLabel ( name, Nothing ) (NewLabelResp offlineId)
                |> Cmd.map LoggedInView

        QNewNote { offlineId, data } ->
            Api.postNewNote data (PostNewNoteResp offlineId)
                |> Cmd.map LoggedInView

        QPinNote ( id, newPinVal ) ->
            case id of
                DatabaseID dID ->
                    Api.toggleNotePinned dID newPinVal ToggleNotePinResp
                        |> Cmd.map LoggedInView

                OfflineID _ ->
                    -- NOTE: if it has no DB id then it's sitting in the queue
                    -- waiting for a previous createNote request to finish
                    -- ergo it should never reach here
                    -- TODO: test this
                    Cmd.none

        QDeleteLabel id ->
            case id of
                DatabaseID dID ->
                    Api.deleteLabel dID DeleteLabelResp
                        |> Cmd.map LoggedInView

                OfflineID _ ->
                    -- NOTE: it should never reach here.
                    -- if it has an offlineID then it's already filtered out
                    -- in addToQueue fn
                    Cmd.none

        QDeleteNote id ->
            -- NOTE: it could just not send previous requests
            -- since the note will get deleted
            -- and there is no undo on deletion
            case id of
                DatabaseID dID ->
                    Api.deleteNote dID DeleteNoteResp
                        |> Cmd.map LoggedInView

                OfflineID _ ->
                    -- NOTE: it should never reach here.
                    -- if it has an offlineID then it's already filtered out
                    -- in addToQueue fn
                    -- TODO: maybe this should return a different type of action
                    -- TODO: validate that it doesn't reach here in addToQueue
                    -- TODO: to avoid this extra check altogether?
                    Cmd.none


handleNextInQueue : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleNextInQueue =
    removeLastQueued >> runNextInQueue


labelIDsSplitter : List ID -> List String -> List Int -> ( List String, List Int )
labelIDsSplitter ids offlineIds dbIds =
    case ids of
        [] ->
            ( offlineIds, dbIds )

        x :: xs ->
            case x of
                OfflineID offlineId ->
                    labelIDsSplitter xs (offlineId :: offlineIds) dbIds

                DatabaseID dbID ->
                    labelIDsSplitter xs offlineIds (dbID :: dbIds)



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


mainView : Model -> Html LoggedInMsg
mainView model =
    div [ css [ height (pct 100), overflow auto ] ]
        [ nav
            [ css
                [ backgroundColor (rgb 140 20 254)
                , color (rgb 255 255 255)
                , padding2 (px 10) (px 30)
                , publicSans
                , fontWeight bolder
                , borderBottom3 (px 3) solid (rgb 0 0 0)
                , position sticky
                , top (px 0)
                , displayFlex
                , justifyContent spaceBetween
                ]
            ]
            [ p
                [ css
                    [ fontSize (px 25)
                    ]
                ]
                [ text "Notes" ]
            , (case model.offlineQueue of
                [] ->
                    Outlined.cloud

                _ ->
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
            , form [ onSubmit CreateNewLabel ]
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
                            , onSubmit CreateNewNote
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
                                , onInput NewNotePlainTextContentChange
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
                                                , onSubmit CreateNewLabel
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
                                                            |> List.filter (\l -> List.any (\j -> j == l.id) labels |> not)
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
            [ if String.length data.title == 0 then
                div [] []

              else
                div
                    [ css
                        [ publicSans
                        , borderBottom3 (px 1) solid (rgb 0 0 0)
                        , padding (px 10)
                        ]
                    ]
                    [ text data.title ]
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



-- css helpers


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



--- Queue


type alias OfflineQueue =
    List OfflineQueueAction


type OfflineQueueAction
    = QNewLabel { name : String, offlineId : String }
    | QDeleteNote ID
    | QDeleteLabel ID
    | QPinNote ( ID, Bool )
    | QNewNote { offlineId : String, offlineLabelIds : List String, data : Api.PostNewNoteInput }
