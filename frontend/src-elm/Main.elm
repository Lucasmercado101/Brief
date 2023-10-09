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
    | BeginWritingNewNote
    | CreateNewNote
    | BeginAddingNewNoteLabels
    | SearchLabelsQueryChange String
    | AddLabelToNewNote ID
    | RemoveLabelFromNewNote ID
    | RemoveLabelFromNote { noteID : ID, labelID : ID }
    | PostNewNoteResp String (Result Http.Error Api.PostNewNoteResponse)
    | ChangeNewLabelName String
    | CreateNewLabel
    | NewLabelResp (Result Http.Error Api.NewLabelResponse)
    | ToggleNotePinResp (Result Http.Error Api.ToggleNotePinnedResp)
    | DeleteNoteResp (Result Http.Error ())


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
                                |> addToQueue
                                    ( QPinNote ( uid, newPinnedVal )
                                    , case uid of
                                        DatabaseID dID ->
                                            Api.toggleNotePinned dID newPinnedVal ToggleNotePinResp
                                                |> Cmd.map LoggedInView

                                        OfflineID _ ->
                                            -- NOTE: if it has no DB id then it's sitting in the queue
                                            -- waiting for a previous createNote request to finish
                                            Cmd.none
                                    )

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
                                |> (\( m, c ) ->
                                        case toDeleteNoteID of
                                            DatabaseID dID ->
                                                ( m, c )
                                                    |> addToQueue
                                                        ( QDeleteNote toDeleteNoteID
                                                        , Api.deleteNote dID DeleteNoteResp
                                                            |> Cmd.map LoggedInView
                                                        )

                                            OfflineID _ ->
                                                let
                                                    addDeleteNoteToQueue =
                                                        ( m, c ) |> addToQueue ( QDeleteNote toDeleteNoteID, Cmd.none )
                                                in
                                                -- check if note is in queue and hasn't been created yet
                                                -- if so, remove it from queue and don't send delete request
                                                case m.offlineQueue of
                                                    ( action, _ ) :: _ ->
                                                        let
                                                            removeAllNoteRelatedActions =
                                                                ( { m
                                                                    | offlineQueue =
                                                                        model.offlineQueue
                                                                            |> List.filter
                                                                                (\( ac, _ ) ->
                                                                                    case ac of
                                                                                        QNewNote newNoteID ->
                                                                                            idDiff newNoteID toDeleteNoteID

                                                                                        QPinNote ( pinNoteId, _ ) ->
                                                                                            idDiff pinNoteId toDeleteNoteID

                                                                                        QNewLabel ->
                                                                                            True

                                                                                        QDeleteNote _ ->
                                                                                            True
                                                                                )
                                                                  }
                                                                , c
                                                                )
                                                        in
                                                        case action of
                                                            QNewNote newNoteOfflineID ->
                                                                if sameId newNoteOfflineID toDeleteNoteID then
                                                                    -- this note was created and modified offline
                                                                    -- it never synced with db, so we remove all requests
                                                                    -- so that it doesn't get created or anything in the DB
                                                                    removeAllNoteRelatedActions

                                                                else
                                                                    addDeleteNoteToQueue

                                                            _ ->
                                                                addDeleteNoteToQueue

                                                    _ ->
                                                        addDeleteNoteToQueue
                                   )

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
                                    newLabel =
                                        { offlineID = generateUID model.seeds |> Tuple.first
                                        , name = model.newLabelName
                                        }
                                in
                                ( { model
                                    | newLabelName = ""
                                    , labels = { id = OfflineID newLabel.offlineID, name = newLabel.name } :: model.labels
                                  }
                                , requestRandomValues ()
                                )
                                    |> addToQueue
                                        ( QNewLabel
                                        , Api.postNewLabel ( model.newLabelName, Nothing ) NewLabelResp
                                            |> Cmd.map LoggedInView
                                        )

                        NewLabelResp res ->
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
                                            |> addToQueue
                                                ( QNewNote (OfflineID newNoteOfflineId)
                                                , Api.postNewNote
                                                    { title =
                                                        if String.length newNoteData.title == 0 then
                                                            Nothing

                                                        else
                                                            Just newNoteData.title
                                                    , content = newNoteData.content
                                                    , pinned = Nothing
                                                    , labels =
                                                        -- NOTE: assuming there's nothing else in the queue
                                                        -- then all labels should be non-offline only labels
                                                        -- NOTE: currently cannot create note WITH new labels in it
                                                        -- so labels are created separately beforehand
                                                        case newNoteData.labels of
                                                            Just { labels } ->
                                                                labels
                                                                    |> List.filter
                                                                        (\e ->
                                                                            case e of
                                                                                DatabaseID _ ->
                                                                                    True

                                                                                _ ->
                                                                                    False
                                                                        )
                                                                    |> List.map
                                                                        (\e ->
                                                                            case e of
                                                                                DatabaseID id ->
                                                                                    id

                                                                                _ ->
                                                                                    1
                                                                        )

                                                            Nothing ->
                                                                []
                                                    }
                                                    (PostNewNoteResp newNoteOfflineId)
                                                    |> Cmd.map LoggedInView
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


addToQueue : ( OfflineQueueAction, Cmd Msg ) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToQueue ( action, req ) ( model, cmds ) =
    case model.offlineQueue of
        [] ->
            ( { model | offlineQueue = [ ( action, req ) ] }
            , Cmd.batch [ cmds, req ]
            )

        queue ->
            case action of
                -- NOTE: optimizations
                QNewLabel ->
                    ( { model | offlineQueue = queue ++ [ ( action, req ) ] }, cmds )

                QNewNote _ ->
                    ( { model | offlineQueue = queue ++ [ ( action, req ) ] }, cmds )

                QDeleteNote _ ->
                    -- TODO: optimizations
                    ( { model | offlineQueue = queue ++ [ ( action, req ) ] }, cmds )

                QPinNote ( id, _ ) ->
                    -- NOTE: only keeping the latest pin toggle request
                    ( { model
                        | offlineQueue =
                            (queue
                                |> List.filter
                                    (\( act, _ ) ->
                                        case act of
                                            QPinNote ( reqNoteId, _ ) ->
                                                idDiff id reqNoteId

                                            _ ->
                                                True
                                    )
                            )
                                ++ [ ( action, req ) ]
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
            case action of
                ( QNewLabel, cmd ) ->
                    ( { model | offlineQueue = restQueue }, cmd )

                ( QNewNote _, cmd ) ->
                    ( { model | offlineQueue = restQueue }, cmd )

                ( QPinNote ( id, newPinVal ), _ ) ->
                    case id of
                        OfflineID _ ->
                            -- NOTE: should never be here, offlineId should be
                            -- replaced by database id from a previous queue response
                            ( { model | offlineQueue = restQueue }, Cmd.none )

                        DatabaseID dID ->
                            ( { model | offlineQueue = restQueue }
                            , Api.toggleNotePinned dID newPinVal ToggleNotePinResp
                                |> Cmd.map LoggedInView
                            )

                ( QDeleteNote id, cmd ) ->
                    -- NOTE: it could just not send previous requests
                    -- since the note will get deleted
                    -- and there is no undo on deletion
                    case id of
                        DatabaseID _ ->
                            ( { model | offlineQueue = restQueue }, cmd )

                        OfflineID _ ->
                            -- TODO: Should never reach here: offline id is only
                            -- allowed so that previous create requests can replace
                            -- the offline id in the future and this never gets called
                            ( { model | offlineQueue = restQueue }
                            , Cmd.none
                            )


handleNextInQueue : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleNextInQueue =
    removeLastQueued >> runNextInQueue



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
                , fontSize (px 25)
                , borderBottom3 (px 3) solid (rgb 0 0 0)
                , position sticky
                , top (px 0)
                ]
            ]
            [ text "Notes" ]

        -- TODO: placeholder
        , div [ css [ padding (px 15), color (hex "fff"), publicSans ] ]
            [ text "Labels (PLACEHOLDER):"
            , div [] (List.map (\l -> div [] [ text l.name ]) model.labels)
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
            , backgroundColor (rgb 255 203 127)
            , hover
                [ boxShadow4 (px 6) (px 6) (px 0) (rgb 0 0 0)
                ]
            ]
        ]
        [ div
            [ css
                [ backgroundColor (rgb 117 93 39)
                , color (hex "fff")
                , height (px 36)
                , displayFlex
                , justifyContent spaceBetween
                , borderBottom3 (px 3) solid (rgb 0 0 0)
                ]
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
    -- TODO: might not need cmd msg?
    List ( OfflineQueueAction, Cmd Msg )


type OfflineQueueAction
    = QNewLabel
    | QDeleteNote ID
    | QPinNote ( ID, Bool )
    | QNewNote ID
