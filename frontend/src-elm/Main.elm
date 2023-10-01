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
    receiveRandomValues ReceivedRandomValues


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


type alias Note =
    { id : ID
    , title : String
    , content : String
    , pinned : Bool
    , labels : List ID
    }


type alias Label =
    { id : ID
    , name : String
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
    , isNewNoteAList : Bool
    , isWritingANewNote : Maybe NewNoteData
    , labels : List Label
    , user : User
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
    | Resulted (Result Http.Error ())


type Msg
    = LoggedOutView LoggedOutMsg
    | FullSyncResp (Result Http.Error Api.FullSyncResponse)
    | NewTitleChange String
    | NewNotePlainTextContentChange String
    | NewNoteIsListChange Bool
    | AddNote
    | ReceivedRandomValues (List Int)
    | TogglePinNote ID
    | DeleteNote ID
    | BeginWritingNewNote
    | FinishWritingNewNote
    | BeginAddingNewNoteLabels
    | SearchLabelsQueryChange String
    | AddLabelToNewNote ID
    | RemoveLabelFromNewNote ID
    | RemoveLabelFromNote { noteID : ID, labelID : ID }



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
      , isNewNoteAList = False
      , isWritingANewNote = Nothing
      , labels = []
      , user =
            if flags.hasSessionCookie then
                LoggedIn

            else
                LoggedOut
                    { username = ""
                    , password = ""
                    }
      }
    , Cmd.none
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
                                ( model, Api.logIn username password Resulted |> Cmd.map (\l -> LoggedOutView l) )

                        Resulted res ->
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
                -- TODO: Change later
                LoggedOutView _ ->
                    ( model, Cmd.none )

                FullSyncResp res ->
                    case res of
                        Ok v ->
                            model |> pure

                        Err v ->
                            model |> pure

                RemoveLabelFromNote { noteID, labelID } ->
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

                NewNoteIsListChange v ->
                    { model
                        | isNewNoteAList = v

                        -- , newContent = Right (String.split "\n" model.newContent)
                    }
                        |> pure

                AddNote ->
                    -- TODO: check if note is empty
                    -- TODO: no iAwaitingRandomValues, instead a compact type
                    ( model
                    , requestRandomValues ()
                    )

                DeleteNote uid ->
                    { model
                        | notes =
                            List.filter (\n -> idDiff n.id uid) model.notes
                    }
                        |> pure

                TogglePinNote uid ->
                    { model
                        | notes =
                            List.map
                                (\n ->
                                    if n.id == uid then
                                        { n | pinned = not n.pinned }

                                    else
                                        n
                                )
                                model.notes
                    }
                        |> pure

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

                FinishWritingNewNote ->
                    case model.isWritingANewNote of
                        Nothing ->
                            model |> pure

                        Just newNoteData ->
                            if String.length newNoteData.content == 0 then
                                model |> pure

                            else
                                ( { model
                                    | isWritingANewNote = Nothing
                                    , notes =
                                        model.notes
                                            ++ [ { id = OfflineID (generateUID model.seeds |> Tuple.first)
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
                                               ]
                                  }
                                , requestRandomValues ()
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
                                        , onSubmit FinishWritingNewNote
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


note : Model -> Note -> Html Msg
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
                , onClick (TogglePinNote data.id)
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
