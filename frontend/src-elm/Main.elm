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


dummyNotes : List Note
dummyNotes =
    [ { id = "1"
      , title = "Project Kickoff Meeting"
      , content = "Agenda:\n- Introductions\n- Project goals and objectives\n- Team roles and responsibilities\n- Timeline and milestones"
      , pinned = False
      , labels = []
      }
    , { id = "011"
      , title = ""
      , content = "Don't forget to read this"
      , pinned = False
      , labels = []
      }
    , { id = "2"
      , title = "Travel Packing List"
      , content = "Clothing:\n- T-shirts\n- Jeans\n- Sweater\n\nToiletries:\n- Toothbrush\n- Shampoo\n- Razor\n\nElectronics:\n- Laptop\n- Charger\n- Headphones"
      , pinned = False
      , labels =
            [ "Fitness"
            , "Study"
            , "Family"
            , "Friends"
            , "Groceries"
            ]
      }
    , { id = "3"
      , title = "Reading List"
      , content = "Books to Read:\n1. 'The Great Gatsby' by F. Scott Fitzgerald\n2. 'The Hobbit' by J.R.R. Tolkien\n3. 'The Alchemist' by Paulo Coelho"
      , pinned = False
      , labels = []
      }
    , { id = "4"
      , title = ""
      , content = "Weekly Workout Plan:\n- Monday: Cardio (30 minutes)\n- Wednesday: Strength training\n- Friday: Yoga (45 minutes)"
      , pinned = False
      , labels = [ "Home" ]
      }
    , { id = "5"
      , title = "Recipe - Chicken Stir-Fry"
      , content = "Ingredients:\n- Chicken breast\n- Bell peppers\n- Broccoli\n- Soy sauce\n- Rice\n\nInstructions:..."
      , pinned = False
      , labels = []
      }
    , { id = "6"
      , title = "Tech Conference Notes"
      , content = "Keynote Speaker: John Smith\n- Discussed latest trends in AI\n- Highlighted the importance of data privacy"
      , pinned = False
      , labels = [ "Home", "Goals" ]
      }
    , { id = "7"
      , title = "Meeting Notes"
      , content = "Discussed project milestones and assigned tasks to team members."
      , pinned = False
      , labels = []
      }
    , { id = "8"
      , title = "Grocery List"
      , content = "Milk, eggs, bread, and fruits."
      , pinned = False
      , labels = []
      }
    , { id = "9"
      , title = ""
      , content = "1. 'The Catcher in the Rye' by J.D. Salinger\n2. 'To Kill a Mockingbird' by Harper Lee\n3. '1984' by George Orwell"
      , pinned = False
      , labels = []
      }
    , { id = "0"
      , title = "Trip Itinerary"
      , content = "Day 1: Explore the city\nDay 2: Visit museums and art galleries\nDay 3: Hike in the mountains\nDay 4: Relax at the beach"
      , pinned = False
      , labels =
            [ "Calls"
            , "Books"
            ]
      }
    , { id = "012"
      , title = "Recipe - Spaghetti Bolognese"
      , content = "Ingredients:\n- Ground beef\n- Onion\n- Garlic\n- Tomatoes\n- Pasta\n\nInstructions:..."
      , pinned = False
      , labels =
            [ "Urgent"
            , "Important"
            , "Fitness"
            ]
      }
    ]


dummyLabels : List String
dummyLabels =
    [ "Work"
    , "Personal"
    , "Shopping"
    , "Health"
    , "Home"
    , "Meeting"
    , "Urgent"
    , "Important"
    , "Fitness"
    , "Study"
    , "Family"
    , "Friends"
    , "Groceries"
    , "Movies"
    , "Travel"
    , "Bills"
    , "Chores"
    , "Goals"
    , "Hobbies"
    , "Calls"
    , "Books"
    , "Music"
    , "Gifts"
    , "Tech"
    ]



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


type alias UID =
    String


type alias Note =
    { id : UID
    , title : String
    , content : String
    , pinned : Bool
    , labels : List String
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
    , labels : List String
    , user : User
    }


type alias NewNoteData =
    { title : String
    , content : String
    , labels :
        Maybe
            { labels : List String
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
    | TogglePinNote UID
    | DeleteNote UID
    | BeginWritingNewNote
    | FinishWritingNewNote
    | BeginAddingNewNoteLabels
    | SearchLabelsQueryChange String
    | AddLabelToNewNote String
    | RemoveLabelFromNewNote String
    | RemoveNoteLabel ( UID, String )



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
      , notes = dummyNotes
      , isNewNoteAList = False
      , isWritingANewNote = Nothing
      , labels = dummyLabels
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

                RemoveNoteLabel ( id, label ) ->
                    { model
                        | notes =
                            List.map
                                (\n ->
                                    if n.id == id then
                                        { n | labels = List.filter (\l -> l /= label) n.labels }

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
                            List.filter (\n -> n.id /= uid) model.notes
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

                RemoveLabelFromNewNote label ->
                    { model
                        | isWritingANewNote =
                            Maybe.map
                                (\data ->
                                    { data
                                        | labels =
                                            Maybe.map
                                                (\{ labelsSearchQuery, labels } ->
                                                    { labels = labels |> List.filter (\l -> l /= label)
                                                    , labelsSearchQuery = labelsSearchQuery
                                                    }
                                                )
                                                data.labels
                                    }
                                )
                                model.isWritingANewNote
                    }
                        |> pure

                FinishWritingNewNote ->
                    case model.isWritingANewNote of
                        Nothing ->
                            model |> pure

                        Just newNoteData ->
                            if
                                -- TODO: when note text can be checkbox list
                                -- case newNoteData.content of
                                --     Left s ->
                                --         String.length s == 0
                                --     Right l ->
                                --         List.all (\s -> String.length s == 0) l
                                String.length newNoteData.content == 0
                            then
                                model |> pure

                            else
                                ( { model
                                    | isWritingANewNote = Nothing
                                    , notes =
                                        model.notes
                                            ++ [ { id = generateUID model.seeds |> Tuple.first
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
                                                                    , onClick (AddLabelToNewNote l)
                                                                    , type_ "button"
                                                                    ]
                                                                    [ text l ]
                                                            )
                                                            (model.labels
                                                                |> List.filter (\l -> List.any (\j -> j == l) labels |> not)
                                                                |> List.filter (String.toLower >> String.contains labelsSearchQuery)
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
                                                                    , onClick (RemoveLabelFromNewNote l)
                                                                    , type_ "button"
                                                                    ]
                                                                    [ text l ]
                                                            )
                                                            labels
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
                        (List.map note (model.notes |> prioritizePinned))
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


note : Note -> Html Msg
note data =
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
                 -- TODO: if it's checkbox note
                 --  case data.content of
                 --     Left s ->
                 --         s
                 --             |> String.split "\n"
                 --             |> (\r ->
                 --                     case r of
                 --                         [] ->
                 --                             [ text s ]
                 --                         h :: t ->
                 --                             if List.length t > 0 then
                 --                                 makeParagraph [ text h ] t
                 --                             else
                 --                                 [ text h ]
                 --                )
                 -- Right l ->
                 --     [ text (String.join "\n" l) ]
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
                                    [ text l
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
                                        , onClick (RemoveNoteLabel ( data.id, l ))
                                        ]
                                        [ text "X" ]
                                    ]
                            )
                            labels
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


generateUID : List Random.Seed -> ( UID, List Random.Seed )
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
