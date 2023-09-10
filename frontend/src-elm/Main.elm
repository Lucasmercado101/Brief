port module Main exposing (..)

import Browser
import Cmd.Extra exposing (pure)
import Css exposing (..)
import Html.Styled exposing (Html, br, button, div, input, label, nav, p, text)
import Html.Styled.Attributes exposing (css, id, placeholder, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
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
      , content = LeftType "Agenda:\n- Introductions\n- Project goals and objectives\n- Team roles and responsibilities\n- Timeline and milestones"
      , pinned = False
      }
    , { id = "2"
      , title = "Travel Packing List"
      , content = LeftType "Clothing:\n- T-shirts\n- Jeans\n- Sweater\n\nToiletries:\n- Toothbrush\n- Shampoo\n- Razor\n\nElectronics:\n- Laptop\n- Charger\n- Headphones"
      , pinned = False
      }
    , { id = "3"
      , title = "Reading List"
      , content = LeftType "Books to Read:\n1. 'The Great Gatsby' by F. Scott Fitzgerald\n2. 'The Hobbit' by J.R.R. Tolkien\n3. 'The Alchemist' by Paulo Coelho"
      , pinned = False
      }
    , { id = "4"
      , title = "Fitness Goals"
      , content = LeftType "Weekly Workout Plan:\n- Monday: Cardio (30 minutes)\n- Wednesday: Strength training\n- Friday: Yoga (45 minutes)"
      , pinned = False
      }
    , { id = "5"
      , title = "Recipe - Chicken Stir-Fry"
      , content = LeftType "Ingredients:\n- Chicken breast\n- Bell peppers\n- Broccoli\n- Soy sauce\n- Rice\n\nInstructions:..."
      , pinned = False
      }
    , { id = "6"
      , title = "Tech Conference Notes"
      , content = LeftType "Keynote Speaker: John Smith\n- Discussed latest trends in AI\n- Highlighted the importance of data privacy"
      , pinned = False
      }
    , { id = "7"
      , title = "Meeting Notes"
      , content = LeftType "Discussed project milestones and assigned tasks to team members."
      , pinned = False
      }
    , { id = "8"
      , title = "Grocery List"
      , content = LeftType "Milk, eggs, bread, and fruits."
      , pinned = False
      }
    , { id = "9"
      , title = "Book Recommendations"
      , content = LeftType "1. 'The Catcher in the Rye' by J.D. Salinger\n2. 'To Kill a Mockingbird' by Harper Lee\n3. '1984' by George Orwell"
      , pinned = False
      }
    , { id = "0"
      , title = "Trip Itinerary"
      , content = LeftType "Day 1: Explore the city\nDay 2: Visit museums and art galleries\nDay 3: Hike in the mountains\nDay 4: Relax at the beach"
      , pinned = False
      }
    , { id = "012"
      , title = "Recipe - Spaghetti Bolognese"
      , content = LeftType "Ingredients:\n- Ground beef\n- Onion\n- Garlic\n- Tomatoes\n- Pasta\n\nInstructions:..."
      , pinned = False
      }
    ]



-- PORTS


port requestRandomValues : () -> Cmd msg


port receiveRandomValues : (List Int -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveRandomValues ReceivedRandomValues



-- MODEL


type alias UID =
    String


type Either a b
    = LeftType a
    | RightType b


type alias Note =
    { id : UID
    , title : String
    , content : Either String (List String)
    , pinned : Bool
    }


type alias Model =
    { seeds : List Random.Seed
    , notes : List Note
    , newTitle : String
    , newContent : Either String (List String)
    , isNewNoteAList : Bool
    , isAwaitingRandomValues : Bool
    }



-- MESSAGE


type Msg
    = NoOp
    | NewTitleChange String
    | NewContentChange String
    | NewNoteIsListChange Bool
    | AddNote
    | ReceivedRandomValues (List Int)
    | TogglePinNote UID



-- INIT


type alias Flags =
    { seeds : List Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seeds =
            List.map Random.initialSeed flags.seeds
    in
    ( { seeds = seeds
      , notes = dummyNotes
      , newTitle = ""
      , newContent = LeftType ""
      , isNewNoteAList = False
      , isAwaitingRandomValues = False
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
    case msg of
        NoOp ->
            model |> pure

        NewTitleChange s ->
            { model | newTitle = s }
                |> pure

        NewContentChange s ->
            { model | newContent = LeftType s }
                |> pure

        NewNoteIsListChange v ->
            { model
                | isNewNoteAList = v

                -- , newContent = RightType (String.split "\n" model.newContent)
            }
                |> pure

        AddNote ->
            -- TODO: check if note is empty
            -- TODO: no iAwaitingRandomValues, instead a compact type
            ( { model
                | isAwaitingRandomValues = True
              }
            , requestRandomValues ()
            )

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
                |> (\m ->
                        if model.isAwaitingRandomValues then
                            { m
                                | isAwaitingRandomValues = False
                                , newTitle = ""
                                , newContent = LeftType ""
                                , isNewNoteAList = False
                                , notes =
                                    { id =
                                        generateUID m.seeds
                                            |> Tuple.first
                                    , title = m.newTitle
                                    , content = m.newContent
                                    , pinned = False
                                    }
                                        :: m.notes
                            }

                        else
                            m
                   )
                |> pure



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "full-container"
        , css
            [ backgroundColor (rgb 24 129 106)
            , width (pct 100)
            , height (pct 100)

            -- temp
            -- , padding (px 25)
            ]
        ]
        [ div [ css [ height (pct 100), overflow auto ] ]
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
            , div [ css [ width (pct 100), displayFlex, marginTop (px 30) ] ]
                [ div [ css [ margin2 (px 0) auto, width (px 500) ] ]
                    [ input
                        [ css
                            [ border3 (px 2) solid (rgb 0 0 0)
                            , publicSans
                            , fontWeight bold
                            , padding (px 8)
                            , margin2 (px 0) auto
                            , width (pct 100)
                            ]
                        , placeholder "TAKE A NEW NOTE"
                        ]
                        []
                    ]
                ]
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
            , div []
                [ label [] [ text "Is new note a list? " ]
                , input
                    [ type_ "checkbox"
                    , onClick (NewNoteIsListChange True)
                    , value model.newTitle
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Title: " ]
                , input
                    [ onInput NewTitleChange
                    , value model.newTitle
                    ]
                    []
                ]
            , div []
                [ label [] [ text "Content: " ]
                , input
                    [ onInput NewContentChange
                    , value model.newTitle
                    ]
                    []
                ]
            , div
                []
                [ button
                    [ onClick AddNote ]
                    [ text "Add note" ]
                ]
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
                ]
                [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
            ]
        , div [ css [ padding (px 10) ] ]
            [ div [ css [ publicSans ] ] [ text data.title ]
            , br [] []
            , p [ css [ publicSans ] ]
                (let
                    -- \n don't break into a newline without this
                    makeParagraph : List (Html msg) -> List String -> List (Html msg)
                    makeParagraph total next =
                        case next of
                            [] ->
                                total

                            x :: xs ->
                                makeParagraph (total ++ [ br [] [], text x ]) xs
                 in
                 case data.content of
                    LeftType s ->
                        s
                            |> String.split "\n"
                            |> (\r ->
                                    case r of
                                        [] ->
                                            [ text s ]

                                        h :: t ->
                                            if List.length t > 0 then
                                                makeParagraph [ text h ] t

                                            else
                                                [ text h ]
                               )

                    RightType l ->
                        [ text (String.join "\n" l) ]
                )
            ]
        ]


publicSans : Style
publicSans =
    fontFamilies [ "Public Sans", .value sansSerif ]


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
