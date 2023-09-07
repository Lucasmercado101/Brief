port module Main exposing (..)

import Browser
import Cmd.Extra exposing (pure)
import Css exposing (..)
import Html.Styled exposing (Html, button, div, input, label, text)
import Html.Styled.Attributes exposing (css, id, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Random
import Random.Char
import Random.Extra
import Random.String



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
      , notes =
            [ { id =
                    generateUID seeds
                        |> Tuple.first
              , title = "My first note"
              , content = LeftType "This is my first note"
              }
            ]
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
            ]
        ]
        [ -- TODO: give the tiled effect of google keep
          -- using translate and transitions
          div [] (List.map note model.notes)
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


note : Note -> Html Msg
note data =
    div
        [ css
            [ border3 (px 2) solid (rgb 0 0 0)
            , margin (px 10)
            , padding (px 10)
            , displayFlex
            , flexDirection column
            , backgroundColor (rgb 255 203 127)
            ]
        ]
        [ div [] [ text data.title ]
        , div []
            [ text
                (case data.content of
                    LeftType s ->
                        s

                    RightType l ->
                        String.join "\n" l
                )
            ]
        ]



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
