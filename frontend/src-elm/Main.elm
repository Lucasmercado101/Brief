port module Main exposing (..)

import Browser
import Cmd.Extra exposing (pure)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
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
        , view = view
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
    div []
        [ div
            []
            (List.map
                (\note ->
                    div
                        [ style "border" "1px solid black"
                        , style "margin" "10px"
                        , style "padding" "10px"
                        , style "display" "flex"
                        , style "flex-direction" "column"
                        ]
                        [ div [] [ text note.title ]
                        , div []
                            [ text
                                (case note.content of
                                    LeftType s ->
                                        s

                                    RightType l ->
                                        String.join "\n" l
                                )
                            ]
                        ]
                )
                model.notes
            )
        , div []
            [ label [] [ text "Is new note a list? " ]
            , Html.input
                [ type_ "checkbox"
                , onClick (NewNoteIsListChange True)
                , value model.newTitle
                ]
                []
            ]
        , div []
            [ label [] [ text "Title: " ]
            , Html.input
                [ onInput NewTitleChange
                , value model.newTitle
                ]
                []
            ]
        , div []
            [ label [] [ text "Content: " ]
            , Html.input
                [ onInput NewContentChange
                , value model.newTitle
                ]
                []
            ]
        , div
            []
            [ Html.button
                [ onClick AddNote ]
                [ text "Add note" ]
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
