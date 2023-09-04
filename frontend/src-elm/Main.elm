module Main exposing (..)

import Browser
import Cmd.Extra exposing (pure)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Random
import UUID exposing (UUID)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type Either a b
    = LeftType a
    | RightType b


type alias Note =
    { id : UUID
    , title : String
    , content : Either String (List String)
    }


type alias Model =
    { notes : List Note
    , newTitle : String
    }



-- MESSAGE


type Msg
    = NoOp
    | NewTitleChange String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes =
            [ { id =
                    Random.step UUID.generator (Random.initialSeed 12345)
                        |> Tuple.first
              , title = "My first note"
              , content = LeftType "This is my first note"
              }
            , { id =
                    Random.step UUID.generator (Random.initialSeed 54321)
                        |> Tuple.first
              , title = "My second note"
              , content = LeftType "Second note here"
              }
            ]
      , newTitle = ""
      }
    , Cmd.none
    )


main : Program () Model Msg
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
            [ label [ Html.Attributes.for "note text" ] []
            , Html.input
                [ onInput NewTitleChange
                , value model.newTitle
                ]
                []
            ]
        ]
