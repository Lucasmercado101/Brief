module Main exposing (..)

import Browser
import Cmd.Extra exposing (pure)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type Either a b
    = LeftType a
    | RightType b


type alias Note =
    { title : String
    , content : Either String (List String)
    }


type alias Model =
    { notes : List Note
    }



-- MESSAGE


type Msg
    = NoOp



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes =
            [ { title = "My first note"
              , content = LeftType "This is my first note"
              }
            , { title = "My second note"
              , content = LeftType "Second note here"
              }
            ]
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



-- VIEW


view : Model -> Html msg
view model =
    div []
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
