module Main exposing (..)

import Browser
import Cmd.Extra exposing (pure)
import Html exposing (Html, div, text)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    {}



-- MESSAGE


type Msg
    = NoOp



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


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


view : model -> Html msg
view model =
    div [] [ text "Hello, World!" ]
