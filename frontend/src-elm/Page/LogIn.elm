module Page.LogIn exposing (..)

import Api
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (auto, column, displayFlex, flexDirection, height, margin, pct, width)
import CssHelpers exposing (publicSans)
import Html.Styled exposing (Html, button, div, form, input, label, text)
import Html.Styled.Attributes exposing (css, placeholder, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Http
import Random
import Route


type alias Model =
    { seeds : List Random.Seed
    , username : String
    , password : String
    , key : Nav.Key
    }


init : Nav.Key -> List Random.Seed -> Model
init key seeds =
    { username = "", password = "", seeds = seeds, key = key }



-- UPDATE


type Msg
    = UsernameChange String
    | PasswordChange String
    | LogIn
    | LogInRes (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameChange username ->
            { model | username = username } |> pure

        PasswordChange password ->
            { model | password = password } |> pure

        LogIn ->
            if model.username == "" || model.password == "" then
                -- TODO: show user error required fields
                model |> pure

            else
                ( model, Api.logIn model.username model.password LogInRes )

        LogInRes res ->
            case res of
                Ok _ ->
                    ( model, Route.replaceUrl model.key Route.Home )

                Err _ ->
                    --         -- TODO: err handling
                    model |> pure



-- VIEW


logInView : Model -> Html Msg
logInView { username, password } =
    -- TODO: no styling, add styling
    div [ css [ width (pct 100), height (pct 100), displayFlex ] ]
        [ form [ css [ margin auto, displayFlex, flexDirection column, publicSans ], onSubmit LogIn ]
            [ label [] [ text "Email: ", input [ placeholder "johnDoe@gmail.com", onInput UsernameChange, value username ] [] ]
            , label [] [ text "Password: ", input [ placeholder "password1234", onInput PasswordChange, value password ] [] ]
            , button [ type_ "submit" ] [ text "submit" ]
            ]
        ]
