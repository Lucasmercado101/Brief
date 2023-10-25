module Page.LogIn exposing (..)

import Css exposing (auto, column, displayFlex, flexDirection, height, margin, pct, px, width)
import CssHelpers exposing (publicSans)
import Html.Styled exposing (Html, button, div, form, input, label, text)
import Html.Styled.Attributes exposing (css, placeholder, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit)


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = LogIn
    | UsernameChange String
    | PasswordChange String



-- | Login
-- | LoginRes (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, Cmd.none )

        UsernameChange username ->
            ( { model | username = username }, Cmd.none )

        PasswordChange password ->
            ( { model | password = password }, Cmd.none )


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
