module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Home
    | LogIn


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map LogIn (s "log-in")
        ]


fromUrl : Url -> Route
fromUrl url =
    parse route url |> Maybe.withDefault LogIn


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            LogIn

        Just url ->
            Maybe.withDefault LogIn (parse route url)
