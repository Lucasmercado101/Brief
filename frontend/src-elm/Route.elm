module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, top)


type Route
    = Home


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        ]


fromUrl : Url -> Route
fromUrl url =
    parse route url |> Maybe.withDefault Home


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            Home

        Just url ->
            Maybe.withDefault Home (parse route url)
