module Route exposing (..)

import Api exposing (SyncableID(..))
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type Route
    = Home
    | LogIn
    | EditLabels
    | EditNote SyncableID


syncableIDParser : Parser (SyncableID -> b) b
syncableIDParser =
    Url.Parser.custom
        "SYNCABLE_ID"
        (\e ->
            case String.toInt e of
                Just v ->
                    Just (DatabaseID v)

                Nothing ->
                    Just (OfflineID e)
        )


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map LogIn (s "log-in")
        , map EditLabels (s "edit-labels")
        , map EditNote
            (s "edit-note" </> syncableIDParser)
        ]


fromUrl : Url -> Route
fromUrl url =
    parse parser url |> Maybe.withDefault LogIn


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            LogIn

        Just url ->
            Maybe.withDefault LogIn (parse parser url)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


routeToString : Route -> String
routeToString route =
    "/"
        ++ (case route of
                Home ->
                    ""

                LogIn ->
                    "log-in"

                EditLabels ->
                    "edit-labels"

                EditNote noteId ->
                    let
                        id =
                            case noteId of
                                DatabaseID v ->
                                    String.fromInt v

                                OfflineID v ->
                                    v
                    in
                    "edit-note" ++ "/" ++ id
           )
