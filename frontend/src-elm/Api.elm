module Api exposing (..)

import Http exposing (header, riskyRequest)
import Json.Encode as JE


baseUrl : String
baseUrl =
    "http://localhost:4000/"


logIn : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
logIn email password msg =
    riskyRequest
        { url = baseUrl ++ "login"
        , headers = []
        , method = "POST"

        -- TODO: timeout?
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "email", JE.string email )
                    , ( "password", JE.string password )
                    ]
                )
        , expect = Http.expectWhatever msg
        }
