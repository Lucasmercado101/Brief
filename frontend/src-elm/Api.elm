module Api exposing (..)

import Http exposing (riskyRequest)
import Json.Decode as JD exposing (Decoder, bool, field, int, list, map2, map4, map7, maybe, string)
import Json.Encode as JE
import Time exposing (Posix)


riskyGet : String -> Http.Body -> Http.Expect msg -> Cmd msg
riskyGet endpoint body expect =
    riskyRequest
        { url = baseUrl ++ endpoint
        , headers = []
        , method = "GET"

        -- TODO: timeout?
        , timeout = Nothing
        , tracker = Nothing
        , body = body
        , expect = expect
        }


riskyPost : String -> Http.Body -> Http.Expect msg -> Cmd msg
riskyPost endpoint body expect =
    riskyRequest
        { url = baseUrl ++ endpoint
        , headers = []
        , method = "POST"

        -- TODO: timeout?
        , timeout = Nothing
        , tracker = Nothing
        , body = body
        , expect = expect
        }


type alias ID =
    Int


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


type alias PostNewNoteInput =
    { title : Maybe String
    , content : String
    , pinned : Maybe Bool
    }


type alias PostNewNoteResponse =
    { id : ID
    , title : Maybe String
    , content : String
    , pinned : Bool
    , createdAt : Posix
    , updatedAt : Posix
    , userId : ID
    }


postNewNoteDecoder : Decoder PostNewNoteResponse
postNewNoteDecoder =
    map7 PostNewNoteResponse
        (field "id" int)
        (field "title" (maybe string))
        (field "content" string)
        (field "pinned" bool)
        (field "createdAt" posixTime)
        (field "updatedAt" posixTime)
        (field "userId" int)


postNewNote : PostNewNoteInput -> (Result Http.Error PostNewNoteResponse -> msg) -> Cmd msg
postNewNote inputData msg =
    riskyPost "note"
        (Http.jsonBody
            (JE.object
                (( "content", JE.string inputData.content )
                    :: (case inputData.title of
                            Just title ->
                                [ ( "title", JE.string title ) ]

                            Nothing ->
                                []
                       )
                    ++ (case inputData.pinned of
                            Just pinned ->
                                [ ( "pinned", JE.bool pinned ) ]

                            Nothing ->
                                []
                       )
                )
            )
        )
        (Http.expectJson msg postNewNoteDecoder)



--


type alias FullSyncResponse =
    ( List Note, List Label )


fullSync : (Result Http.Error FullSyncResponse -> msg) -> Cmd msg
fullSync msg =
    riskyGet "full-sync" Http.emptyBody (Http.expectJson msg fullSyncDecoder)


fullSyncDecoder : Decoder FullSyncResponse
fullSyncDecoder =
    map2 (\a b -> ( a, b ))
        (field "notes" (list noteDecoder))
        (field "labels" (list labelDecoder))


type alias Note =
    { id : ID
    , title : Maybe String
    , content : String
    , pinned : Bool
    , createdAt : Posix
    , updatedAt : Posix
    , labels : List ID
    }


type alias Label =
    { id : ID
    , name : String
    , createdAt : Posix
    , updatedAt : Posix
    }


noteDecoder : Decoder Note
noteDecoder =
    map7 Note
        (field "id" int)
        (field "title" (maybe string))
        (field "content" string)
        (field "pinned" bool)
        (field "createdAt" posixTime)
        (field "updatedAt" posixTime)
        (field "labels" (list int))


labelDecoder : Decoder Label
labelDecoder =
    map4 Label
        (field "id" int)
        (field "name" string)
        (field "createdAt" posixTime)
        (field "updatedAt" posixTime)


posixTime : Decoder Posix
posixTime =
    int |> JD.map Time.millisToPosix
