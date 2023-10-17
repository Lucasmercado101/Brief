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


riskyDelete : String -> Http.Body -> Http.Expect msg -> Cmd msg
riskyDelete endpoint body expect =
    riskyRequest
        { url = baseUrl ++ endpoint
        , headers = []
        , method = "DELETE"

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


riskyPut : String -> Http.Body -> Http.Expect msg -> Cmd msg
riskyPut endpoint body expect =
    riskyRequest
        { url = baseUrl ++ endpoint
        , headers = []
        , method = "PUT"

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
    , labels : List ID
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
                ([ ( "content", JE.string inputData.content )
                 , ( "labels", JE.list JE.int inputData.labels )
                 ]
                    ++ (case inputData.title of
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



---


type alias EditNoteInput =
    { title : Maybe (Nullable String)
    , content : Maybe String
    , pinned : Maybe Bool
    , labels : Maybe (List ID)
    }


type alias EditNoteResp =
    { id : ID
    , title : Maybe String
    , content : String
    , pinned : Bool
    , createdAt : Posix
    , updatedAt : Posix
    , labels : List Label
    }


type Nullable a
    = Val a
    | Null


editNoteDecoder : Decoder EditNoteResp
editNoteDecoder =
    map7 EditNoteResp
        (field "id" int)
        (field "title" (maybe string))
        (field "content" string)
        (field "pinned" bool)
        (field "createdAt" posixTime)
        (field "updatedAt" posixTime)
        (field "labels" (list labelDecoder))


editNote : Int -> EditNoteInput -> (Result Http.Error EditNoteResp -> msg) -> Cmd msg
editNote noteID inputData msg =
    riskyPut ("note/" ++ String.fromInt noteID)
        (Http.jsonBody
            (JE.object
                ([]
                    ++ (case inputData.title of
                            Just v ->
                                case v of
                                    Val title ->
                                        [ ( "title", JE.string title ) ]

                                    Null ->
                                        [ ( "title", JE.null ) ]

                            Nothing ->
                                []
                       )
                    ++ (case inputData.content of
                            Just content ->
                                [ ( "content", JE.string content ) ]

                            Nothing ->
                                []
                       )
                    ++ (case inputData.pinned of
                            Just pinned ->
                                [ ( "pinned", JE.bool pinned ) ]

                            Nothing ->
                                []
                       )
                    ++ (case inputData.labels of
                            Just labels ->
                                [ ( "labels", JE.list JE.int labels ) ]

                            Nothing ->
                                []
                       )
                )
            )
        )
        (Http.expectJson msg editNoteDecoder)



---


removeLabelFromNote : Int -> List Int -> (Result Http.Error () -> msg) -> Cmd msg
removeLabelFromNote noteID labelsIDs msg =
    riskyPut ("note/" ++ String.fromInt noteID)
        (Http.jsonBody (JE.object [ ( "labels", JE.list JE.int labelsIDs ) ]))
        (Http.expectWhatever msg)



---


deleteNote : Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteNote noteID msg =
    riskyDelete
        ("note/" ++ String.fromInt noteID)
        Http.emptyBody
        (Http.expectWhatever msg)



---


deleteLabel : Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteLabel labelID msg =
    riskyDelete
        ("label/" ++ String.fromInt labelID)
        Http.emptyBody
        (Http.expectWhatever msg)



---


type alias NewLabelResponse =
    Label


postNewLabel : ( String, Maybe ID ) -> (Result Http.Error NewLabelResponse -> msg) -> Cmd msg
postNewLabel ( name, parentNoteID ) msg =
    riskyPost "label"
        (Http.jsonBody
            (JE.object
                (( "name", JE.string name )
                    :: (case parentNoteID of
                            Just noteId ->
                                [ ( "noteID", JE.int noteId ) ]

                            Nothing ->
                                []
                       )
                )
            )
        )
        (Http.expectJson msg labelDecoder)



---


type alias ToggleNotePinnedResp =
    ( Int, Bool )


toggleNotePinnedDecoder : Decoder ToggleNotePinnedResp
toggleNotePinnedDecoder =
    map2 Tuple.pair (field "id" int) (field "pinned" bool)


toggleNotePinned : Int -> Bool -> (Result Http.Error ToggleNotePinnedResp -> msg) -> Cmd msg
toggleNotePinned noteID pinned msg =
    riskyPut ("note/" ++ String.fromInt noteID)
        (Http.jsonBody (JE.object [ ( "pinned", JE.bool pinned ) ]))
        (Http.expectJson msg toggleNotePinnedDecoder)



---


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



---


type OfflineFirstId
    = -- Not synced with DB yet,
      -- generated ID offline
      OfflineID String
      -- Synced with DB,
      -- using DB's ID
    | DatabaseID ID


type Optional a
    = HasIt a
    | Undefined


type Operation
    = DeleteLabels (List Int)
    | CreateLabels (List { offlineId : String, name : String })
    | DeleteNotes (List Int)
    | CreateNotes
        (List
            { offlineId : String
            , title : Optional String
            , content : String
            , pinned : Bool
            , labels : List OfflineFirstId
            }
        )
    | EditNote
        { id : OfflineFirstId
        , title : Optional String
        , content : String
        , pinned : Bool
        , labels : List OfflineFirstId
        }
    | ChangeLabelName
        { name : String
        , id : OfflineFirstId
        }


operationEncoder : Operation -> JE.Value
operationEncoder operation =
    let
        offlineFirstEncoder =
            \l ->
                case l of
                    OfflineID v ->
                        JE.string v

                    DatabaseID v ->
                        JE.int v
    in
    case operation of
        DeleteLabels ids ->
            JE.object
                [ ( "operation", JE.string "DELETE_LABELS" )
                , ( "ids", JE.list JE.int ids )
                ]

        CreateLabels data ->
            JE.object
                [ ( "operation", JE.string "CREATE_LABELS" )
                , ( "labels"
                  , JE.list
                        (\l ->
                            JE.object
                                [ ( "offlineId", JE.string l.offlineId )
                                , ( "name", JE.string l.name )
                                ]
                        )
                        data
                  )
                ]

        DeleteNotes ids ->
            JE.object
                [ ( "operation", JE.string "DELETE_NOTES" )
                , ( "ids", JE.list JE.int ids )
                ]

        CreateNotes data ->
            JE.object
                [ ( "operation", JE.string "CREATE_NOTES" )
                , ( "notes"
                  , JE.list
                        (\n ->
                            JE.object
                                ([ ( "offlineId", JE.string n.offlineId )
                                 , ( "content", JE.string n.content )
                                 , ( "pinned", JE.bool n.pinned )
                                 , ( "labels", JE.list offlineFirstEncoder n.labels )
                                 ]
                                    ++ (case n.title of
                                            HasIt t ->
                                                [ ( "title"
                                                  , JE.string t
                                                  )
                                                ]

                                            Undefined ->
                                                []
                                       )
                                )
                        )
                        data
                  )
                ]

        EditNote data ->
            JE.object
                ([ ( "operation", JE.string "EDIT_NOTE" )
                 , ( "offlineId", offlineFirstEncoder data.id )
                 , ( "content", JE.string data.content )
                 , ( "pinned", JE.bool data.pinned )
                 , ( "labels", JE.list offlineFirstEncoder data.labels )
                 ]
                    ++ (case data.title of
                            HasIt t ->
                                [ ( "title"
                                  , JE.string t
                                  )
                                ]

                            Undefined ->
                                []
                       )
                )

        ChangeLabelName data ->
            JE.object
                [ ( "operation", JE.string "CHANGE_LABEL_NAME" )
                , ( "id", offlineFirstEncoder data.id )
                , ( "name", JE.string data.name )
                ]


type alias OfflineId =
    String


type alias ChangesInput =
    { operations : List Operation
    , lastSyncedAt : Posix
    , currentData :
        { notes : List ID
        , labels : List ID
        }
    }


type alias ChangesResponse =
    { deleted :
        { notes : List ID
        , labels : List ID
        }
    , failedToCreate : List OfflineId
    , failedToEdit :
        { notes : List OfflineFirstId
        , labels : List OfflineFirstId
        }
    , justSyncedAt : Posix
    , downSyncedData :
        { notes : List Note
        }
    , justCreatedData :
        { notes : List ( Note, OfflineId )
        , labels : List ( Label, OfflineId )
        }
    }



--


posixTime : Decoder Posix
posixTime =
    int |> JD.map Time.millisToPosix
