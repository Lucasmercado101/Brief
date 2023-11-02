module Api exposing (..)

import Either exposing (Either(..))
import Http exposing (riskyRequest)
import Json.Decode as JD exposing (Decoder, bool, field, int, list, map2, map4, map5, map6, map7, map8, maybe, oneOf, string)
import Json.Encode as JE
import Time exposing (Posix, millisToPosix, posixToMillis)


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


type alias DbID =
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
    { id : DbID
    , title : Maybe String
    , content : String
    , pinned : Bool
    , createdAt : Posix
    , updatedAt : Posix
    , labels : List DbID
    }


type alias Label =
    { id : DbID
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


type SyncableID
    = -- Not synced with DB yet,
      -- generated ID offline
      OfflineID String
      -- Synced with DB,
      -- using DB's ID
    | DatabaseID DbID


type Operation
    = -- delete ids will get changed to DatabaseID once
      -- previous create returns with the DB's ID
      -- TODO: main.elm could have a different operation type
      -- and these would be onlineID only as much as possible instead?
      -- as they would get filtered out beforehand and only real "Operation"
      -- type would be created only JUST before we actually make the request
      -- TODO: also be a record like in Main.elm and just share the same alias?
      DeleteLabels (List SyncableID)
    | DeleteNotes (List SyncableID)
    | CreateLabels (List { offlineId : String, name : String })
    | CreateNotes
        (List
            { id : String
            , title : Maybe String
            , content : String
            , pinned : Bool
            , labels : List SyncableID
            }
        )
    | EditNote
        { id : SyncableID
        , title : Maybe String
        , content : Maybe String
        , pinned : Maybe Bool
        , labels : Maybe (List SyncableID)
        }
    | ChangeLabelName
        { name : String
        , id : SyncableID
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
                , ( "ids", JE.list offlineFirstEncoder ids )
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
                , ( "ids", JE.list offlineFirstEncoder ids )
                ]

        CreateNotes data ->
            JE.object
                [ ( "operation", JE.string "CREATE_NOTES" )
                , ( "notes"
                  , JE.list
                        (\n ->
                            JE.object
                                ([ ( "offlineId", JE.string n.id )
                                 , ( "content", JE.string n.content )
                                 , ( "pinned", JE.bool n.pinned )
                                 , ( "labels", JE.list offlineFirstEncoder n.labels )
                                 ]
                                    ++ (case n.title of
                                            Just t ->
                                                [ ( "title"
                                                  , JE.string t
                                                  )
                                                ]

                                            Nothing ->
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
                 , ( "id", offlineFirstEncoder data.id )
                 ]
                    ++ (case data.title of
                            Just t ->
                                [ ( "title"
                                  , JE.string t
                                  )
                                ]

                            Nothing ->
                                []
                       )
                    ++ (case data.content of
                            Just t ->
                                [ ( "content"
                                  , JE.string t
                                  )
                                ]

                            Nothing ->
                                []
                       )
                    ++ (case data.pinned of
                            Just t ->
                                [ ( "pinned"
                                  , JE.bool t
                                  )
                                ]

                            Nothing ->
                                []
                       )
                    ++ (case data.labels of
                            Just t ->
                                [ ( "labels"
                                  , JE.list offlineFirstEncoder t
                                  )
                                ]

                            Nothing ->
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
        { notes : List DbID
        , labels : List DbID
        }
    }


type alias ChangesResponse =
    { deleted :
        { notes : List DbID
        , labels : List DbID
        }
    , failedToCreate : List OfflineId

    -- , failedToEdit :
    --     { notes : List SyncableID
    --     , labels : List SyncableID
    --     }
    , justSyncedAt : Posix

    -- Brings latest data, only those that were edited
    -- after justSyncedAt is older than their UpdatedAt
    -- are returned
    , downSyncedData :
        { notes : List Note
        , labels : List Label
        }
    , justCreatedData :
        { notes : List ( Note, OfflineId )
        , labels : List ( Label, OfflineId )
        }
    }


changesResponseDecoder : Decoder ChangesResponse
changesResponseDecoder =
    let
        deletedDecoder =
            map2 (\a b -> { notes = a, labels = b })
                (field "notes" (list int))
                (field "labels" (list int))

        offlineFirstDecoder : Decoder SyncableID
        offlineFirstDecoder =
            oneOf
                [ JD.map DatabaseID int
                , JD.map OfflineID string
                ]

        -- TODO: this is incorrect, fix decoder
        -- failedToEditDecoder =
        --     map2 (\a b -> { notes = a, labels = b })
        --         (field "notes" (list offlineFirstDecoder))
        --         (field "labels" (list offlineFirstDecoder))
        downSyncNoteDecoder : Decoder (Either ( Note, OfflineId ) Note)
        downSyncNoteDecoder =
            map8
                (\a b c d e f g h ->
                    case h of
                        Just offlineId ->
                            Left
                                ( { id = a
                                  , title = b
                                  , content = c
                                  , pinned = d
                                  , createdAt = e
                                  , updatedAt = f
                                  , labels = g
                                  }
                                , offlineId
                                )

                        Nothing ->
                            Right
                                { id = a
                                , title = b
                                , content = c
                                , pinned = d
                                , createdAt = e
                                , updatedAt = f
                                , labels = g
                                }
                )
                (field "id" int)
                (field "title" (maybe string))
                (field "content" string)
                (field "pinned" bool)
                (field "createdAt" posixTime)
                (field "updatedAt" posixTime)
                (field "labels" (list int))
                (maybe (field "offlineId" string))

        downSyncLabelDecoder : Decoder (Either ( Label, OfflineId ) Label)
        downSyncLabelDecoder =
            map5
                (\a b c d e ->
                    case e of
                        Just offlineId ->
                            Left
                                ( { id = a
                                  , name = b
                                  , createdAt = c
                                  , updatedAt = d
                                  }
                                , offlineId
                                )

                        Nothing ->
                            Right
                                { id = a
                                , name = b
                                , createdAt = c
                                , updatedAt = d
                                }
                )
                (field "id" int)
                (field "name" string)
                (field "createdAt" posixTime)
                (field "updatedAt" posixTime)
                (maybe (field "offlineId" string))

        downSyncedDataDecoder =
            map2
                (\n l ->
                    { notes = n
                    , labels = l
                    }
                )
                (field "notes" (list downSyncNoteDecoder))
                (field "labels" (list downSyncLabelDecoder))
    in
    map4
        (\a b d e ->
            let
                offlineOrJustCreatedPartitioner : List (Either ( a, OfflineId ) a) -> List ( a, OfflineId ) -> List a -> ( List ( a, OfflineId ), List a )
                offlineOrJustCreatedPartitioner eithers items offlineIds =
                    -- TODO: use List.partition instead?
                    case eithers of
                        [] ->
                            ( items, offlineIds )

                        x :: xs ->
                            case x of
                                Left ( data, offlineId ) ->
                                    offlineOrJustCreatedPartitioner xs (( data, offlineId ) :: items) offlineIds

                                Right data ->
                                    offlineOrJustCreatedPartitioner xs items (data :: offlineIds)

                ( justCreatedNotes, downSyncedNotes ) =
                    offlineOrJustCreatedPartitioner e.notes [] []

                ( justCreatedLabels, downSyncedLabels ) =
                    offlineOrJustCreatedPartitioner e.labels [] []
            in
            { deleted = a
            , failedToCreate = b
            , justSyncedAt = d
            , downSyncedData =
                { notes = downSyncedNotes
                , labels = downSyncedLabels
                }
            , justCreatedData =
                { notes = justCreatedNotes
                , labels = justCreatedLabels
                }
            }
        )
        (field "deleted" deletedDecoder)
        (field "failedToCreate" (list string))
        -- (field "failedToEdit" failedToEditDecoder)
        (field "justSyncedAt" posixTime)
        (field "data" downSyncedDataDecoder)


sendChanges : ChangesInput -> (Result Http.Error ChangesResponse -> msg) -> Cmd msg
sendChanges inputData msg =
    riskyPost "changes"
        (Http.jsonBody
            (JE.object
                [ ( "operations", JE.list operationEncoder inputData.operations )
                , ( "lastSyncedAt", JE.int (posixToMillis inputData.lastSyncedAt) )
                , ( "currentData"
                  , JE.object
                        [ ( "notes", JE.list JE.int inputData.currentData.notes )
                        , ( "labels", JE.list JE.int inputData.currentData.labels )
                        ]
                  )
                ]
            )
        )
        (Http.expectJson msg changesResponseDecoder)


posixTime : Decoder Posix
posixTime =
    int |> JD.map Time.millisToPosix
