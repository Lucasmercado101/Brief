module Helpers exposing (..)

import Api exposing (SyncableID(..))
import Task
import Time exposing (Posix)


idDiff : SyncableID -> SyncableID -> Bool
idDiff a b =
    case ( a, b ) of
        ( OfflineID c, OfflineID d ) ->
            c /= d

        ( DatabaseID c, DatabaseID d ) ->
            c /= d

        _ ->
            True


sameId : SyncableID -> SyncableID -> Bool
sameId a b =
    not (idDiff a b)


labelIDsSplitterHelper : List SyncableID -> List String -> List Int -> ( List String, List Int )
labelIDsSplitterHelper ids offlineIds dbIds =
    case ids of
        [] ->
            ( offlineIds, dbIds )

        x :: xs ->
            case x of
                OfflineID offlineId ->
                    labelIDsSplitterHelper xs (offlineId :: offlineIds) dbIds

                DatabaseID dbID ->
                    labelIDsSplitterHelper xs offlineIds (dbID :: dbIds)


labelIDsSplitter : List SyncableID -> ( List String, List Int )
labelIDsSplitter ids =
    labelIDsSplitterHelper ids [] []


partitionFirstHelper : List a -> (a -> Bool) -> ( Maybe a, List a ) -> ( Maybe a, List a )
partitionFirstHelper arr pred ( first, checked ) =
    case arr of
        [] ->
            ( Nothing, checked )

        x :: xs ->
            if pred x then
                ( Just x, checked ++ xs )

            else
                partitionFirstHelper xs pred ( Nothing, checked ++ [ x ] )



{-
   Like List.partition but only returns the first element that matches the predicate
   and the rest of the elements with that first element missing

   or Nothing and the original array if it's not there
-}


partitionFirst : (a -> Bool) -> List a -> ( Maybe a, List a )
partitionFirst pred arr =
    partitionFirstHelper arr pred ( Nothing, [] )



{-
   Returns the first value that is present, like the boolean ||.
-}


or : Maybe a -> Maybe a -> Maybe a
or default new =
    case new of
        Just v ->
            Just v

        Nothing ->
            default



{-
   It's just `filterNot` https://package.elm-lang.org/packages/elm-community/list-extra/8.7.0/List-Extra#filterNot
-}


exclude : (a -> Bool) -> List a -> List a
exclude pred list =
    List.filter (not << pred) list



{-
   Get the first item in the list whose pred returns True
-}


listFirst : (a -> Bool) -> List a -> Maybe a
listFirst pred list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just x

            else
                listFirst pred xs


maybeToBool : Maybe a -> Bool
maybeToBool maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


getCurrentTime : (Posix -> msg) -> Cmd msg
getCurrentTime msg =
    Time.now
        |> Task.perform msg


elIsIn : List a -> (c -> a -> Bool) -> c -> Bool
elIsIn list pred el =
    List.any (pred el) list
