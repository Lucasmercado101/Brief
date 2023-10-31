port module Ports exposing (..)


port updateLastSyncedAt : Int -> Cmd msg


port receiveRandomValues : (List Int -> msg) -> Sub msg


port requestRandomValues : () -> Cmd msg
