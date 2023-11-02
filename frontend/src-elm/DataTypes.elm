module DataTypes exposing (..)

import Api exposing (SyncableID(..))
import Time exposing (Posix)


type alias UniqueStr =
    String


type alias Note =
    { id : SyncableID
    , title : Maybe String
    , content : String
    , pinned : Bool
    , createdAt : Posix
    , updatedAt : Posix
    , labels : List SyncableID
    , order : Int
    }


type alias Label =
    { id : SyncableID
    , name : UniqueStr
    , createdAt : Posix
    , updatedAt : Posix
    }
