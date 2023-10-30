module Page.EditNote exposing (..)

import Api exposing (SyncableID)
import Browser.Navigation as Nav
import DataTypes exposing (Label, Note)
import Html.Styled exposing (Html, div)
import Random


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { noteId : SyncableID

    -- global data
    , key : Nav.Key
    , seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    }


init :
    { notes : List Note
    , labels : List Label
    , noteId : SyncableID
    , seeds : List Random.Seed
    , key : Nav.Key
    }
    -> Model
init { notes, labels, noteId, seeds, key } =
    { notes = notes
    , labels = labels
    , noteId = noteId
    , seeds = seeds
    , key = key
    }


type Msg
    = NoOp


view : Model -> Html Msg
view model =
    div [] []
