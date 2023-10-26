module Page.EditLabels exposing (..)

import Api exposing (SyncableID(..))
import Cmd.Extra exposing (pure)
import Css exposing (..)
import CssHelpers exposing (black, delaGothicOne, error, padX, padY, publicSans, secondary, textColor, white)
import DataTypes exposing (Label, Note)
import Dog exposing (dogSvg)
import Helpers exposing (exclude, labelIDsSplitter, listFirst, partitionFirst, sameId)
import Html.Styled exposing (Html, br, button, div, input, label, li, p, strong, text, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, title, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (OfflineQueueOps, addToQueue, emptyOfflineQueue, offlineQueueIsEmpty, qDeleteLabel, qDeleteLabels, qEditLabelName, qNewLabel, queueToOperations)
import Ports exposing (receiveRandomValues, requestRandomValues, updateLastSyncedAt)
import Random
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)



-- Subs


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveRandomValues ReceivedRandomValues


getNewTimeAndCreateLabel : { id : String, name : String } -> Cmd Msg
getNewTimeAndCreateLabel data =
    Time.now
        |> Task.perform (CreateNewLabel data)



--


type EditLabelKind
    = Selected SyncableID
    | ConfirmDelete SyncableID
    | Editing SyncableID String


type alias Model =
    { seeds : List Random.Seed
    , selected : List EditLabelKind
    , searchQuery : String
    , confirmDeleteAllSelectedLabels : Bool

    -- global data
    , labels : List Label
    , notes : List Note

    -- sync stuff
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    , lastSyncedAt : Posix
    }


init :
    { seeds : List Random.Seed
    , labels : List Label
    , notes : List Note
    , offlineQueue : OfflineQueueOps
    , runningQueueOn : Maybe OfflineQueueOps
    , lastSyncedAt : Posix
    }
    -> Model
init { labels, seeds, notes, offlineQueue, runningQueueOn, lastSyncedAt } =
    { seeds = seeds
    , selected = []
    , searchQuery = ""
    , confirmDeleteAllSelectedLabels = False
    , labels = labels
    , notes = notes
    , offlineQueue = offlineQueue
    , runningQueueOn = runningQueueOn
    , lastSyncedAt = lastSyncedAt
    }


type Msg
    = ExitEditingLabelsView
    | ChangeEditLabelsSearchQuery String
    | CreateNewLabelEditLabelsView
    | CreateNewLabel { id : String, name : String } Posix
    | SelectLabel SyncableID
    | ClearEditLabelsSelections
    | RequestDeleteLabel SyncableID
    | ConfirmDeleteLabel SyncableID
    | CancelDeleteLabel SyncableID
    | EditLabel ( SyncableID, String )
    | ChangeEditingLabelName ( SyncableID, String )
    | ConfirmEditingLabelName ( SyncableID, String )
    | CancelEditingLabelName SyncableID
    | RemoveLabelFromSelected SyncableID
    | RequestConfirmDeleteMultipleLabels
    | ConfirmDeleteMultipleLabels
    | CancelDeleteMultipleLabels
    | ReceivedChangesResp (Result Http.Error Api.ChangesResponse)
    | ReceivedRandomValues (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedRandomValues values ->
            ( { model | seeds = List.map Random.initialSeed values }, Cmd.none )

        ExitEditingLabelsView ->
            -- TODO:
            model
                -- { model | editLabelsScreen = Nothing }
                |> pure

        ChangeEditLabelsSearchQuery newQuery ->
            { model | searchQuery = newQuery }
                |> pure

        CreateNewLabelEditLabelsView ->
            if String.length model.searchQuery == 0 then
                model |> pure

            else if List.any (\l -> l.name == model.searchQuery) model.labels then
                -- TODO: make this visual to the user in the form of an error
                model |> pure

            else
                let
                    newLabelOfflineId : String
                    newLabelOfflineId =
                        generateUID model.seeds |> Tuple.first

                    newLabel =
                        { id = newLabelOfflineId
                        , name = model.searchQuery
                        }
                in
                ( { model | searchQuery = "" }
                , Cmd.batch [ requestRandomValues (), getNewTimeAndCreateLabel newLabel ]
                )

        CreateNewLabel data time ->
            { model
                | labels =
                    { id = OfflineID data.id
                    , name = data.name
                    , updatedAt = time
                    , createdAt = time
                    }
                        :: model.labels
            }
                |> pure
                |> addToQueue (qNewLabel { offlineId = data.id, name = data.name }) ReceivedChangesResp

        SelectLabel id ->
            let
                sameIdOnSelected =
                    \e ->
                        case e of
                            Selected i ->
                                sameId id i

                            ConfirmDelete i ->
                                sameId id i

                            Editing i _ ->
                                sameId id i
            in
            { model
                | selected =
                    if List.any sameIdOnSelected model.selected then
                        model.selected |> exclude sameIdOnSelected

                    else
                        Selected id :: model.selected
            }
                |> pure

        ClearEditLabelsSelections ->
            { model | selected = [] }
                |> pure

        RequestDeleteLabel id ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        if sameId i id then
                                            ConfirmDelete i

                                        else
                                            e

                                    ConfirmDelete _ ->
                                        e

                                    Editing _ _ ->
                                        e
                            )
            }
                |> pure

        ConfirmDeleteLabel id ->
            { model
                | selected =
                    model.selected
                        |> List.filter
                            (\e ->
                                case e of
                                    Selected i ->
                                        True

                                    ConfirmDelete i ->
                                        if sameId i id then
                                            False

                                        else
                                            True

                                    Editing _ _ ->
                                        True
                            )
            }
                |> pure
                |> addToQueue (qDeleteLabel id) ReceivedChangesResp

        CancelDeleteLabel id ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        e

                                    ConfirmDelete i ->
                                        if sameId i id then
                                            Selected i

                                        else
                                            e

                                    Editing _ _ ->
                                        e
                            )
            }
                |> pure

        EditLabel ( id, newName ) ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        if sameId i id then
                                            Editing id newName

                                        else
                                            e

                                    ConfirmDelete i ->
                                        e

                                    Editing _ _ ->
                                        e
                            )
            }
                |> pure

        ChangeEditingLabelName ( id, newName ) ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        e

                                    ConfirmDelete i ->
                                        e

                                    Editing i _ ->
                                        if sameId i id then
                                            Editing id newName

                                        else
                                            e
                            )
            }
                |> pure

        ConfirmEditingLabelName ( id, newName ) ->
            if newName == "" then
                model |> pure

            else if List.any (\l -> (l.name |> String.toLower) == (newName |> String.toLower)) model.labels then
                -- TODO: show message to the user "This label already exists"
                -- and disable create button
                model |> pure

            else
                { model
                    | labels =
                        model.labels
                            |> List.map
                                (\l ->
                                    if sameId l.id id then
                                        { l | name = newName }

                                    else
                                        l
                                )
                    , selected =
                        model.selected
                            |> List.map
                                (\e ->
                                    case e of
                                        Selected i ->
                                            e

                                        ConfirmDelete i ->
                                            e

                                        Editing i _ ->
                                            if sameId i id then
                                                Selected i

                                            else
                                                e
                                )
                }
                    |> pure
                    |> addToQueue (qEditLabelName { name = newName, id = id }) ReceivedChangesResp

        CancelEditingLabelName id ->
            { model
                | selected =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        e

                                    ConfirmDelete i ->
                                        e

                                    Editing i _ ->
                                        if sameId i id then
                                            Selected i

                                        else
                                            e
                            )
            }
                |> pure

        RemoveLabelFromSelected id ->
            { model
                | selected =
                    model.selected
                        |> exclude
                            (\e ->
                                case e of
                                    Selected i ->
                                        sameId id i

                                    ConfirmDelete i ->
                                        sameId id i

                                    Editing i _ ->
                                        sameId id i
                            )
            }
                |> pure

        RequestConfirmDeleteMultipleLabels ->
            { model | confirmDeleteAllSelectedLabels = True }
                |> pure

        CancelDeleteMultipleLabels ->
            { model | confirmDeleteAllSelectedLabels = False }
                |> pure

        ConfirmDeleteMultipleLabels ->
            let
                deletedIds : List SyncableID
                deletedIds =
                    model.selected
                        |> List.map
                            (\e ->
                                case e of
                                    Selected i ->
                                        i

                                    ConfirmDelete i ->
                                        i

                                    Editing i _ ->
                                        i
                            )
            in
            { model
                | labels =
                    model.labels
                        |> exclude (\l -> List.any (sameId l.id) deletedIds)
                , selected = []
                , confirmDeleteAllSelectedLabels = False
            }
                |> pure
                |> addToQueue (qDeleteLabels deletedIds) ReceivedChangesResp

        ReceivedChangesResp resp ->
            case resp of
                Ok { deleted, failedToCreate, failedToEdit, justSyncedAt, downSyncedData, justCreatedData } ->
                    ( { model
                        | notes =
                            let
                                ( _, notOutdatedNotes ) =
                                    List.partition
                                        (\e -> List.any (\l -> sameId (DatabaseID l.id) e.id) downSyncedData.notes)
                                        model.notes

                                updatedNotes : List Note
                                updatedNotes =
                                    downSyncedData.notes
                                        |> List.map
                                            (\e ->
                                                { id = DatabaseID e.id
                                                , title = e.title
                                                , content = e.content
                                                , pinned = e.pinned
                                                , createdAt = e.createdAt
                                                , updatedAt = e.updatedAt
                                                , labels = e.labels |> List.map DatabaseID
                                                }
                                            )
                            in
                            notOutdatedNotes
                                -- remove the ones that were failed to create
                                |> exclude (\l -> List.any (\e -> sameId l.id (OfflineID e)) failedToCreate)
                                -- remove the ones that don't exist in DB
                                |> exclude (\l -> List.any (\e -> sameId l.id (DatabaseID e)) deleted.notes)
                                -- update just created
                                |> List.map
                                    (\l ->
                                        case listFirst (\( _, offlineId ) -> sameId l.id (OfflineID offlineId)) justCreatedData.notes of
                                            Just ( v, _ ) ->
                                                { id = DatabaseID v.id
                                                , title = v.title
                                                , content = v.content
                                                , pinned = v.pinned
                                                , createdAt = v.createdAt
                                                , updatedAt = v.updatedAt
                                                , labels = v.labels |> List.map DatabaseID
                                                }

                                            Nothing ->
                                                l
                                    )
                                |> (++) updatedNotes
                        , labels =
                            let
                                ( _, notOutdatedLabels ) =
                                    List.partition
                                        (\e -> List.any (\l -> sameId (DatabaseID l.id) e.id) downSyncedData.labels)
                                        model.labels

                                updatedLabels : List Label
                                updatedLabels =
                                    downSyncedData.labels
                                        |> List.map
                                            (\e ->
                                                { id = DatabaseID e.id
                                                , name = e.name
                                                , createdAt = e.createdAt
                                                , updatedAt = e.updatedAt
                                                }
                                            )
                            in
                            notOutdatedLabels
                                -- remove the ones that were failed to create
                                |> exclude (\l -> List.any (\e -> sameId l.id (OfflineID e)) failedToCreate)
                                -- remove the ones that don't exist in DB
                                |> exclude (\l -> List.any (\e -> sameId l.id (DatabaseID e)) deleted.labels)
                                -- update just created
                                |> List.map
                                    (\l ->
                                        case listFirst (\( _, offlineId ) -> sameId l.id (OfflineID offlineId)) justCreatedData.labels of
                                            Just ( v, _ ) ->
                                                { id = DatabaseID v.id
                                                , name = v.name
                                                , createdAt = v.createdAt
                                                , updatedAt = v.updatedAt
                                                }

                                            Nothing ->
                                                l
                                    )
                                |> (++) updatedLabels
                        , offlineQueue = emptyOfflineQueue
                        , runningQueueOn =
                            if offlineQueueIsEmpty model.offlineQueue then
                                Nothing

                            else
                                Just model.offlineQueue
                        , lastSyncedAt = justSyncedAt
                      }
                    , Cmd.batch
                        [ updateLastSyncedAt (Time.posixToMillis justSyncedAt)
                        , if offlineQueueIsEmpty model.offlineQueue then
                            Cmd.none

                          else
                            Api.sendChanges
                                { operations = queueToOperations model.offlineQueue
                                , lastSyncedAt = justSyncedAt
                                , currentData =
                                    { notes = model.notes |> List.map .id |> labelIDsSplitter |> Tuple.second
                                    , labels = model.labels |> List.map .id |> labelIDsSplitter |> Tuple.second
                                    }
                                }
                                ReceivedChangesResp
                        ]
                    )

                -- TODO: error handling here
                Err _ ->
                    model |> pure


view : Model -> Html Msg
view ({ selected, searchQuery, confirmDeleteAllSelectedLabels } as model) =
    let
        header =
            div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , alignItems flexStart
                    , marginBottom (px 18)
                    ]
                ]
                [ div
                    [ css
                        [ paddingLeft (px 25)
                        , displayFlex
                        , flexDirection column
                        , paddingTop (px 12)
                        ]
                    ]
                    [ p [ css [ delaGothicOne, fontSize (px 45) ] ] [ text "Labels" ]
                    , p [ css [ publicSans, fontSize (px 22) ] ] [ text ((List.length model.labels |> String.fromInt) ++ " Total") ]
                    ]
                , button
                    [ css
                        [ padding (px 12)
                        , backgroundColor transparent
                        , border (px 0)
                        , cursor pointer
                        ]
                    , onClick ExitEditingLabelsView
                    ]
                    [ Filled.close 32
                        Inherit
                        |> Svg.Styled.fromUnstyled
                    ]
                ]

        searchBar =
            div
                [ css
                    [ displayFlex
                    , backgroundColor white
                    , alignItems center
                    , borderBottom3 (px 3) solid black
                    , borderTop3 (px 3) solid black
                    ]
                ]
                [ label [ for "labels-edit-searchBar", css [ displayFlex, justifyContent center, alignItems center, paddingLeft (px 12), paddingRight (px 6) ] ] [ Outlined.search 18 Inherit |> Svg.Styled.fromUnstyled ]
                , input
                    [ id "labels-edit-searchBar"
                    , css
                        [ border (px 0)
                        , fontWeight (int 600)
                        , width (pct 100)
                        , height (pct 100)
                        , padY (px 7)
                        , publicSans
                        , fontSize (px 16)
                        ]
                    , placeholder "Search labels..."
                    , value searchQuery
                    , onInput ChangeEditLabelsSearchQuery
                    ]
                    []
                ]

        itemsList =
            ul [ css [ overflow auto, height (pct 100) ] ]
                (List.map
                    (\label ->
                        li
                            []
                            [ button
                                [ css
                                    ([ paddingLeft (px 12)
                                     , publicSans
                                     , padY (px 5)
                                     , width (pct 100)
                                     , textAlign start
                                     , backgroundColor transparent
                                     , border (px 0)
                                     , cursor pointer
                                     , fontSize (px 16)
                                     , fontWeight (int 600)
                                     ]
                                        ++ (if
                                                List.any
                                                    (\e ->
                                                        case e of
                                                            Selected i ->
                                                                sameId label.id i

                                                            ConfirmDelete i ->
                                                                sameId label.id i

                                                            Editing i _ ->
                                                                sameId label.id i
                                                    )
                                                    selected
                                            then
                                                [ textColor white, backgroundColor black ]

                                            else
                                                [ hover [ textColor white, backgroundColor black ] ]
                                           )
                                    )
                                , onClick (SelectLabel label.id)
                                ]
                                [ p
                                    [ css
                                        [ whiteSpace noWrap
                                        , textOverflow ellipsis
                                        , overflow hidden
                                        ]
                                    ]
                                    [ text label.name ]
                                ]
                            ]
                    )
                    (model.labels
                        |> List.filter (.name >> String.toLower >> String.contains (String.toLower searchQuery))
                    )
                )

        newLabelBtn =
            button
                [ css [ cursor pointer, fontWeight (int 800), publicSans, fontSize (px 16), backgroundColor white, padY (px 12), border (px 0), borderTop3 (px 3) solid black ]
                , onClick CreateNewLabelEditLabelsView
                ]
                [ text "CREATE NEW LABEL" ]

        labelCard { name, id } isFirst =
            div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 480)
                    , marginTop
                        (px
                            (if isFirst then
                                0

                             else
                                32
                            )
                        )
                    ]
                ]
                [ div [ css [ backgroundColor white, displayFlex, justifyContent spaceBetween, borderBottom3 (px 5) solid black ] ]
                    [ button
                        [ css
                            [ displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor error
                            , textColor white
                            , border (px 0)
                            , borderRight3 (px 5) solid black
                            , cursor pointer
                            , width (px 42)
                            ]
                        , onClick (RequestDeleteLabel id)
                        ]
                        [ Filled.delete 36 Inherit |> Svg.Styled.fromUnstyled ]
                    , div [ css [ displayFlex ] ]
                        [ button
                            [ css
                                [ displayFlex
                                , justifyContent center
                                , alignItems center
                                , textColor black
                                , border (px 0)
                                , backgroundColor transparent
                                , borderLeft3 (px 5) solid black
                                , cursor pointer
                                , hover [ backgroundColor black, textColor white ]
                                ]
                            , onClick (EditLabel ( id, name ))
                            ]
                            [ Filled.edit 42 Inherit |> Svg.Styled.fromUnstyled ]
                        , button
                            [ css
                                [ displayFlex
                                , justifyContent center
                                , alignItems center
                                , textColor black
                                , border (px 0)
                                , backgroundColor transparent
                                , borderLeft3 (px 5) solid black
                                , cursor pointer
                                , hover [ backgroundColor black, textColor white ]
                                ]
                            , onClick (RemoveLabelFromSelected id)
                            ]
                            [ Filled.close 42 Inherit |> Svg.Styled.fromUnstyled ]
                        ]
                    ]
                , div [ css [ padX (px 32), paddingTop (px 16), paddingBottom (px 32) ] ]
                    [ p [ css [ delaGothicOne, fontSize (px 38) ] ] [ text name ]
                    , p [ css [ publicSans, fontSize (px 18), marginBottom (px 26) ] ] [ text "Created on: July 30th at 12:05 PM" ]
                    , p [ css [ publicSans, fontSize (px 22) ] ] [ text "Last Updated on: July 30th at 12:05 PM" ]
                    , div [ css [ marginTop (px 12) ] ]
                        [ p [ css [ publicSans, textDecoration underline, fontSize (px 22), display inline ] ] [ text "50 Notes" ]
                        , p [ css [ publicSans, fontSize (px 22), display inline ] ] [ text " using this label" ]
                        ]
                    ]
                ]

        confirmDeleteCard { name, id } isFirst =
            div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 480)
                    , minWidth (px 480)
                    , marginTop
                        (px
                            (if isFirst then
                                0

                             else
                                32
                            )
                        )
                    ]
                ]
                [ p [ css [ delaGothicOne, fontSize (px 38), marginTop (px 12), marginBottom (px 16), textAlign center ] ] [ text "Really?" ]
                , div [ css [ display inlineBlock, textAlign center ] ]
                    [ p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "Are you sure you want to " ]
                    , p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "delete" ]
                    , p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text " label \"" ]
                    , strong [ css [ fontWeight (int 900), publicSans, fontSize (px 18), display inline ] ] [ text name ]
                    , p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "\"?" ]
                    ]
                , div [ css [ displayFlex, marginTop (px 16), backgroundColor white ] ]
                    [ button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), borderTop3 (px 5) solid black, width (pct 100), padY (px 10), textAlign center ]
                        , onClick (CancelDeleteLabel id)
                        ]
                        [ text "Cancel" ]
                    , button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), width (pct 100), borderLeft3 (px 5) solid black, borderTop3 (px 5) solid black, padY (px 10), textAlign center ]
                        , onClick (ConfirmDeleteLabel id)
                        ]
                        [ text "Confirm" ]
                    ]
                ]

        editLabelCard { name, id } isFirst newName =
            div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 480)
                    , minWidth (px 480)
                    , marginTop
                        (px
                            (if isFirst then
                                0

                             else
                                32
                            )
                        )
                    ]
                ]
                [ p [ css [ delaGothicOne, fontSize (px 38), marginTop (px 12), marginBottom (px 16), textAlign center ] ] [ text name ]
                , label [ css [ publicSans, fontSize (px 18), padX (px 16), displayFlex, alignItems center ] ]
                    [ text "Name:"
                    , input
                        [ css [ padY (px 12), marginLeft (px 12), border3 (px 3) solid black, publicSans, fontSize (px 18), width (pct 100) ]
                        , class "label-edit-input"
                        , placeholder name
                        , value newName
                        , onInput (\e -> ChangeEditingLabelName ( id, e ))
                        ]
                        []
                    ]
                , div [ css [ displayFlex, marginTop (px 16), backgroundColor white ] ]
                    [ button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), borderTop3 (px 5) solid black, width (pct 100), padY (px 10), textAlign center ]
                        , onClick (CancelEditingLabelName id)
                        ]
                        [ text "Cancel" ]
                    , button
                        [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), width (pct 100), borderLeft3 (px 5) solid black, borderTop3 (px 5) solid black, padY (px 10), textAlign center ]
                        , onClick (ConfirmEditingLabelName ( id, newName ))
                        ]
                        [ text "Confirm" ]
                    ]
                ]

        someLabelSelected =
            List.length selected /= 0

        selectedActions amount =
            let
                selectedAmountActions =
                    [ button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderRight3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor black, textColor white ]
                            , cursor pointer
                            ]
                        , title "Delete selected labels"
                        , onClick RequestConfirmDeleteMultipleLabels
                        ]
                        [ Filled.delete 32 Inherit |> Svg.Styled.fromUnstyled ]
                    , div [ css [ width (pct 100), padY (px 16) ] ]
                        [ p [ css [ publicSans, fontSize (px 42), fontWeight bold ] ] [ text amount ]
                        , p [ css [ publicSans, fontSize (px 42) ] ] [ text "Selected" ]
                        ]
                    , button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderLeft3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor error, textColor white ]
                            , cursor pointer
                            ]
                        , title "Clear selected labels"
                        , onClick ClearEditLabelsSelections
                        ]
                        [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
                    ]

                confirmDeleteMultiple =
                    [ button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderRight3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor black, textColor white ]
                            , cursor pointer
                            ]
                        , onClick CancelDeleteMultipleLabels
                        ]
                        [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
                    , div [ css [ width (pct 100), padY (px 16) ] ]
                        [ p [ css [ publicSans, fontSize (px 42) ] ] [ text "Delete" ]
                        , strong [ css [ publicSans, fontSize (px 42), display inline ] ] [ text amount ]
                        , p [ css [ display inline, publicSans, fontSize (px 42) ] ] [ text " labels?" ]
                        ]
                    , button
                        [ css
                            [ width (px 64)
                            , border (px 0)
                            , textColor inherit
                            , borderLeft3 (px 3) solid white
                            , displayFlex
                            , justifyContent center
                            , alignItems center
                            , backgroundColor transparent
                            , hover [ backgroundColor error, textColor white ]
                            , cursor pointer
                            ]
                        , onClick ConfirmDeleteMultipleLabels
                        ]
                        [ Filled.check 32 Inherit |> Svg.Styled.fromUnstyled ]
                    ]
            in
            div [ css [ displayFlex, marginBottom (px 32), textAlign center, textColor white, border3 (px 3) solid white ] ]
                (if confirmDeleteAllSelectedLabels then
                    confirmDeleteMultiple

                 else
                    selectedAmountActions
                )
    in
    div [ css [ displayFlex, flexDirection row, height (pct 100) ] ]
        [ div [ css [ displayFlex, padY (px 45), height (pct 100) ] ]
            [ div
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
                    , displayFlex
                    , flexDirection column
                    , maxWidth (px 345)
                    , minWidth (px 345)
                    , width (px 345)
                    , overflow hidden

                    -- TODO: more consistent spacing between them
                    , marginRight (px 200)
                    , marginLeft (px 200)
                    ]
                ]
                [ header
                , searchBar
                , itemsList
                , if (searchQuery /= "") && (List.filter (.name >> String.toLower >> (==) (String.toLower searchQuery)) model.labels |> List.isEmpty) then
                    newLabelBtn

                  else
                    text ""
                ]
            ]
        , div [ css [ overflowY auto, height (pct 100), padY (px 45), padX (px 32) ] ]
            [ if someLabelSelected then
                selectedActions (selected |> List.length |> String.fromInt)

              else
                div [ css [ displayFlex, alignItems center, flexDirection column, justifyContent center, height (pct 80) ] ]
                    [ dogSvg |> Svg.Styled.fromUnstyled
                    , p [ css [ publicSans, fontSize (px 42), fontWeight (int 300), textAlign center, textColor white ] ]
                        [ text "Select a label to edit"
                        , br [] []
                        , text "and it will appear here"
                        ]
                    ]
            , ul []
                (List.indexedMap
                    (\i ( label, labelKind ) ->
                        case labelKind of
                            Selected _ ->
                                labelCard { name = label.name, id = label.id } (i == 0)

                            ConfirmDelete _ ->
                                confirmDeleteCard { name = label.name, id = label.id } (i == 0)

                            Editing _ newName ->
                                editLabelCard { name = label.name, id = label.id } (i == 0) newName
                    )
                    (let
                        labelKinds : List Label -> List EditLabelKind -> List ( Label, Maybe EditLabelKind ) -> List ( Label, Maybe EditLabelKind )
                        labelKinds labels selection current =
                            case labels of
                                [] ->
                                    current

                                x :: xs ->
                                    let
                                        ( foundKind, restKind ) =
                                            selection
                                                |> partitionFirst
                                                    (\r ->
                                                        case r of
                                                            Selected i ->
                                                                sameId x.id i

                                                            ConfirmDelete i ->
                                                                sameId x.id i

                                                            Editing i _ ->
                                                                sameId x.id i
                                                    )
                                    in
                                    labelKinds xs restKind (( x, foundKind ) :: current)
                     in
                     labelKinds model.labels selected []
                        |> List.filterMap
                            (\( id, kind ) ->
                                case kind of
                                    Nothing ->
                                        Nothing

                                    Just k ->
                                        Just ( id, k )
                            )
                    )
                )
            ]
        ]
