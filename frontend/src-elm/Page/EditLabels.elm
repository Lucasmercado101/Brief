module Page.EditLabels exposing (..)

import Api exposing (SyncableID(..))
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (alignItems, auto, backgroundColor, bold, border, border3, borderBottom3, borderLeft3, borderRight3, borderTop3, center, column, cursor, display, displayFlex, ellipsis, flexStart, fontSize, fontWeight, height, hidden, hover, inherit, inline, inlineBlock, int, justifyContent, marginBottom, marginLeft, marginRight, marginTop, maxWidth, minWidth, noWrap, overflow, overflowY, padding, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, pointer, px, solid, spaceBetween, start, textAlign, textDecoration, textOverflow, transparent, underline, whiteSpace, width)
import CssHelpers exposing (black, col, delaGothicOne, error, padX, padY, publicSans, row, secondary, textColor, white)
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
import OfflineQueue exposing (Action(..), OfflineQueueOps, emptyOfflineQueue, offlineQueueIsEmpty, qDeleteLabel, qDeleteLabels, qEditLabelName, qNewLabel, queueToOperations)
import Ports exposing (receiveRandomValues, requestRandomValues, updateLastSyncedAt)
import Random
import Route
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)


pureWithSignal : Signal -> Model -> ( Model, Cmd msg, Maybe Signal )
pureWithSignal s m =
    ( m, Cmd.none, Just s )


pureNoSignal : Model -> ( Model, Cmd msg, Maybe Signal )
pureNoSignal m =
    ( m, Cmd.none, Nothing )



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
    { selected : List EditLabelKind
    , searchQuery : String
    , confirmDeleteAllSelectedLabels : Bool

    -- global data
    , key : Nav.Key
    , seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    }


init :
    { seeds : List Random.Seed
    , labels : List Label
    , notes : List Note
    , key : Nav.Key
    }
    -> Model
init { labels, seeds, notes, key } =
    { key = key
    , seeds = seeds
    , selected = []
    , searchQuery = ""
    , confirmDeleteAllSelectedLabels = False
    , labels = labels
    , notes = notes
    }


type Msg
    = GoHome
    | ChangeSearchQuery String
    | RequestTimeForCreateNewLabel
    | CreateNewLabel { id : String, name : String } Posix
    | SelectLabel SyncableID
    | ClearLabelsSelections
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
    | ReceivedRandomValues (List Int)


type Signal
    = OfflineQueueAction OfflineQueue.Action


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Signal )
update msg model =
    case msg of
        ReceivedRandomValues values ->
            { model | seeds = List.map Random.initialSeed values }
                |> pureNoSignal

        GoHome ->
            -- TODO:
            ( model, Nav.pushUrl model.key (Route.routeToString Route.Home), Nothing )

        ChangeSearchQuery newQuery ->
            { model | searchQuery = newQuery }
                |> pureNoSignal

        RequestTimeForCreateNewLabel ->
            if String.length model.searchQuery == 0 then
                model |> pureNoSignal

            else if List.any (\l -> l.name == model.searchQuery) model.labels then
                -- TODO: make this visual to the user in the form of an error
                model |> pureNoSignal

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
                , Nothing
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
                |> pureWithSignal (OfflineQueueAction (QNewLabel { offlineId = data.id, name = data.name }))

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
                |> pureNoSignal

        ClearLabelsSelections ->
            { model | selected = [] }
                |> pureNoSignal

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
                |> pureNoSignal

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
                |> pureWithSignal (OfflineQueueAction (QDeleteLabel id))

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
                |> pureNoSignal

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
                |> pureNoSignal

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
                |> pureNoSignal

        ConfirmEditingLabelName ( id, newName ) ->
            if newName == "" then
                model |> pureNoSignal

            else if List.any (\l -> (l.name |> String.toLower) == (newName |> String.toLower)) model.labels then
                -- TODO: show message to the user "This label already exists"
                -- and disable create button
                model |> pureNoSignal

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
                    |> pureWithSignal (OfflineQueueAction (QEditLabelName { name = newName, id = id }))

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
                |> pureNoSignal

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
                |> pureNoSignal

        RequestConfirmDeleteMultipleLabels ->
            { model | confirmDeleteAllSelectedLabels = True }
                |> pureNoSignal

        CancelDeleteMultipleLabels ->
            { model | confirmDeleteAllSelectedLabels = False }
                |> pureNoSignal

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
                |> pureWithSignal (OfflineQueueAction (QDeleteLabels deletedIds))


view : Model -> Html Msg
view ({ selected, searchQuery, confirmDeleteAllSelectedLabels } as model) =
    let
        header =
            row
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , alignItems flexStart
                    ]
                ]
                [ col
                    [ css
                        [ paddingLeft (px 25)
                        , paddingTop (px 12)
                        , paddingBottom (px 18)
                        ]
                    ]
                    [ p [ css [ delaGothicOne, fontSize (px 45) ] ] [ text "Labels" ]
                    , p [ css [ publicSans, fontSize (px 22) ] ] [ text ((List.length model.labels |> String.fromInt) ++ " Total") ]
                    ]
                , button
                    [ css
                        [ padding (px 12)
                        , backgroundColor white
                        , border (px 0)
                        , cursor pointer
                        , borderLeft3 (px 5) solid black
                        , height (pct 100)
                        ]
                    , onClick GoHome
                    ]
                    [ Filled.home 38
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
                    , onInput ChangeSearchQuery
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
                , onClick RequestTimeForCreateNewLabel
                ]
                [ text "CREATE NEW LABEL" ]

        labelCard { name, id } isFirst =
            col
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
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
            col
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
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
            col
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
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
                        , onClick ClearLabelsSelections
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
    row [ css [ height (pct 100) ] ]
        [ div [ css [ displayFlex, padY (px 45), height (pct 100) ] ]
            [ col
                [ css
                    [ backgroundColor secondary
                    , border3 (px 5) solid black
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
                col [ css [ alignItems center, justifyContent center, height (pct 80) ] ]
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
