module Page.Home exposing (..)

import Api exposing (SyncableID(..))
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (alignItems, auto, backgroundColor, bold, bolder, border, border3, borderBottom3, borderLeft3, borderRight3, boxShadow4, center, color, column, cursor, displayFlex, ellipsis, flexDirection, flexStart, flexWrap, fontSize, fontWeight, height, hex, hidden, hover, inherit, int, justifyContent, margin, margin2, marginBottom, marginLeft, marginRight, marginTop, maxWidth, minHeight, minWidth, noWrap, overflow, overflowY, padding, padding2, paddingLeft, paddingRight, paddingTop, pct, pointer, position, property, px, rgb, row, solid, spaceBetween, start, sticky, textAlign, textOverflow, top, transparent, whiteSpace, width, wrap)
import CssHelpers exposing (black, col, delaGothicOne, displayGrid, error, fullWidth, gap, mx, padX, padY, primary, publicSans, secondary, textColor, userSelectNone, white)
import DataTypes exposing (Label, Note)
import Helpers exposing (exclude, idDiff, labelIDsSplitter, listFirst, or, partitionFirst, sameId)
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (Action(..), OQCreateLabel, OQCreateNote, OQDeleteNote, OQEditNote, OfflineQueueOps, emptyOfflineQueue, offlineQueueIsEmpty, qCreateNewNote, qDeleteNote, qEditNoteLabels, qNewLabel, qToggleNotePin, queueToOperations)
import Ports exposing (receiveRandomValues, requestRandomValues, updateLastSyncedAt)
import Random
import Route
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)


pureNoSignal : Model -> ( Model, Cmd msg, Maybe Signal )
pureNoSignal m =
    ( m, Cmd.none, Nothing )


pureWithSignal : Signal -> Model -> ( Model, Cmd msg, Maybe Signal )
pureWithSignal s m =
    ( m, Cmd.none, Just s )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveRandomValues ReceivedRandomValues


type alias LabelsColumnMenu =
    Maybe String


type alias Model =
    { key : Nav.Key
    , seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    , isWritingANewNote : Maybe NewNoteData
    , newLabelName : String
    , labelsMenu : LabelsColumnMenu
    , filters :
        { label : Maybe SyncableID
        , content : Maybe String
        }
    }


init : { navKey : Nav.Key, seeds : List Random.Seed, labels : List Label, notes : List Note } -> Model
init { navKey, seeds, labels, notes } =
    ({ key = navKey
     , seeds = seeds
     , notes = notes
     , isWritingANewNote = Nothing
     , newLabelName = ""
     , labels = labels
     , labelsMenu = Nothing
     , filters =
        { label = Nothing
        , content = Nothing
        }
     }
     -- TODO: full-sync with regards to indexedDb
     -- , Cmd.none
     --     -- TODO:
     -- if hasSessionCookie then
     --     -- Api.fullSync FullSyncResp
     --     --
     --   else
     --     Cmd.none
    )


type alias NewNoteData =
    { title : String
    , content : String
    , labels :
        Maybe
            { labels : List SyncableID
            , labelsSearchQuery : String
            }
    }


type Msg
    = ChangeNotePinned ( SyncableID, Bool )
    | ReceivedRandomValues (List Int)
    | RemoveLabelFromNote { noteID : SyncableID, labelID : SyncableID }
      -- Labels menu
    | SelectLabelToFilterBy SyncableID
    | OpenLabelsMenu
    | CloseLabelsMenu
    | ChangeLabelsSearchQuery String
    | GoToEditLabelsScreen


type Signal
    = OfflineQueueAction OfflineQueue.Action


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Signal )
update msg model =
    case msg of
        -- Labels column menu
        OpenLabelsMenu ->
            { model | labelsMenu = Just "" }
                |> pureNoSignal

        CloseLabelsMenu ->
            { model | labelsMenu = Nothing }
                |> pureNoSignal

        ChangeLabelsSearchQuery s ->
            { model | labelsMenu = Maybe.map (\_ -> s) model.labelsMenu }
                |> pureNoSignal

        SelectLabelToFilterBy id ->
            let
                newFilter : Maybe SyncableID
                newFilter =
                    case model.filters.label of
                        Nothing ->
                            Just id

                        Just oldId ->
                            if sameId oldId id then
                                Nothing

                            else
                                Just id
            in
            { model
                | filters =
                    { label = newFilter
                    , content = model.filters.content
                    }
            }
                |> pureNoSignal

        GoToEditLabelsScreen ->
            ( { model | labelsMenu = Nothing }, Route.replaceUrl model.key Route.EditLabels, Nothing )

        ChangeNotePinned ( uid, newPinnedVal ) ->
            { model
                | notes =
                    List.map
                        (\n ->
                            if n.id == uid then
                                { n | pinned = newPinnedVal }

                            else
                                n
                        )
                        model.notes
            }
                |> pureWithSignal (OfflineQueueAction (QToggleNotePin uid newPinnedVal))

        RemoveLabelFromNote { noteID, labelID } ->
            let
                ( noteExists, restNotes ) =
                    partitionFirst (\n -> sameId n.id noteID) model.notes
            in
            case noteExists of
                Nothing ->
                    model |> pureNoSignal

                Just noteData ->
                    let
                        newNotes =
                            noteData.labels |> exclude (sameId labelID)
                    in
                    { model | notes = { noteData | labels = newNotes } :: restNotes }
                        |> pureWithSignal (OfflineQueueAction (QEditNoteLabels noteID (Just newNotes)))

        ReceivedRandomValues values ->
            { model | seeds = List.map Random.initialSeed values }
                |> pureNoSignal


view : Model -> Bool -> Html Msg
view model isSyncing =
    div [ css [ displayFlex, flexDirection column, height (pct 100), overflow auto ] ]
        [ nav
            [ css
                [ backgroundColor (rgb 140 20 254)
                , color (rgb 255 255 255)
                , publicSans
                , fontWeight bolder
                , borderBottom3 (px 3) solid (rgb 0 0 0)
                , position sticky
                , top (px 0)
                , displayFlex
                , justifyContent spaceBetween
                ]
            ]
            [ let
                labelsCount =
                    model.labels |> List.length |> String.fromInt
              in
              case model.labelsMenu of
                Just _ ->
                    openLabelsMenuBtn labelsCount

                Nothing ->
                    closedLabelsMenuBtn labelsCount
            , p
                [ css
                    [ fontSize (px 25)
                    ]
                ]
                [ text "Notes" ]
            , (if isSyncing then
                Outlined.sync

               else
                Outlined.cloud
              )
                28
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        , div [ css [ displayFlex, height (pct 100), overflow hidden ] ]
            [ case model.labelsMenu of
                Just _ ->
                    labelsMenuColumn model

                Nothing ->
                    text ""
            , notesGrid model
            ]
        ]


labelsMenuWidth : Float
labelsMenuWidth =
    277


closedLabelsMenuBtn : String -> Html Msg
closedLabelsMenuBtn labelsCount =
    button
        [ css
            [ padY (px 8)
            , paddingLeft (px 8)
            , maxWidth (px labelsMenuWidth)
            , minWidth (px labelsMenuWidth)
            , backgroundColor white
            , textColor black
            , border (px 0)
            , borderRight3 (px 3) solid black
            , displayFlex
            , publicSans
            , alignItems center
            , justifyContent spaceBetween
            , cursor pointer
            , fontSize (px 16)
            , fontWeight bold
            ]
        , type_ "button"
        , onClick OpenLabelsMenu
        ]
        [ div
            [ css
                [ displayFlex
                , alignItems center
                ]
            ]
            [ Filled.label 32
                Inherit
                |> Svg.Styled.fromUnstyled
            , p [ css [ marginLeft (px 10) ] ] [ text "Labels" ]
            , p [ css [ marginLeft (px 10) ] ] [ text ("(" ++ labelsCount ++ ")") ]
            ]
        , div [ css [ width (px 50), height (px 32), displayFlex, justifyContent center, alignItems center ] ]
            [ Filled.arrow_drop_down 32
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        ]


openLabelsMenuBtn : String -> Html Msg
openLabelsMenuBtn labelsCount =
    div
        [ css
            [ paddingLeft (px 8)
            , maxWidth (px labelsMenuWidth)
            , minWidth (px labelsMenuWidth)
            , backgroundColor black
            , textColor white
            , border (px 0)
            , borderRight3 (px 3) solid black
            , displayFlex
            , publicSans
            , alignItems center
            , justifyContent spaceBetween
            , fontSize (px 16)
            , fontWeight bold
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , alignItems center
                , padY (px 8)
                ]
            ]
            [ Filled.label 32
                Inherit
                |> Svg.Styled.fromUnstyled
            , p [ css [ marginLeft (px 10) ] ] [ text "Labels" ]
            , p [ css [ marginLeft (px 10) ] ] [ text ("(" ++ labelsCount ++ ")") ]
            ]
        , button
            [ css
                [ width (px 50)
                , height (pct 100)
                , displayFlex
                , justifyContent center
                , alignItems center
                , border (px 0)
                , backgroundColor error
                , cursor pointer
                , textColor white
                ]
            , type_ "button"
            , onClick CloseLabelsMenu
            ]
            [ Filled.close 32
                Inherit
                |> Svg.Styled.fromUnstyled
            ]
        ]


labelsMenuColumn : Model -> Html Msg
labelsMenuColumn { labels, filters, labelsMenu } =
    div
        [ css
            [ maxWidth (px labelsMenuWidth)
            , minWidth (px labelsMenuWidth)
            , backgroundColor secondary
            , height (pct 100)
            , borderRight3 (px 3) solid black
            , displayFlex
            , flexDirection column
            ]
        ]
        [ div
            [ css
                [ height (px 48)
                , displayFlex
                , alignItems center
                , borderBottom3 (px 2) solid black
                , backgroundColor primary
                ]
            ]
            [ label
                [ css [ padX (px 10), height (pct 100), displayFlex, alignItems center ]
                , for "search-labels"
                ]
                [ Outlined.search 24 Inherit
                    |> Svg.Styled.fromUnstyled
                ]
            , input
                [ css
                    [ fontWeight bold
                    , publicSans
                    , height (pct 100)
                    , width (pct 100)
                    , border (px 0)
                    , fontSize (px 16)
                    , backgroundColor transparent
                    ]
                , value (Maybe.withDefault "" labelsMenu)
                , onInput ChangeLabelsSearchQuery
                , id "search-labels"
                , placeholder "Search labels..."
                ]
                []
            , button
                [ css
                    [ width (px 80)
                    , height (pct 100)
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , border (px 0)
                    , backgroundColor white
                    , cursor pointer
                    , textColor black
                    , borderLeft3 (px 3) solid black
                    ]
                , type_ "button"
                , onClick GoToEditLabelsScreen
                ]
                [ Filled.edit 28 Inherit
                    |> Svg.Styled.fromUnstyled
                ]
            ]
        , ul
            [ css
                [ fontWeight (int 600)
                , height (pct 100)
                , overflowY auto
                , displayFlex
                , flexDirection column
                ]
            ]
            (List.indexedMap
                (\i e ->
                    button
                        [ css
                            ([ paddingLeft (px 10)
                             , padY (px 5)
                             , cursor pointer
                             , hover [ textColor white, backgroundColor black ]
                             , publicSans
                             , border (px 0)
                             , fontSize (px 16)
                             , cursor pointer
                             , backgroundColor transparent
                             , fontWeight (int 600)
                             , textColor inherit
                             , userSelectNone
                             ]
                                ++ (case filters.label of
                                        Nothing ->
                                            []

                                        Just v ->
                                            if sameId v e.id then
                                                [ textColor white, backgroundColor black ]

                                            else
                                                []
                                   )
                                ++ (if i == 0 then
                                        [ paddingTop (px 10) ]

                                    else
                                        []
                                   )
                            )
                        , type_ "button"
                        , onClick (SelectLabelToFilterBy e.id)
                        ]
                        [ p
                            [ css
                                [ whiteSpace noWrap
                                , textOverflow ellipsis
                                , overflow hidden
                                , textAlign start
                                ]
                            ]
                            [ text e.name ]
                        ]
                )
                (labels
                    |> List.sortBy (.createdAt >> Time.posixToMillis)
                    |> (\l ->
                            case labelsMenu of
                                Just filterQuery ->
                                    l |> List.filter (\e -> String.contains (filterQuery |> String.toLower) (e.name |> String.toLower))

                                Nothing ->
                                    l
                       )
                )
            )
        ]


notesGrid : Model -> Html Msg
notesGrid model =
    let
        header =
            div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , marginBottom (px 32)
                    , alignItems start
                    ]
                ]
                [ p
                    [ css
                        [ delaGothicOne
                        , fontSize (px 32)
                        , textColor white
                        ]
                    ]
                    [ text "Notes - ", text (model.notes |> List.length |> String.fromInt), text " Total" ]
                , div
                    [ css
                        [ publicSans
                        , fontWeight (int 800)
                        , padY (px 18)
                        , backgroundColor white
                        , border3 (px 5) solid black
                        , cursor pointer
                        , textAlign center
                        , fontSize (px 18)

                        -- 2 notes size + padding
                        , width (px 505)
                        ]
                    ]
                    [ text "CREATE NEW NOTE"
                    ]
                ]
    in
    div [ css [ width (pct 100), overflowY auto ] ]
        [ col
            [ css
                [ paddingTop (px 28)
                , paddingRight (px 32)
                , paddingLeft (px 32)
                ]
            ]
            [ header
            , case model.notes of
                [] ->
                    -- TODO: add no notes empty state design
                    div [] []

                _ ->
                    div
                        [ -- TODO: give the tiled effect of google keep
                          -- using translate and transitions
                          css
                            [ displayFlex
                            , flexDirection row
                            , flexWrap wrap
                            , marginTop (px 30)
                            , alignItems flexStart
                            , gap 10
                            , margin2 (px 0) auto
                            ]
                        ]
                        (List.map (note model)
                            (model.notes
                                |> prioritizePinned
                                |> (\e ->
                                        case model.filters.label of
                                            Just label ->
                                                e |> List.filter (\l -> List.any (\j -> j == label) l.labels)

                                            Nothing ->
                                                e
                                   )
                            )
                        )
            ]
        ]


note : Model -> Note -> Html Msg
note model data =
    let
        noteTitle =
            case data.title of
                Nothing ->
                    text ""

                Just title ->
                    div
                        [ css
                            [ delaGothicOne
                            , borderBottom3 (px 1) solid (rgb 0 0 0)
                            , padding (px 10)
                            ]
                        ]
                        [ text title ]

        content =
            p [ css [ publicSans, padding (px 10) ] ]
                (let
                    -- NOTE: \n doesn't break into a newline so I do this
                    makeParagraph : List (Html msg) -> List String -> List (Html msg)
                    makeParagraph total next =
                        case next of
                            [] ->
                                total

                            x :: xs ->
                                makeParagraph (total ++ [ br [] [], text x ]) xs
                 in
                 data.content
                    |> String.split "\n"
                    |> (\r ->
                            case r of
                                [] ->
                                    [ text data.content ]

                                h :: t ->
                                    if List.length t > 0 then
                                        makeParagraph [ text h ] t

                                    else
                                        [ text h ]
                       )
                )

        labelsFooter =
            case data.labels of
                [] ->
                    text ""

                labels ->
                    div
                        [ css
                            [ padding (px 10)
                            , displayFlex
                            , flexWrap wrap
                            , gap 5
                            , marginTop auto
                            ]
                        ]
                        (List.map
                            (\l ->
                                div
                                    [ css
                                        [ backgroundColor (hex "#6ac0ff")
                                        , padding (px 2)
                                        , border3 (px 1) solid (rgb 0 0 0)
                                        , hover [ boxShadow4 (px 3) (px 3) (px 0) (rgb 0 0 0) ]
                                        , displayFlex
                                        ]
                                    , class "note-label"
                                    ]
                                    [ text l.name
                                    , button
                                        [ class "note-label-remove-button"
                                        , css
                                            [ border3 (px 1) solid (rgb 0 0 0)
                                            , padding2 (px 0) (px 2)
                                            , marginLeft (px 3)
                                            , backgroundColor (hex "ff0000")
                                            , color (hex "fff")
                                            , cursor pointer
                                            ]
                                        , type_ "button"
                                        , onClick
                                            (RemoveLabelFromNote
                                                { noteID = data.id
                                                , labelID = l.id
                                                }
                                            )
                                        ]
                                        [ text "X" ]
                                    ]
                            )
                            (model.labels |> List.filter (\e -> List.any (\r -> r == e.id) labels))
                        )
    in
    div
        [ css
            [ border3 (px 3) solid (rgb 0 0 0)
            , displayFlex
            , flexDirection column
            , maxWidth (px 240)
            , minWidth (px 240)
            , minHeight (px 120)
            , backgroundColor (rgb 255 203 127)
            , hover
                [ boxShadow4 (px 6) (px 6) (px 0) (rgb 0 0 0) ]
            ]
        ]
        [ noteTitle
        , content
        , labelsFooter
        ]


prioritizePinned : List Note -> List Note
prioritizePinned notes =
    let
        pinned =
            List.filter (\n -> n.pinned == True) notes

        unpinned =
            List.filter (\n -> n.pinned == False) notes
    in
    pinned ++ unpinned
