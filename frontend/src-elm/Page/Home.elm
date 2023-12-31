module Page.Home exposing (..)

import Api exposing (SyncableID(..))
import Browser.Dom exposing (getElement)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Cmd.Extra exposing (pure)
import Css exposing (alignItems, auto, backgroundColor, bold, bolder, border, border3, borderBottom3, borderLeft3, borderRight3, borderTop3, boxShadow4, center, color, column, cursor, displayFlex, ellipsis, flexDirection, flexStart, flexWrap, fontSize, fontWeight, height, hex, hidden, hover, inherit, int, justifyContent, margin, margin2, marginBottom, marginLeft, marginRight, marginTop, maxWidth, minHeight, minWidth, noWrap, overflow, overflowY, padding, padding2, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, pointer, position, property, px, rgb, solid, spaceBetween, start, sticky, textAlign, textOverflow, top, transparent, whiteSpace, width, wrap)
import CssHelpers exposing (black, col, delaGothicOne, displayGrid, error, fullWidth, gap, mx, padX, padY, primary, publicSans, row, secondary, textColor, userSelectNone, white)
import DataTypes exposing (Label, Note)
import Helpers exposing (elIsIn, exclude, getCurrentTime, idDiff, labelIDsSplitter, listFirst, maybeToBool, or, partitionFirst, sameId)
import Html.Styled exposing (Html, br, button, div, form, img, input, label, li, nav, p, span, strong, text, textarea, ul)
import Html.Styled.Attributes exposing (class, css, for, id, placeholder, src, style, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit, stopPropagationOn)
import Http
import Json.Decode as JD
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (Action(..), OQCreateLabel, OQCreateNote, OQDeleteNote, OQEditNote, OfflineQueueOps, emptyOfflineQueue, offlineQueueIsEmpty, qCreateNewNote, qDeleteNote, qEditNoteLabels, qNewLabel, qToggleNotePin, queueToOperations)
import Platform.Sub as Sub
import Ports exposing (receiveRandomValues, requestRandomValues, updateLastSyncedAt, windowResized)
import Random
import Route
import Svg.Styled
import Task
import Time exposing (Posix)
import UID exposing (generateUID)



-- SUBSCRIPTIONS


pureNoSignal : Model -> ( Model, Cmd msg, Maybe Signal )
pureNoSignal m =
    ( m, Cmd.none, Nothing )


pureWithSignal : Signal -> Model -> ( Model, Cmd msg, Maybe Signal )
pureWithSignal s m =
    ( m, Cmd.none, Just s )


selectedNoteHotkeys : SyncableID -> String -> Msg
selectedNoteHotkeys noteId string =
    case string of
        "ArrowLeft" ->
            IncreaseNoteOrder noteId

        "ArrowRight" ->
            DecreaseNoteOrder noteId

        "Escape" ->
            DeselectNote

        _ ->
            NoOp


clickedDocDecoder : JD.Decoder Msg
clickedDocDecoder =
    JD.map2
        (\x y -> ClickedMouse { x = x, y = y })
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveRandomValues ReceivedRandomValues
        , case model.selectedNote of
            Just n ->
                Sub.batch
                    [ onKeyDown (JD.map (selectedNoteHotkeys n) (JD.field "key" JD.string))
                    , Browser.Events.onClick clickedDocDecoder
                    ]

            Nothing ->
                Sub.none
        ]



--


type alias LabelsColumnMenu =
    Maybe String


type alias Model =
    { newLabelName : String
    , selectedNote : Maybe SyncableID
    , searchQuery : String

    -- global data
    , key : Nav.Key
    , seeds : List Random.Seed
    , notes : List Note
    , labels : List Label
    }


init :
    { key : Nav.Key
    , seeds : List Random.Seed
    , labels : List Label
    , notes : List Note
    }
    -> Model
init { key, seeds, labels, notes } =
    { key = key
    , seeds = seeds
    , searchQuery = ""
    , notes = notes
    , newLabelName = ""
    , selectedNote = Nothing
    , labels = labels
    }


type Msg
    = NoOp
    | ChangeNotePinned ( SyncableID, Bool )
    | ReceivedRandomValues (List Int)
    | RemoveLabelFromNote { noteID : SyncableID, labelID : SyncableID }
    | SelectedNote SyncableID
    | DeselectNote
    | ClickedMouse { x : Float, y : Float }
    | EditNote SyncableID
    | RequestTimeForNewNoteCreation
    | CreateAndEditNote Posix
    | IncreaseNoteOrder SyncableID
    | DecreaseNoteOrder SyncableID
    | ClearLabelFilter


type Signal
    = OfflineQueueAction OfflineQueue.Action
    | ClearLabelFilters



{-
   Check if cursor is inside a DOM node
-}


cursorInNode : { x1 : Float, y1 : Float } -> { x2 : Float, y2 : Float, width : Float, height : Float } -> Bool
cursorInNode { x1, y1 } { x2, y2, width, height } =
    x2 <= x1 && x1 <= x2 + width && y2 <= y1 && y1 <= y2 + height


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Signal )
update msg model =
    let
        fire l =
            ( model, l, Nothing )
    in
    case msg of
        NoOp ->
            model |> pureNoSignal

        RequestTimeForNewNoteCreation ->
            ( model, Cmd.batch [ getCurrentTime CreateAndEditNote, requestRandomValues () ], Nothing )

        CreateAndEditNote currentTime ->
            let
                offlineId : String
                offlineId =
                    generateUID model.seeds |> Tuple.first

                newNote =
                    { id = offlineId
                    , title = Nothing
                    , content = "Hello world!"
                    , pinned = False
                    , labels = []
                    , order = Nothing
                    }
            in
            -- TODO: handle case where note could not be created but i'm in note editing page
            -- handle that response
            ( { model
                | notes =
                    { id = OfflineID offlineId
                    , title = Nothing
                    , content = "Hello world!"
                    , pinned = False
                    , createdAt = currentTime
                    , updatedAt = currentTime
                    , labels = []
                    , order = List.length model.notes + 1
                    }
                        :: model.notes
              }
            , Route.replaceUrl model.key (Route.EditNote (OfflineID offlineId))
            , Just (OfflineQueueAction (QCreateNewNote newNote))
            )

        EditNote id ->
            ( model, Route.replaceUrl model.key (Route.EditNote id), Nothing )

        DeselectNote ->
            case model.selectedNote of
                Just _ ->
                    { model | selectedNote = Nothing } |> pureNoSignal

                Nothing ->
                    model |> pureNoSignal

        ClearLabelFilter ->
            ( model, Cmd.none, Just ClearLabelFilters )

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

        IncreaseNoteOrder uid ->
            case listFirst (\e -> sameId e.id uid) model.notes of
                Nothing ->
                    model |> pureNoSignal

                Just noteExists ->
                    let
                        biggerOrderNote =
                            List.foldl
                                (\next acc ->
                                    case acc of
                                        Nothing ->
                                            if next.order > noteExists.order then
                                                Just next

                                            else
                                                Nothing

                                        Just accVal ->
                                            if next.order < accVal.order && next.order > noteExists.order then
                                                Just next

                                            else
                                                Just accVal
                                )
                                Nothing
                                model.notes
                    in
                    case biggerOrderNote of
                        Just biggerOrderNoteExists ->
                            { model
                                | notes =
                                    List.map
                                        (\n ->
                                            if n.id == noteExists.id then
                                                { n | order = biggerOrderNoteExists.order }

                                            else if n.id == biggerOrderNoteExists.id then
                                                { n | order = noteExists.order }

                                            else
                                                n
                                        )
                                        model.notes
                            }
                                |> pureWithSignal
                                    (OfflineQueueAction
                                        (QSwitchNoteOrders
                                            ( noteExists.id, noteExists.order )
                                            ( biggerOrderNoteExists.id, biggerOrderNoteExists.order )
                                        )
                                    )

                        Nothing ->
                            model |> pureNoSignal

        DecreaseNoteOrder uid ->
            case listFirst (\e -> sameId e.id uid) model.notes of
                Nothing ->
                    model |> pureNoSignal

                Just noteExists ->
                    let
                        smallerOrderNote =
                            List.foldl
                                (\next acc ->
                                    case acc of
                                        Nothing ->
                                            if next.order < noteExists.order then
                                                Just next

                                            else
                                                Nothing

                                        Just accVal ->
                                            if next.order > accVal.order && next.order < noteExists.order then
                                                Just next

                                            else
                                                Just accVal
                                )
                                Nothing
                                model.notes
                    in
                    case smallerOrderNote of
                        Just smallerOrderNoteExists ->
                            { model
                                | notes =
                                    List.map
                                        (\n ->
                                            if n.id == noteExists.id then
                                                { n | order = smallerOrderNoteExists.order }

                                            else if n.id == smallerOrderNoteExists.id then
                                                { n | order = noteExists.order }

                                            else
                                                n
                                        )
                                        model.notes
                            }
                                |> pureWithSignal
                                    (OfflineQueueAction
                                        (QSwitchNoteOrders
                                            ( noteExists.id, noteExists.order )
                                            ( smallerOrderNoteExists.id, smallerOrderNoteExists.order )
                                        )
                                    )

                        Nothing ->
                            model |> pureNoSignal

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

        ClickedMouse { x, y } ->
            case model.selectedNote of
                Just selectedNote ->
                    let
                        id =
                            case selectedNote of
                                DatabaseID i ->
                                    String.fromInt i

                                OfflineID i ->
                                    i
                    in
                    fire
                        (getElement ("note-" ++ id)
                            |> Task.attempt
                                (\res ->
                                    case res of
                                        Ok el ->
                                            let
                                                elData : { x2 : Float, y2 : Float, width : Float, height : Float }
                                                elData =
                                                    { x2 = el.element.x, y2 = el.element.y, width = el.element.width, height = el.element.height }
                                            in
                                            if cursorInNode { x1 = x, y1 = y } elData then
                                                NoOp

                                            else
                                                DeselectNote

                                        Err _ ->
                                            NoOp
                                )
                        )

                Nothing ->
                    model |> pureNoSignal

        ReceivedRandomValues values ->
            { model | seeds = List.map Random.initialSeed values }
                |> pureNoSignal

        SelectedNote id ->
            { model
                | selectedNote =
                    case model.selectedNote of
                        Nothing ->
                            Just id

                        Just oldId ->
                            if sameId id oldId then
                                Nothing

                            else
                                Just id
            }
                |> pureNoSignal



-- TODO: for when there's multiple selected
-- let
--     noteAlreadySelected =
--         List.any (sameId id) model.selectedNotes
-- in
-- { model
--     | selectedNotes =
--         if noteAlreadySelected then
--             model.selectedNotes |> exclude (sameId id)
--         else
--             id :: model.selectedNotes
-- }
-- |> pureNoSignal


alwaysStopPropagation : a -> ( a, Bool )
alwaysStopPropagation x =
    ( x, True )


clickedOnSelectedNote : Svg.Styled.Attribute Msg
clickedOnSelectedNote =
    stopPropagationOn "click" (JD.succeed ( NoOp, True ))


labelsMenuWidth : Float
labelsMenuWidth =
    277


noteCardWidth : number
noteCardWidth =
    240


noteCardGridGapSize : number
noteCardGridGapSize =
    10


view :
    Model
    -> { width : Int, height : Int }
    -> Maybe SyncableID
    -> Bool
    -> Html Msg
view model windowRes filteringByLabel labelsMenuIsOpen =
    let
        scrollbarWidth : number
        scrollbarWidth =
            -- more or less
            50

        menuWidth =
            if labelsMenuIsOpen then
                labelsMenuWidth

            else
                0

        notesPerRowRec : Float -> Float
        notesPerRowRec total =
            if total + noteCardWidth + noteCardGridGapSize + scrollbarWidth + menuWidth >= toFloat windowRes.width then
                total - noteCardGridGapSize

            else
                notesPerRowRec (total + noteCardWidth + noteCardGridGapSize)

        gridWidth : Float
        gridWidth =
            notesPerRowRec 0

        sidesMargin =
            (toFloat windowRes.width - gridWidth - menuWidth) / 2

        header =
            div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , marginBottom (px 32)
                    , alignItems start
                    , width (px gridWidth)
                    , marginLeft (px sidesMargin)
                    , paddingTop (px 28)
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
                , button
                    [ css
                        [ publicSans
                        , fontWeight (int 800)
                        , padY (px 18)
                        , backgroundColor white
                        , border3 (px 5) solid black
                        , cursor pointer
                        , textAlign center
                        , fontSize (px 18)
                        , width (px (2 * noteCardWidth + noteCardGridGapSize))
                        ]
                    , onClick RequestTimeForNewNoteCreation
                    ]
                    [ text "CREATE NEW NOTE"
                    ]
                ]

        filteredByLabelHeader filterLabel =
            let
                amount =
                    model.notes
                        |> List.filter (\e -> List.any (\r -> r == filterLabel.id) e.labels)
                        |> List.length
            in
            row
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , marginBottom (px 32)
                    , alignItems start
                    , width (px gridWidth)
                    , marginLeft (px sidesMargin)
                    , paddingTop (px 28)
                    ]
                ]
                [ col [ css [ paddingRight (px 50) ] ]
                    [ p [ css [ publicSans, fontSize (px 22), textColor white ] ]
                        (case amount of
                            0 ->
                                [ text "There are no notes with the label:" ]

                            1 ->
                                [ text "There is "
                                , strong [] [ text "1" ]
                                , text " note with the label:"
                                ]

                            _ ->
                                [ text "There are "
                                , strong [] [ text (String.fromInt amount) ]
                                , text " notes with the label:"
                                ]
                        )
                    , p
                        [ css
                            [ delaGothicOne
                            , fontSize (px 42)
                            , textColor white
                            ]
                        ]
                        [ text filterLabel.name ]
                    ]
                , button
                    [ css
                        [ publicSans
                        , fontWeight (int 800)
                        , padY (px 18)
                        , backgroundColor error
                        , border3 (px 5) solid black
                        , cursor pointer
                        , textColor white
                        , textAlign center
                        , fontSize (px 18)
                        , minWidth (px noteCardWidth)
                        , maxWidth (px noteCardWidth)
                        , hover [ backgroundColor black ]
                        ]
                    , onClick ClearLabelFilter
                    ]
                    [ text "CLEAR FILTERS"
                    ]
                ]
    in
    div
        [ css [ width (pct 100), overflowY auto ]
        ]
        [ col
            []
            [ case filteringByLabel of
                Just filterLabel ->
                    case listFirst (.id >> sameId filterLabel) model.labels of
                        Just l ->
                            filteredByLabelHeader l

                        Nothing ->
                            -- TODO: what now?
                            header

                Nothing ->
                    header
            , case model.notes of
                [] ->
                    -- TODO: add no notes empty state design
                    div [] []

                _ ->
                    div
                        [ -- TODO: give the tiled effect of google keep
                          -- using translate and transitions
                          css
                            [ marginTop (px 30)
                            , alignItems flexStart
                            , paddingBottom (px 188)
                            ]
                        ]
                        [ row
                            [ css
                                [ width (px gridWidth)
                                , flexWrap wrap
                                , marginLeft (px sidesMargin)
                                , gap noteCardGridGapSize
                                , alignItems flexStart
                                ]
                            ]
                            (List.map (noteCard model)
                                (model.notes
                                    |> List.sortWith flippedComparison
                                    |> prioritizePinned
                                    |> (\e ->
                                            case filteringByLabel of
                                                Just label ->
                                                    e |> List.filter (\l -> List.any (\j -> j == label) l.labels)

                                                Nothing ->
                                                    e
                                       )
                                    |> (\e ->
                                            case model.searchQuery of
                                                "" ->
                                                    e

                                                query ->
                                                    e
                                                        |> List.filter
                                                            (\l ->
                                                                let
                                                                    includesTitle =
                                                                        case l.title of
                                                                            Nothing ->
                                                                                False

                                                                            Just title ->
                                                                                String.contains (String.toLower query) (String.toLower title)

                                                                    includesContent =
                                                                        String.contains (String.toLower query) (String.toLower l.content)
                                                                in
                                                                includesTitle || includesContent
                                                            )
                                       )
                                    |> List.indexedMap
                                        (\i e ->
                                            ( e
                                            , i
                                            , case model.selectedNote of
                                                Nothing ->
                                                    False

                                                Just selId ->
                                                    sameId e.id selId
                                            )
                                        )
                                )
                            )
                        ]
            ]
        ]


flippedComparison : { a | order : comparable } -> { b | order : comparable } -> Order
flippedComparison a b =
    case compare a.order b.order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


noteCard : Model -> ( Note, Int, Bool ) -> Html Msg
noteCard model ( data, order, selected ) =
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
            p
                [ css [ publicSans, padding (px 10), height (pct 100) ]
                ]
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
                    ul
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
                                li
                                    [ css
                                        [ backgroundColor
                                            (if selected then
                                                white

                                             else
                                                primary
                                            )
                                        , padding (px 2)
                                        , border3 (px 1) solid (rgb 0 0 0)
                                        , hover [ boxShadow4 (px 3) (px 3) (px 0) (rgb 0 0 0) ]
                                        , displayFlex
                                        , publicSans
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
                                        , stopPropagationOn "click"
                                            (JD.succeed
                                                ( RemoveLabelFromNote
                                                    { noteID = data.id
                                                    , labelID = l.id
                                                    }
                                                , True
                                                )
                                            )
                                        ]
                                        [ text "X" ]
                                    ]
                            )
                            (model.labels |> List.filter (\e -> List.any (\r -> r == e.id) labels))
                        )

        topActions =
            if not selected then
                text ""

            else
                row
                    [ css [ backgroundColor white, justifyContent spaceBetween, borderBottom3 (px 3) solid black ]
                    ]
                    [ div
                        [ css
                            [ if data.pinned then
                                backgroundColor black

                              else
                                hover [ backgroundColor black, color white ]
                            , color
                                (if data.pinned then
                                    white

                                 else
                                    black
                                )
                            , borderRight3 (px 3) solid black
                            , cursor pointer
                            , padding (px 4)
                            , displayFlex
                            , alignItems center
                            , justifyContent center
                            ]
                        , onClick (ChangeNotePinned ( data.id, not data.pinned ))
                        ]
                        [ Filled.push_pin 32 Inherit |> Svg.Styled.fromUnstyled ]
                    , div
                        [ css
                            [ hover
                                [ backgroundColor black
                                , color white
                                ]
                            , borderLeft3 (px 3) solid black
                            , cursor pointer
                            , padding (px 4)
                            , displayFlex
                            , alignItems center
                            , justifyContent center
                            ]
                        , onClick (EditNote data.id)
                        ]
                        [ Filled.edit 32 Inherit |> Svg.Styled.fromUnstyled ]
                    ]

        bottomActions =
            if not selected then
                text ""

            else
                row
                    [ css [ backgroundColor white, justifyContent spaceBetween, borderTop3 (px 3) solid black, alignItems center, paddingRight (px 8) ]
                    ]
                    [ row []
                        [ div
                            [ css
                                [ hover [ backgroundColor black, color white ]
                                , color black
                                , borderRight3 (px 3) solid black
                                , cursor pointer
                                , padding (px 4)
                                , displayFlex
                                , alignItems center
                                , justifyContent center
                                ]
                            , onClick (IncreaseNoteOrder data.id)
                            ]
                            [ Filled.chevron_left 32 Inherit |> Svg.Styled.fromUnstyled ]
                        , div
                            [ css
                                [ hover [ backgroundColor black, color white ]
                                , color black
                                , borderRight3 (px 3) solid black
                                , cursor pointer
                                , padding (px 4)
                                , displayFlex
                                , alignItems center
                                , justifyContent center
                                ]
                            , onClick (DecreaseNoteOrder data.id)
                            ]
                            [ Filled.chevron_right 32 Inherit |> Svg.Styled.fromUnstyled ]
                        ]
                    , p [ css [ publicSans, fontWeight bold ] ] [ text ("#" ++ String.fromInt (order + 1)) ]
                    ]
    in
    div
        ([ css
            ([ border3 (px 3) solid (rgb 0 0 0)
             , displayFlex
             , flexDirection column
             , maxWidth (px noteCardWidth)
             , minWidth (px noteCardWidth)
             , minHeight (px 120)
             , backgroundColor (rgb 255 203 127)
             , hover [ boxShadow4 (px 6) (px 6) (px 0) (rgb 0 0 0) ]
             ]
                ++ (if selected then
                        [ backgroundColor primary
                        ]

                    else
                        []
                   )
            )
         , id
            ("note-"
                ++ (case data.id of
                        DatabaseID id ->
                            String.fromInt id

                        OfflineID id ->
                            id
                   )
            )
         , if selected then
            clickedOnSelectedNote

           else
            css []

         -- TODO: like elm blueprint planner, check if clicked and released on same spot
         -- otherwise picks up selecting text
         ]
            ++ (if selected then
                    []

                else
                    [ stopPropagationOn "click" (JD.succeed ( SelectedNote data.id, True ))
                    ]
                -- TODO: unselect on click outside of note
               )
        )
        [ topActions
        , noteTitle
        , content
        , labelsFooter
        , bottomActions
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
