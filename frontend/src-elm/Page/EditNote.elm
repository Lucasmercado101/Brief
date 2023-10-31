module Page.EditNote exposing (..)

import Api exposing (SyncableID)
import Browser.Navigation as Nav
import Css exposing (alignItems, backgroundColor, bold, border, border3, borderBottom3, borderLeft3, borderRight3, borderTop3, center, color, column, cursor, display, displayFlex, flexDirection, fontSize, fontWeight, height, hover, inline, inlineBlock, int, justifyContent, marginBottom, marginTop, maxHeight, maxWidth, minWidth, none, padding, paddingBottom, paddingTop, pct, pointer, px, resize, solid, spaceBetween, stretch, textAlign, transparent, vertical, width)
import CssHelpers exposing (black, col, delaGothicOne, error, padX, padY, primary, publicSans, row, secondary, textColor, white)
import DataTypes exposing (Label, Note)
import Helpers exposing (listFirst, sameId)
import Html.Styled exposing (Html, button, div, input, p, strong, text, textarea)
import Html.Styled.Attributes exposing (autofocus, css, placeholder, title, value)
import Html.Styled.Events exposing (onClick, onInput)
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import OfflineQueue exposing (Action(..))
import Random
import Route
import Svg.Styled


pureNoSignal : Model -> ( Model, Cmd msg, Maybe Signal )
pureNoSignal m =
    ( m, Cmd.none, Nothing )


pureWithSignal : Signal -> Model -> ( Model, Cmd msg, Maybe Signal )
pureWithSignal s m =
    ( m, Cmd.none, Just s )


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
    , noteData : Maybe Note
    }


init :
    { notes : List Note
    , labels : List Label
    , noteId : SyncableID
    , seeds : List Random.Seed
    , key : Nav.Key
    , noteData : Maybe Note
    }
    -> Model
init { notes, labels, noteId, seeds, key, noteData } =
    { notes = notes
    , labels = labels
    , noteId = noteId
    , seeds = seeds
    , key = key
    , noteData = noteData
    }



-- UPDATE


type Msg
    = NoOp
    | FinishEditing
    | ChangePin Bool
    | ChangeTitle String
    | ChangeContent String


type Signal
    = OfflineQueueAction OfflineQueue.Action


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Signal )
update msg model =
    let
        noteExists =
            listFirst (.id >> sameId model.noteId) model.notes
    in
    case noteExists of
        Nothing ->
            -- TODO: what now?
            model |> pureNoSignal

        Just originalNote ->
            let
                changeNoteData fn =
                    { model
                        | noteData =
                            model.noteData
                                |> Maybe.map fn
                    }
            in
            case msg of
                NoOp ->
                    model |> pureNoSignal

                FinishEditing ->
                    case model.noteData of
                        Just noteData ->
                            let
                                titleChanged =
                                    noteData.title /= originalNote.title

                                contentChanged =
                                    noteData.content /= originalNote.content

                                queueSignal =
                                    if titleChanged || contentChanged then
                                        Just
                                            (OfflineQueueAction
                                                (QEditNote
                                                    { id = originalNote.id
                                                    , title =
                                                        if titleChanged then
                                                            noteData.title

                                                        else
                                                            Nothing
                                                    , content =
                                                        if contentChanged then
                                                            Just noteData.content

                                                        else
                                                            Nothing
                                                    , pinned = Nothing
                                                    , labels = Nothing
                                                    }
                                                )
                                            )

                                    else
                                        Nothing
                            in
                            ( model, Route.replaceUrl model.key Route.Home, queueSignal )

                        Nothing ->
                            model |> pureNoSignal

                ChangeTitle s ->
                    changeNoteData (\e -> { e | title = Just s })
                        |> pureNoSignal

                ChangeContent s ->
                    changeNoteData (\e -> { e | content = s })
                        |> pureNoSignal

                ChangePin newVal ->
                    ( { model
                        | notes =
                            model.notes
                                |> List.map
                                    (\e ->
                                        if sameId e.id originalNote.id then
                                            { e | pinned = newVal }

                                        else
                                            e
                                    )
                      }
                    , Cmd.none
                    , Just (OfflineQueueAction (QToggleNotePin originalNote.id newVal))
                    )



-- VIEW


view : Model -> Html Msg
view model =
    case model.noteData of
        Nothing ->
            -- TODO: empty state when no note or just redirect?
            div [] [ text "Note Not found" ]

        Just val ->
            let
                topActions =
                    row [ css [ justifyContent spaceBetween, alignItems center, backgroundColor white, borderBottom3 (px 3) solid black ] ]
                        [ button
                            [ css
                                [ border (px 0)
                                , if val.pinned then
                                    hover [ color black, backgroundColor white ]

                                  else
                                    hover [ backgroundColor black, color white ]
                                , cursor pointer
                                , displayFlex
                                , justifyContent center
                                , alignItems center
                                , paddingTop (px 8)
                                , paddingBottom (px 6)
                                , padX (px 8)
                                , borderRight3 (px 2) solid black
                                , if val.pinned then
                                    Css.batch [ backgroundColor black, color white ]

                                  else
                                    Css.batch []
                                ]
                            , title
                                (if val.pinned then
                                    "Unpin"

                                 else
                                    "pin"
                                )
                            , onClick (ChangePin (not val.pinned))
                            ]
                            [ Filled.push_pin 32 Inherit |> Svg.Styled.fromUnstyled ]
                        , p [ css [ delaGothicOne, fontSize (px 24) ] ] [ text "Editing Note" ]
                        , button
                            [ css [ hover [ backgroundColor black, color white ], border (px 0), cursor pointer, displayFlex, justifyContent center, alignItems center, paddingTop (px 8), paddingBottom (px 6), padX (px 8), backgroundColor primary, borderLeft3 (px 2) solid black, color black ]
                            , onClick FinishEditing
                            ]
                            [ Filled.check 32 Inherit |> Svg.Styled.fromUnstyled ]
                        ]

                bottomActions =
                    row [ css [ justifyContent spaceBetween, backgroundColor white, borderTop3 (px 3) solid black ] ]
                        [ button [ css [ border (px 0), cursor pointer, displayFlex, color white, justifyContent center, alignItems center, backgroundColor error, paddingTop (px 8), paddingBottom (px 6), padX (px 8), borderRight3 (px 2) solid black ] ] [ Filled.delete 28 Inherit |> Svg.Styled.fromUnstyled ]
                        , button [ css [ border (px 0), cursor pointer, displayFlex, justifyContent center, alignItems center, paddingTop (px 8), paddingBottom (px 6), padX (px 8), borderLeft3 (px 2) solid black ] ] [ Filled.label 28 Inherit |> Svg.Styled.fromUnstyled ]
                        ]

                confirmDeleteCard { name, id } =
                    div
                        [ css
                            [ backgroundColor secondary
                            , border3 (px 5) solid black
                            , displayFlex
                            , flexDirection column
                            , maxWidth (px 480)
                            , minWidth (px 480)
                            ]
                        ]
                        [ p [ css [ delaGothicOne, fontSize (px 38), marginTop (px 12), marginBottom (px 16), textAlign center ] ] [ text "Really?" ]
                        , div [ css [ display inlineBlock, textAlign center ] ]
                            [ p [ css [ publicSans, fontSize (px 18), display inline ] ]
                                [ text
                                    ("Are you sure you want to delete "
                                        ++ (case name of
                                                Just _ ->
                                                    "note \""

                                                Nothing ->
                                                    "this note?"
                                           )
                                    )
                                ]
                            , case name of
                                Just noteName ->
                                    strong [ css [ fontWeight (int 900), publicSans, fontSize (px 18), display inline ] ] [ text noteName ]

                                Nothing ->
                                    text ""
                            , case name of
                                Just noteName ->
                                    p [ css [ publicSans, fontSize (px 18), display inline ] ] [ text "\"?" ]

                                Nothing ->
                                    text ""
                            ]
                        , div [ css [ displayFlex, marginTop (px 16), backgroundColor white ] ]
                            [ button
                                [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor transparent, border (px 0), publicSans, fontSize (px 22), borderTop3 (px 5) solid black, width (pct 100), padY (px 10), textAlign center ]

                                -- , onClick (CancelDeleteLabel id)
                                ]
                                [ text "Cancel" ]
                            , button
                                [ css [ hover [ textColor white, backgroundColor black ], cursor pointer, fontWeight bold, backgroundColor error, textColor white, border (px 0), publicSans, fontSize (px 22), width (pct 100), borderLeft3 (px 5) solid black, borderTop3 (px 5) solid black, padY (px 10), textAlign center ]

                                -- , onClick (ConfirmDeleteLabel id)
                                ]
                                [ text "Confirm" ]
                            ]
                        ]
            in
            -- TODO: add max title and content length
            row [ css [ height (pct 100), justifyContent center, alignItems center ] ]
                [ div
                    [ css
                        [ -- TODO: a better width
                          minWidth (px 400)
                        , height (pct 100)
                        , displayFlex
                        , alignItems center
                        ]
                    ]
                    [ col [ css [ backgroundColor secondary, border3 (px 3) solid black, maxHeight (pct 70), width (pct 100) ] ]
                        [ topActions
                        , textarea
                            [ css [ delaGothicOne, fontSize (px 24), padding (px 16), backgroundColor transparent, border (px 0), resize vertical ]
                            , autofocus True
                            , placeholder "Note Title Here."
                            , onInput ChangeTitle
                            , value
                                (case val.title of
                                    Just t ->
                                        t

                                    Nothing ->
                                        ""
                                )
                            ]
                            []
                        , textarea
                            [ css [ publicSans, padding (px 16), backgroundColor transparent, border (px 0), fontSize (px 16), resize vertical ]
                            , onInput ChangeContent
                            , value val.content
                            , placeholder "Note Content Here."
                            ]
                            []
                        , bottomActions
                        ]
                    , confirmDeleteCard { name = val.title, id = model.noteId }
                    ]
                ]
