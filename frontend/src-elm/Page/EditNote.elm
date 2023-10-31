module Page.EditNote exposing (..)

import Api exposing (SyncableID)
import Browser.Navigation as Nav
import Css exposing (alignItems, backgroundColor, border3, borderBottom3, borderLeft3, borderRight3, borderTop3, center, fontSize, height, justifyContent, padding, paddingBottom, paddingTop, pct, px, solid, spaceBetween, width)
import CssHelpers exposing (black, col, delaGothicOne, error, padX, publicSans, row, secondary, white)
import DataTypes exposing (Label, Note)
import Helpers exposing (listFirst, sameId)
import Html.Styled exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (css)
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Random
import Svg.Styled


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
    let
        note =
            listFirst (.id >> sameId model.noteId) model.notes
    in
    case note of
        Nothing ->
            -- TODO: empty state when no note or just redirect?
            div [] [ text "Note Not found" ]

        Just val ->
            let
                topActions =
                    row [ css [ justifyContent spaceBetween, backgroundColor white, borderBottom3 (px 3) solid black ] ]
                        [ div [ css [ paddingTop (px 8), paddingBottom (px 6), padX (px 8), borderRight3 (px 2) solid black ] ] [ Filled.push_pin 32 Inherit |> Svg.Styled.fromUnstyled ]
                        , p [ css [ delaGothicOne, fontSize (px 24) ] ] [ text "Editing Note" ]
                        , div [ css [ paddingTop (px 8), paddingBottom (px 6), padX (px 8), backgroundColor error, borderLeft3 (px 2) solid black ] ] [ Filled.close 32 Inherit |> Svg.Styled.fromUnstyled ]
                        ]

                bottomActions =
                    row [ css [ justifyContent spaceBetween, backgroundColor white, borderTop3 (px 3) solid black ] ]
                        [ div [ css [ backgroundColor error, paddingTop (px 8), paddingBottom (px 6), padX (px 8), borderRight3 (px 2) solid black ] ] [ Filled.delete 28 Inherit |> Svg.Styled.fromUnstyled ]
                        , div [ css [ paddingTop (px 8), paddingBottom (px 6), padX (px 8), borderLeft3 (px 2) solid black ] ] [ Filled.label 28 Inherit |> Svg.Styled.fromUnstyled ]
                        ]
            in
            row [ css [ height (pct 100), justifyContent center, alignItems center ] ]
                [ div
                    [ css
                        [ -- TODO: a better width
                          width (px 400)
                        ]
                    ]
                    [ col [ css [ backgroundColor secondary, border3 (px 3) solid black ] ]
                        [ topActions
                        , p [ css [ delaGothicOne, fontSize (px 24), padding (px 16) ] ]
                            [ text
                                (case val.title of
                                    Just t ->
                                        t

                                    Nothing ->
                                        ""
                                )
                            ]
                        , p [ css [ publicSans, padding (px 16) ] ] [ text val.content ]
                        , bottomActions
                        ]
                    ]
                ]
