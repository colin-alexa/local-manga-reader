module Page.Chapters.View exposing (..)

import Css exposing (..)
import Data.Chapter exposing (Chapter)
import Error exposing (formatHttpError)
import Html.Styled as Styled exposing (Html, a, button, div, h3, styled, table, td, text, th, tr)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (onClick)
import Page.Chapters exposing (Model, Msg(..))
import RemoteData exposing (WebData)
import Route exposing (Route)
import VirtualDom


styledPageHeader : List (Styled.Attribute msg) -> List (Html msg) -> Html msg
styledPageHeader =
    styled div
        [ backgroundColor (hex "#FFF")
        , boxShadow5 zero (px 4) (px 4) (px 2) (rgba 0 0 0 0.2)
        , border zero
        , width (pct 100)
        , padding2 (rem 0.25) (rem 1)
        ]


styledPageBody : List (Styled.Attribute msg) -> List (Html msg) -> Html msg
styledPageBody =
    styled div
        [ displayFlex
        , alignItems spaceBetween
        , padding2 (rem 1) (rem 2)
        ]


styledView : Model -> Html Msg
styledView model =
    case model.series of
        RemoteData.NotAsked ->
            text "Huh?"

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Failure err ->
            text (formatHttpError err)

        RemoteData.Success series ->
            div []
                [ styledPageHeader [] [ text series.name ]
                , viewChapters model.chapters
                ]


viewChapters : WebData (List Chapter) -> Html Msg
viewChapters chaptersData =
    case chaptersData of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success chapters ->
            div []
                [ h3 [] [ text "Chapters" ]
                , div [] (List.map viewChapter chapters)
                ]

        RemoteData.Failure err ->
            text (formatHttpError err)


viewChapter : Chapter -> Html Msg
viewChapter chapter =
    div [] [ text chapter.name ]


view : Model -> VirtualDom.Node Msg
view =
    Styled.toUnstyled << styledView
