module Page.Series.View exposing (..)

import Browser.Navigation exposing (load)
import Components exposing (loadingSpinner, pageHeaderView, webDataView)
import Css exposing (..)
import Data.Chapter exposing (Chapter)
import Data.Series exposing (Series)
import Html.Styled as Styled exposing (Html, a, div, h3, li, styled, text, ul)
import Html.Styled.Attributes exposing (href)
import Page.Series exposing (Model, Msg(..))
import Route
import VirtualDom


styledPageBody : List (Styled.Attribute msg) -> List (Html msg) -> Html msg
styledPageBody =
    styled div
        [ displayFlex
        , alignItems spaceBetween
        , padding2 (rem 1) (rem 2)
        ]


styledView : Model -> Series -> Html Msg
styledView model series =
    div []
        [ pageHeaderView [] [ text series.name ] []
        , webDataView
            { loading = loadingSpinner (Css.rem 1) [] []
            , error = text
            , view = viewChapters series
            }
            model.chapters
        ]


viewChapters : Series -> List Chapter -> Html Msg
viewChapters series chapters =
    div []
        [ h3 [] [ text "Chapters" ]
        , ul [] (List.map (viewChapter series) chapters)
        ]


viewChapter : Series -> Chapter -> Html Msg
viewChapter series chapter =
    li []
        [ a
            [ href <| Route.urlForRoute (Route.Chapter series.id chapter.id) ]
            [ text <| String.fromInt chapter.number ++ " - " ++ chapter.name ]
        ]


view : Model -> VirtualDom.Node Msg
view model =
    Styled.toUnstyled
        (webDataView
            { loading = loadingSpinner (rem 1) [] []
            , error = text
            , view = styledView model
            }
            model.series
        )
