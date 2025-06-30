module Page.AllSeries.View exposing (..)

import Components exposing (loadingSpinner, pageHeaderView, webDataView)
import Css exposing (..)
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Page.AllSeries exposing (Model, Msg(..), Series)
import VirtualDom


view : Model -> VirtualDom.Node Msg
view model =
    Styled.toUnstyled
        (webDataView
            { loading = loadingSpinner (pct 10) [] []
            , error = text
            , view = viewAllSeries
            }
            model.allSeries
        )


viewAllSeries : List Series -> Html Msg
viewAllSeries series =
    div []
        [ pageHeaderView []
            [ homeLink ]
            []
        , seriesList [] <| List.map seriesCard series
        ]


homeLink =
    styled a
        []
        []
        [ text "home" ]


seriesList =
    styled div
        [ displayFlex
        ]


seriesCard : Series -> Html Msg
seriesCard series =
    styled div
        [ displayFlex
        , flexDirection column
        ]
        []
        [ text <| series.name ++ ", by " ++ Maybe.withDefault "unknown" series.author
        , webDataView
            { loading = loadingSpinner (rem 1) [] []
            , error = text
            , view = text << String.fromInt
            }
            series.chapters
        ]
