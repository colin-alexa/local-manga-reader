module Page.Reader.View exposing (view)

import Components exposing (loadingSpinner, navLink, pageHeaderView, toggle, webDataView)
import Css exposing (..)
import Data.Chapter exposing (Chapter)
import Data.ReaderLocation exposing (ReaderLocation)
import Data.Series exposing (Series)
import Html
import Html.Styled as Styled exposing (a, div, img, label, styled, text)
import Html.Styled.Attributes exposing (for, href, id, src)
import Html.Styled.Events exposing (onClick)
import Page.Reader exposing (Media(..), Model, Msg(..))
import RemoteData exposing (WebData)
import Request exposing (getPageAssetUrl)
import Route exposing (Route(..))
import VirtualDom


view : Model -> Html.Html Msg
view model =
    Styled.toUnstyled <|
        div []
            [ pageHeaderView
                []
                [ navLink Route.AllSeries "home"
                , seriesNav model.series
                , chapterNav model.location
                , pageNav model.location
                ]
                []
            , viewLoadedPage model.location
            ]


seriesNav : WebData Series -> Styled.Html Msg
seriesNav seriesData =
    webDataView
        { loading = loadingSpinner (rem 1) [] []
        , error = text
        , view = \series -> navLink (Route.Series series.id) series.name
        }
        seriesData


chapterNav : WebData ReaderLocation -> Styled.Html Msg
chapterNav locationData =
    webDataView
        { loading = loadingSpinner (rem 1) [] []
        , error = text
        , view = \{ chapter } -> navLink (Route.Chapter chapter.seriesId chapter.id) chapter.name
        }
        locationData


pageNav : WebData ReaderLocation -> Styled.Html Msg
pageNav locationData =
    webDataView
        { loading = loadingSpinner (rem 1) [] []
        , error = text
        , view =
            \{ chapter, page } ->
                navLink
                    (Route.Reader chapter.seriesId chapter.id page)
                    (String.fromInt page)
        }
        locationData


controlsHeight media =
    case media of
        Mobile _ ->
            inches 0.3

        _ ->
            inches 0


pageHeight media =
    calc (vh 100) minus (controlsHeight media)


readerContainer media =
    styled div
        [ position relative
        , marginTop (px 20)
        , height (pageHeight media)
        ]


navigateLeftOverlay =
    styled div
        [ position absolute
        , left zero
        , width (pct 50)
        , height (pct 100)
        ]


navigateRightOverlay =
    styled div
        [ position absolute
        , right zero
        , width (pct 50)
        , height (pct 100)
        ]


reader : Model -> Styled.Html Msg
reader model =
    readerContainer model.media
        []
        [ viewLoadedPage model.location
        , navigateLeftOverlay [ onClick PrevPage ] []
        ]


viewLoadedPage : WebData ReaderLocation -> Styled.Html Msg
viewLoadedPage locationData =
    webDataView
        { loading = loadingSpinner (rem 10) [] []
        , error = text
        , view = \location -> viewPageImage location
        }
        locationData


viewPageImage : ReaderLocation -> Styled.Html Msg
viewPageImage location =
    let
        maybeImageUrl =
            getPageAssetUrl location
    in
    case maybeImageUrl of
        Nothing ->
            img [] []

        Just url ->
            img [ src url ] []
