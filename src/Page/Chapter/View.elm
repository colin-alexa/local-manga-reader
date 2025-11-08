module Page.Chapter.View exposing (view)

import Components exposing (loadingSpinner, pageHeaderView, toggle, webDataView)
import Css exposing (..)
import Data.Chapter exposing (Chapter)
import Html.Styled as Styled exposing (a, div, img, label, styled, text)
import Html.Styled.Attributes exposing (for, href, id, src)
import Page.Chapter exposing (Model, Msg(..), PreviewImageSize(..))
import Request exposing (getPageAssetUrl)
import Route exposing (Route(..))
import VirtualDom


previewImageContainer : PreviewImageSize -> List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
previewImageContainer size =
    let
        ( imageWidth, imageHeight ) =
            case size of
                Small ->
                    ( inches 1, inches 1.5 )

                Large ->
                    ( inches 2.5, inches 3.75 )
    in
    styled div
        [ width imageWidth
        , height imageHeight
        ]


pagePreviewImage : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
pagePreviewImage =
    styled img
        [ width (pct 100)
        ]


pagePreviewCard : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
pagePreviewCard =
    styled a
        [ displayFlex
        , flexDirection column
        , alignItems center
        , justifyContent spaceBetween
        , cursor pointer
        , textDecoration none
        , color (hex "000")
        , visited
            [ color (hex "000") ]
        ]


pagePreviewList : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
pagePreviewList =
    styled div
        [ displayFlex
        , property "gap" ".5rem"
        , flexWrap wrap
        , marginTop (px 24)
        ]


view : Model -> VirtualDom.Node Msg
view model =
    Styled.toUnstyled
        (div []
            [ webDataView
                { loading = loadingSpinner (rem 1) [] []
                , error = text
                , view = viewChapter model.previewSize
                }
                model.chapter
            , img [] []
            ]
        )


previewImageSizeToggle : Bool -> Styled.Html Msg
previewImageSizeToggle state =
    toggle state
        (\isOn ->
            if isOn then
                SetImagePreviewSize Large

            else
                SetImagePreviewSize Small
        )
        [ id "preview-image-size" ]


viewChapter : PreviewImageSize -> Chapter -> Styled.Html Msg
viewChapter size chapter =
    div []
        [ pageHeaderView []
            [ text (chapter.name ++ " - " ++ String.fromInt chapter.pageCount ++ " pages") ]
            [ styled label [ marginRight (rem 0.25) ] [ for "preview-image-size" ] [ text "Larger Images: " ]
            , previewImageSizeToggle (size == Large)
            ]
        , pagePreviewList [] <| List.map (viewPagePreview size chapter) (List.range 1 chapter.pageCount)
        ]


viewPagePreview : PreviewImageSize -> Chapter -> Int -> Styled.Html Msg
viewPagePreview size chapter page =
    let
        maybeImageUrl =
            getPageAssetUrl { chapter = chapter, page = page }
    in
    case maybeImageUrl of
        Nothing ->
            img [] []

        Just url ->
            pagePreviewCard [ href (Route.urlForRoute <| Reader chapter.seriesId chapter.id page) ]
                [ previewImageContainer size
                    []
                    [ pagePreviewImage [ src url ] [] ]
                , text (String.fromInt page)
                ]
