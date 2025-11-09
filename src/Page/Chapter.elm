module Page.Chapter exposing (..)

import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter)
import Data.Series exposing (Series)
import RemoteData exposing (WebData)
import Request exposing (getChapter, getSeries)


type PreviewImageSize
    = Small
    | Large


type alias Model =
    { navKey : Nav.Key
    , series : WebData Series
    , chapter : WebData Chapter
    , previewSize : PreviewImageSize
    }


type Msg
    = SeriesReceived (WebData Series)
    | ChapterReceived (WebData Chapter)
    | SetImagePreviewSize PreviewImageSize


init : Nav.Key -> ApiId -> ApiId -> ( Model, Cmd Msg )
init key seriesId chapterId =
    ( { navKey = key, series = RemoteData.Loading, chapter = RemoteData.Loading, previewSize = Small }
    , Cmd.batch [ getSeries seriesId SeriesReceived, getChapter chapterId ChapterReceived ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeriesReceived seriesData ->
            ( { model | series = seriesData }, Cmd.none )

        ChapterReceived chapterData ->
            ( { model | chapter = chapterData }, Cmd.none )

        SetImagePreviewSize newSize ->
            ( { model | previewSize = newSize }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
