module Page.Reader exposing (..)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter)
import Data.ReaderLocation as ReaderLocation exposing (ReaderLocation)
import Data.Series exposing (Series)
import RemoteData exposing (WebData)
import Request
import Route
import Task exposing (perform)


type Media
    = Unknown
    | Desktop Viewport
    | Mobile Viewport


type alias Model =
    { navKey : Nav.Key
    , series : WebData Series
    , location : WebData ReaderLocation
    , media : Media
    }


type Msg
    = SetMedia Media
    | SeriesReceived (WebData Series)
    | ChaptersReceived ApiId Int (WebData (List Chapter))
    | PrevPage
    | PrevChapter
    | NextPage
    | NextChapter
    | JumpTo Chapter


whichMedia : Viewport -> Media
whichMedia vp =
    if vp.viewport.width > 1000 then
        Desktop vp

    else
        Mobile vp


queryMedia : Cmd Msg
queryMedia =
    perform (SetMedia << whichMedia) getViewport


init : Nav.Key -> ApiId -> ApiId -> Int -> ( Model, Cmd Msg )
init navKey series chapter page =
    ( { navKey = navKey
      , series = RemoteData.Loading
      , location = RemoteData.Loading
      , media = Unknown
      }
    , Cmd.batch
        [ queryMedia
        , Request.getSeries series SeriesReceived
        , Request.getChaptersForSeries series (ChaptersReceived chapter page)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        SetMedia media ->
            ( { model | media = media }, Cmd.none )

        SeriesReceived seriesData ->
            ( { model | series = seriesData }, Cmd.none )

        ChaptersReceived chapterId page chapterData ->
            ( { model | location = RemoteData.andThen (ReaderLocation.locationOr404 chapterId page) chapterData }, Cmd.none )

        PrevPage ->
            RemoteData.unwrap
                noop
                (\location ->
                    ( model
                    , Route.pushUrl
                        model.navKey
                        (Route.Reader
                            location.chapter.seriesId
                            location.chapter.id
                            location.page
                        )
                    )
                )
                model.location

        PrevChapter ->
            Debug.todo "branch 'PrevChapter' not implemented"

        NextPage ->
            Debug.todo "branch 'NextPage' not implemented"

        NextChapter ->
            Debug.todo "branch 'NextChapter' not implemented"

        JumpTo _ ->
            Debug.todo "branch 'JumpTo _' not implemented"
