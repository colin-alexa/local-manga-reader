module Page.Reader exposing (..)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter)
import Data.ReaderLocation as ReaderLocation exposing (ReaderLocation)
import Data.Series exposing (Series)
import Json.Decode
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
    , textareaFocused : Bool
    }


type Msg
    = Noop
    | SetMedia Media
    | UpdateMedia
    | SetTextareaFocus Bool
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
      , textareaFocused = False
      }
    , Cmd.batch
        [ queryMedia
        , Request.getSeries series SeriesReceived
        , Request.getChaptersForSeries series (ChaptersReceived chapter page)
        ]
    )


updateLocation : (ReaderLocation -> ReaderLocation) -> Model -> ( Model, Cmd Msg )
updateLocation getNewLocation model =
    RemoteData.unwrap
        (noop model)
        (\location ->
            let
                newLocation =
                    getNewLocation location
            in
            ( { model | location = RemoteData.Success newLocation }
            , Route.pushUrl
                model.navKey
                (Route.Reader
                    newLocation.chapter.seriesId
                    newLocation.chapter.id
                    newLocation.page
                )
            )
        )
        model.location


noop : a -> ( a, Cmd Msg )
noop m =
    ( m, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            noop model

        SetMedia media ->
            ( { model | media = media }, Cmd.none )

        UpdateMedia ->
            ( model, queryMedia )

        SetTextareaFocus state ->
            ( { model | textareaFocused = state }, Cmd.none )

        SeriesReceived seriesData ->
            ( { model | series = seriesData }, Cmd.none )

        ChaptersReceived chapterId page chapterData ->
            ( { model | location = RemoteData.andThen (ReaderLocation.locationOr404 chapterId page) chapterData }, Cmd.none )

        PrevPage ->
            updateLocation ReaderLocation.previousPage model

        PrevChapter ->
            updateLocation ReaderLocation.previousChapter model

        NextPage ->
            updateLocation ReaderLocation.nextPage model

        NextChapter ->
            updateLocation ReaderLocation.nextChapter model

        JumpTo _ ->
            Debug.todo "branch 'JumpTo _' not implemented"


keyToMsg : String -> Msg
keyToMsg key =
    case key of
        "ArrowLeft" ->
            PrevPage

        "ArrowUp" ->
            PrevChapter

        "ArrowDown" ->
            NextChapter

        "ArrowRight" ->
            NextPage

        _ ->
            Noop


keypressDecoder : { a | textareaFocused : Bool } -> Json.Decode.Decoder Msg
keypressDecoder model =
    if model.textareaFocused then
        Json.Decode.succeed Noop

    else
        Json.Decode.map keyToMsg (Json.Decode.field "key" Json.Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (keypressDecoder model)
        , onResize (\w h -> UpdateMedia)
        ]
