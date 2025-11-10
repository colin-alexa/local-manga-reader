module Page.Reader exposing (..)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter)
import Data.ReaderLocation as ReaderLocation exposing (ReaderLocation)
import Data.Series exposing (Series)
import Json.Decode
import MultiInput
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
    , allowKeyboardNavigation : Bool
    , tagSelect :
        { items : WebData (List String)
        , state : MultiInput.State
        }
    , allowFreeformTags : Bool
    }


type Msg
    = Noop
    | SetMedia Media
    | UpdateMedia
    | SetAllowKeyboardNavigation Bool
    | SeriesReceived (WebData Series)
    | ChaptersReceived ApiId Int (WebData (List Chapter))
    | TagsReceived (WebData (List String))
    | TagInputMsg MultiInput.Msg
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
      , allowKeyboardNavigation = True
      , allowFreeformTags = True
      , tagSelect =
            { items = RemoteData.NotAsked
            , state = MultiInput.init "tags"
            }
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

        SetAllowKeyboardNavigation state ->
            ( { model | allowKeyboardNavigation = state }, Cmd.none )

        SeriesReceived seriesData ->
            ( { model | series = seriesData }, Cmd.none )

        ChaptersReceived chapterId page chapterData ->
            let
                locationData =
                    RemoteData.andThen (ReaderLocation.locationOr404 chapterId page) chapterData
            in
            locationData
                |> RemoteData.unwrap (noop { model | location = locationData })
                    (\location ->
                        ( { model
                            | location = locationData
                            , tagSelect =
                                { items = RemoteData.Loading
                                , state = model.tagSelect.state
                                }
                          }
                        , Request.getTagsForLocation location TagsReceived
                        )
                    )

        TagsReceived tagsData ->
            ( { model | tagSelect = { items = tagsData, state = model.tagSelect.state } }, Cmd.none )

        TagInputMsg multiInputMsg ->
            model.tagSelect.items
                |> RemoteData.unwrap (noop model)
                    (\tags ->
                        let
                            ( newState, newTags, tagSelectCmd ) =
                                MultiInput.update { separators = [ "\n", "\t", "," ] } multiInputMsg model.tagSelect.state tags
                        in
                        ( { model
                            | tagSelect =
                                { items = RemoteData.Success newTags
                                , state = newState
                                }
                          }
                        , Cmd.map TagInputMsg tagSelectCmd
                        )
                    )

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


keypressDecoder : { a | allowKeyboardNavigation : Bool } -> Json.Decode.Decoder Msg
keypressDecoder model =
    if model.allowKeyboardNavigation then
        Json.Decode.map keyToMsg (Json.Decode.field "key" Json.Decode.string)

    else
        Json.Decode.succeed Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (keypressDecoder model)
        , onResize (\w h -> UpdateMedia)
        , Sub.map TagInputMsg <| MultiInput.subscriptions model.tagSelect.state
        ]
