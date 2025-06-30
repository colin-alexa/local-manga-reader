module Page.Series exposing (..)

import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter, chaptersDecoder)
import Data.Series exposing (Series, seriesDecoder)
import Http
import RemoteData exposing (WebData)
import Request exposing (getChaptersForSeries)


type alias Model =
    { navKey : Nav.Key
    , series : WebData Series
    , chapters : WebData (List Chapter)
    }


type Msg
    = SeriesReceived (WebData Series)
    | ChaptersReceived (WebData (List Chapter))


init : Nav.Key -> ApiId -> ( Model, Cmd Msg )
init navKey seriesId =
    ( { navKey = navKey
      , series = RemoteData.Loading
      , chapters = RemoteData.NotAsked
      }
    , Request.getSeries seriesId SeriesReceived
    )


fetchSeries : ApiId -> Cmd Msg
fetchSeries id =
    getChaptersForSeries id ChaptersReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeriesReceived response ->
            case response of
                RemoteData.Success series ->
                    ( { model
                        | series = response
                        , chapters = RemoteData.Loading
                      }
                    , getChaptersForSeries series.id ChaptersReceived
                    )

                _ ->
                    ( { model | series = response }, Cmd.none )

        ChaptersReceived response ->
            ( { model | chapters = response }, Cmd.none )
