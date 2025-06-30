module Page.AllSeries exposing (..)

import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId, idToInt)
import Data.Series
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import Request


type alias Series =
    { id : ApiId
    , name : String
    , author : Maybe String
    , infoLink : Maybe String
    , chapters : WebData Int
    }


type alias ApiSeries =
    Data.Series.Series


seriesFromApiSeries : WebData Int -> ApiSeries -> Series
seriesFromApiSeries chapters { id, name, author, infoLink } =
    { id = id
    , name = name
    , author = author
    , infoLink = infoLink
    , chapters = chapters
    }


type alias Model =
    { navKey : Nav.Key
    , allSeries : WebData (List Series)
    }


type Msg
    = AllSeriesReceived (WebData (List ApiSeries))
    | ChapterCountReceived Series (WebData Int)


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { navKey = key, allSeries = RemoteData.Loading }
    , Request.getAllSeries AllSeriesReceived
    )


loadingDict : List Series -> Dict Int (WebData Int)
loadingDict ls =
    Dict.fromList (List.map (\s -> ( idToInt s.id, RemoteData.Loading )) ls)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AllSeriesReceived (RemoteData.Success seriesData) ->
            let
                series =
                    List.map (seriesFromApiSeries RemoteData.Loading) seriesData
            in
            ( { model
                | allSeries = RemoteData.Success series
              }
            , Cmd.batch
                (List.map
                    (\s ->
                        Request.countChaptersForSeries s.id
                            (ChapterCountReceived s << RemoteData.fromResult)
                    )
                    series
                )
            )

        AllSeriesReceived seriesData ->
            let
                series =
                    RemoteData.map (List.map (seriesFromApiSeries RemoteData.Loading)) seriesData
            in
            ( { model | allSeries = series }, Cmd.none )

        ChapterCountReceived series countData ->
            ( { model
                | allSeries =
                    model.allSeries
                        |> RemoteData.map
                            (List.map
                                (\s ->
                                    if s == series then
                                        { s | chapters = countData }

                                    else
                                        s
                                )
                            )
              }
            , Cmd.none
            )
