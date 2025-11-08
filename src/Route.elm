module Route exposing (Route(..), parseUrl, pushUrl, urlForRoute)

import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId, formatId, idParser)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (..)


type Route
    = NotFound
    | AllSeries
    | Series ApiId
    | Chapter ApiId ApiId
    | Reader ApiId ApiId Int


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map AllSeries top
        , map Series (s "series" </> idParser)
        , map Chapter (s "series" </> idParser </> s "chapters" </> idParser)
        , map Reader (s "series" </> idParser </> s "chapters" </> idParser </> s "page" </> int)
        ]


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl navKey route =
    urlForRoute route |> Nav.pushUrl navKey


urlForRoute : Route -> String
urlForRoute route =
    case route of
        NotFound ->
            "/not-found"

        AllSeries ->
            "/"

        Series seriesId ->
            "/series/" ++ Data.ApiId.formatId seriesId

        Chapter seriesId chapterId ->
            Url.Builder.absolute
                [ "series"
                , formatId seriesId
                , "chapters"
                , formatId chapterId
                ]
                []

        Reader seriesId chapterId pageNumber ->
            Url.Builder.absolute
                [ "series"
                , formatId seriesId
                , "chapters"
                , formatId chapterId
                , "page"
                , String.fromInt pageNumber
                ]
                []
