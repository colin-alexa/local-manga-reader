module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Data.ApiId exposing (ApiId)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Chapters
    | Chapter ApiId


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
        [ map Chapters top
        , map Chapters (s "chapters")
        , map Chapter (s "chapters" </> Data.ApiId.idParser)
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Chapters ->
            "/chapters"

        Chapter id ->
            "/chapters/" ++ Data.ApiId.formatId id
