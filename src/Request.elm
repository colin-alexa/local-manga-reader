module Request exposing (countChaptersForSeries, getAllSeries, getChapter, getChaptersForSeries, getPageAssetUrl, getSeries)

import Data.ApiId exposing (ApiId, formatId)
import Data.Chapter exposing (chapterDecoder, chaptersDecoder)
import Data.ReaderLocation exposing (ReaderLocation)
import Data.Series exposing (seriesDecoder)
import Dict
import Http
import Json.Decode
import RemoteData exposing (WebData)
import Url exposing (Protocol(..))
import Url.Builder as Builder exposing (QueryParameter)


assetHost : String
assetHost =
    "http://localhost:8080"


apiHost : String
apiHost =
    "http://localhost:5019"


type ApiResource
    = AllChapters ApiId
    | Chapter ApiId
    | AllSeries
    | Series ApiId


type ApiComparison
    = EQ
    | NE
    | LTE
    | GTE
    | LIKE


type ApiSortOrder
    = ASC
    | DESC


type ApiQuery
    = Filter String ApiComparison String
    | Search String
    | Slice Int Int
    | Sort String ApiSortOrder


formatQueryCompare : ApiComparison -> String -> String
formatQueryCompare comp field =
    case comp of
        EQ ->
            field

        NE ->
            field ++ "_ne"

        LTE ->
            field ++ "_lte"

        GTE ->
            field ++ "_gte"

        LIKE ->
            field ++ "_like"


formatQuerySortOrder : ApiSortOrder -> String
formatQuerySortOrder ord =
    case ord of
        ASC ->
            "asc"

        DESC ->
            "desc"


formatQuery : List ApiQuery -> List QueryParameter
formatQuery paramList =
    let
        formatSingleParam p =
            case p of
                Filter field compare value ->
                    [ Builder.string (formatQueryCompare compare field) value ]

                Search query ->
                    [ Builder.string "q" query ]

                Slice start end ->
                    [ Builder.int "_start" start, Builder.int "_end" end ]

                Sort field order ->
                    [ Builder.string "_sort" field, Builder.string "_order=" (formatQuerySortOrder order) ]
    in
    List.concatMap formatSingleParam paramList


apiResourceToUrl : ApiResource -> String
apiResourceToUrl resource =
    case resource of
        AllChapters seriesId ->
            Builder.crossOrigin apiHost [ "chapters" ] <| formatQuery [ Filter "seriesId" EQ (formatId seriesId) ]

        Chapter chapterId ->
            Builder.crossOrigin apiHost [ "chapters", formatId chapterId ] []

        AllSeries ->
            Builder.crossOrigin apiHost [ "series" ] []

        Series seriesId ->
            Builder.crossOrigin apiHost [ "series", formatId seriesId ] []


getApiResource : ApiResource -> Http.Expect msg -> Cmd msg
getApiResource resource expect =
    Http.get
        { url = apiResourceToUrl resource
        , expect = expect
        }


getAllSeries : (WebData (List Data.Series.Series) -> msg) -> Cmd msg
getAllSeries f =
    getApiResource
        AllSeries
        (Http.expectJson
            (f << RemoteData.fromResult)
            (Json.Decode.list seriesDecoder)
        )


getSeries : ApiId -> (WebData Data.Series.Series -> msg) -> Cmd msg
getSeries id f =
    getApiResource
        (Series id)
        (Http.expectJson
            (f << RemoteData.fromResult)
            seriesDecoder
        )


getChaptersForSeries : ApiId -> (WebData (List Data.Chapter.Chapter) -> msg) -> Cmd msg
getChaptersForSeries id f =
    getApiResource
        (AllChapters id)
        (Http.expectJson
            (f << RemoteData.fromResult)
            chaptersDecoder
        )


getChapter : ApiId -> (WebData Data.Chapter.Chapter -> msg) -> Cmd msg
getChapter id f =
    getApiResource
        (Chapter id)
        (Http.expectJson
            (f << RemoteData.fromResult)
            chapterDecoder
        )


getPageAssetUrl : { a | chapter : Data.Chapter.Chapter, page : Int } -> Maybe String
getPageAssetUrl { chapter, page } =
    if page < 1 || page > chapter.pageCount then
        Nothing

    else
        let
            chapterDir =
                String.fromInt chapter.number ++ " - " ++ chapter.name

            imageFileName =
                String.fromInt page ++ ".jpeg"
        in
        Just <| Builder.crossOrigin assetHost [ chapterDir, imageFileName ] []



-- Get count from HTTP Headers


type alias HttpResult a =
    Result Http.Error a


resultFromHttpResponse : (Http.Metadata -> b -> HttpResult a) -> Http.Response b -> HttpResult a
resultFromHttpResponse f response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.GoodStatus_ meta body ->
            f meta body


getJsonApiTotalCount : Http.Metadata -> a -> HttpResult Int
getJsonApiTotalCount { headers } _ =
    let
        maybeCount =
            Dict.get "x-total-count" headers
    in
    Result.fromMaybe (Http.BadStatus 400) (maybeCount |> Maybe.andThen String.toInt)


countChaptersForSeries : ApiId -> (HttpResult Int -> msg) -> Cmd msg
countChaptersForSeries seriesId f =
    Http.request
        { method = "HEAD"
        , headers = []
        , url =
            Builder.crossOrigin apiHost [ "chapters" ] <|
                formatQuery [ Filter "seriesId" EQ (formatId seriesId), Slice 0 10000 ]
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse f (resultFromHttpResponse getJsonApiTotalCount)
        , timeout = Nothing
        , tracker = Nothing
        }
