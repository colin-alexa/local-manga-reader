module Request exposing (getAsset, getChapter, getChaptersForSeries, getSeries)

import Data.ApiId exposing (ApiId, formatId)
import Data.Chapter exposing (chapterDecoder, chaptersDecoder)
import Data.Series
import Http
import Json.Decode
import RemoteData exposing (WebData)
import Url exposing (Protocol(..))


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


formatQuery : ApiQuery -> String
formatQuery param =
    case param of
        Filter field compare value ->
            formatQueryCompare compare field ++ "=" ++ value

        Search query ->
            "q=" ++ query

        Slice start end ->
            "_start=" ++ String.fromInt start ++ "&_end=" ++ String.fromInt end

        Sort field order ->
            "_sort=" ++ field ++ "&_order=" ++ formatQuerySortOrder order


formatQueryParams : List ApiQuery -> String
formatQueryParams queries =
    case queries of
        [] ->
            ""

        _ ->
            "?" ++ (String.join "&" <| List.map formatQuery queries)


resourceToPath : ApiResource -> String
resourceToPath resource =
    case resource of
        AllChapters seriesId ->
            "/chapters" ++ formatQueryParams [ Filter "seriesId" EQ (formatId seriesId) ]

        Chapter chapterId ->
            "/chapters/" ++ formatId chapterId

        AllSeries ->
            "/series"

        Series seriesId ->
            "/series/" ++ formatId seriesId


getAsset : String -> Http.Expect msg -> Cmd msg
getAsset path expect =
    Http.get
        { url = assetHost ++ path
        , expect = expect
        }


getApiResource : ApiResource -> Http.Expect msg -> Cmd msg
getApiResource resource expect =
    Http.get
        { url = apiHost ++ resourceToPath resource
        , expect = expect
        }


dangerousHead : List a -> a
dangerousHead xs =
    case xs of
        [] ->
            Debug.todo "It's called dangerous for a reason"

        x :: _ ->
            x


getSeries : (WebData Data.Series.Series -> msg) -> Cmd msg
getSeries f =
    getApiResource
        AllSeries
        (Http.expectJson
            (f
                << RemoteData.fromResult
                << Result.map dangerousHead
            )
            (Json.Decode.list Data.Series.seriesDecoder)
        )


getChapter : ApiId -> (WebData Data.Chapter.Chapter -> msg) -> Cmd msg
getChapter id f =
    getApiResource
        (Chapter id)
        (Http.expectJson
            (f << RemoteData.fromResult)
            chapterDecoder
        )


getChaptersForSeries : ApiId -> (WebData (List Data.Chapter.Chapter) -> msg) -> Cmd msg
getChaptersForSeries id f =
    getApiResource
        (AllChapters id)
        (Http.expectJson
            (f << RemoteData.fromResult)
            chaptersDecoder
        )
