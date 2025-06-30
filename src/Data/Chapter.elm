module Data.Chapter exposing (..)

import Data.ApiId exposing (ApiId, idDecoder)
import Json.Decode as Decode exposing (Decoder, int, list, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)


type alias Chapter =
    { id : ApiId
    , seriesId : ApiId
    , number : Int
    , name : String
    , pageCount : Int
    , volumeNumber : Maybe Int
    , summary : Maybe String
    }


chapterDecoder : Decoder Chapter
chapterDecoder =
    Decode.succeed Chapter
        |> required "id" idDecoder
        |> required "seriesId" idDecoder
        |> required "number" int
        |> required "name" string
        |> required "pageCount" int
        |> optional "volumeNumber" (maybe int) Nothing
        |> optional "summary" (maybe string) Nothing


chaptersDecoder : Decoder (List Chapter)
chaptersDecoder =
    list chapterDecoder
