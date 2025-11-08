module Data.Series exposing (..)

import Data.ApiId exposing (ApiId, idDecoder)
import Json.Decode as Decode exposing (Decoder, maybe, string)
import Json.Decode.Pipeline exposing (required)


type alias Series =
    { id : ApiId
    , name : String
    , author : Maybe String
    , infoLink : Maybe String
    }


seriesDecoder : Decoder Series
seriesDecoder =
    Decode.succeed Series
        |> required "id" idDecoder
        |> required "name" string
        |> required "author" (maybe string)
        |> required "info_link" (maybe string)
