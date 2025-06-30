module Data.ApiId exposing (..)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)


type ApiId
    = ApiId Int


idDecoder : Decoder ApiId
idDecoder =
    Decode.map ApiId int


idEncoder : ApiId -> Encode.Value
idEncoder (ApiId id) =
    Encode.int id


formatId : ApiId -> String
formatId (ApiId id) =
    String.fromInt id


idParser : Parser (ApiId -> b) b
idParser =
    custom "API_ID" <| \id -> Maybe.map ApiId (String.toInt id)


idToInt : ApiId -> Int
idToInt (ApiId id) =
    id
