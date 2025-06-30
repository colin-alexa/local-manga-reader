module Data.Post exposing (Post, PostId, formatId, idParser, postDecoder, postEncoder, postsDecoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)


type PostId
    = PostId Int


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idEncoder : PostId -> Encode.Value
idEncoder (PostId id) =
    Encode.int id


formatId : PostId -> String
formatId (PostId id) =
    String.fromInt id


idParser : Parser (PostId -> b) b
idParser =
    custom "POSTID" <| \id -> Maybe.map PostId (String.toInt id)


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", idEncoder post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder
