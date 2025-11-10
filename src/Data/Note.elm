module Data.Note exposing (..)

import Data.ApiId exposing (ApiId, idDecoder, idEncoder)
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Note =
    { id : ApiId
    , text : String
    , date : String
    , tags : List String
    }


noteDecoder : Decoder Note
noteDecoder =
    Decode.succeed Note
        |> required "id" idDecoder
        |> required "text" string
        |> required "date" string
        |> required "tags" (list string)


notesDecoder : Decoder (List Note)
notesDecoder =
    list noteDecoder


noteEncoder : Note -> Encode.Value
noteEncoder note =
    Encode.object
        [ ( "id", idEncoder note.id )
        , ( "text", Encode.string note.text )
        , ( "date", Encode.string note.date )
        , ( "tags", Encode.list Encode.string note.tags )
        ]
