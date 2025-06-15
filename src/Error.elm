module Error exposing (..)

import Http


formatHttpError : Http.Error -> String
formatHttpError err =
    case err of
        Http.Timeout ->
            "Server is taking too long to respond, try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus code ->
            "Request failed with code: " ++ String.fromInt code

        Http.BadUrl message ->
            message

        Http.BadBody message ->
            message
