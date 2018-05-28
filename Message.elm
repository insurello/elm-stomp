module Stomp.Message exposing (Message, header, payload)

import Json.Decode exposing (Value, decodeValue)
import Stomp.Internal.Frame exposing (headerValue)
import Stomp.Internal.Message exposing (InternalMessage)


type alias Message =
    InternalMessage


header : String -> Message -> Maybe String
header name message =
    headerValue name message.headers


payload : Json.Decode.Decoder a -> Message -> Result String a
payload decoder message =
    case message.payload of
        Just value ->
            decodeValue decoder value

        Nothing ->
            Result.Err "Empty message body"
