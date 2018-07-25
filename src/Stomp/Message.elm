module Stomp.Message exposing (Message, header, payload)

{-| A message from the server. A server can send a message either as a response to a remote procedure call or because you created a subscription on a topic.


# Messages

@docs Message


# Message Content

@docs header, payload

-}

import Json.Decode exposing (Value, decodeValue)
import Stomp.Internal.Frame exposing (headerValue)
import Stomp.Internal.Message exposing (InternalMessage)


{-| A message from the server.
-}
type alias Message =
    InternalMessage


{-| Read the value of a message header.

    header "message-id" message

-}
header : String -> Message -> Maybe String
header name message =
    headerValue name message.headers


{-| Decode the payload content of a message.

    import Stomp.Message
    import Json.Decode exposing (list, string)

    getStrings : Stomp.Message.Message -> List String
    getStrings message =
        Stomp.Message.payload (list string) message

-}
payload : Json.Decode.Decoder a -> Message -> Result String a
payload decoder message =
    case message.payload of
        Just value ->
            decodeValue decoder value

        Nothing ->
            Result.Err "Empty message body"
