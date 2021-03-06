module Stomp.Subscription exposing
    ( Subscription, init
    , onMessage, expectJson
    , withSubscriptionId
    , autoAck, clientAck, clientIndividualAck
    , batch, none
    )

{-| A subscription on a topic.


# Subscriptions

@docs Subscription, init


# Response

@docs onMessage, expectJson


# Subscription Identifier

@docs withSubscriptionId


# Acknowledgement

@docs autoAck, clientAck, clientIndividualAck


# Batching

@docs batch, none

-}

import Json.Decode exposing (Decoder)
import Stomp.Internal.Batch exposing (Batch)
import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Subscription exposing (AckMode(..))
import Stomp.Message


{-| Describes a subscription.
-}
type alias Subscription msg =
    Batch (Stomp.Internal.Subscription.Sub msg)


{-| Construct a subscription on a specific topic.
-}
init : String -> Subscription msg
init destination =
    Stomp.Internal.Batch.singleton
        { id = destination
        , destination = destination
        , onMessage = Nothing
        , ack = AutoAck
        }


{-| Set a callback to be triggered when a message is received.
-}
onMessage : Callback msg -> Subscription msg -> Subscription msg
onMessage callback =
    Stomp.Internal.Batch.map
        (\subscription ->
            { subscription | onMessage = Just callback }
        )


{-| Set a callback to be triggered when a message is received and a JSON decoder to be used to decode the message body.
-}
expectJson : (Result String a -> msg) -> Decoder a -> Subscription msg -> Subscription msg
expectJson callback decoder =
    let
        decodeMessage message =
            message
                |> Result.andThen
                    (\msg ->
                        case Stomp.Message.payload decoder msg of
                            Ok payload ->
                                Ok payload

                            Err error ->
                                Err error
                    )
    in
    onMessage (\a -> callback (decodeMessage a))


{-| Set the message acknowledgment mode to "auto" (the default).

When the ack mode is "auto", then the client does not need to send the server
acknowledgements for the messages it receives. The server will assume the
client has received the message as soon as it sends it to the client. This
acknowledgment mode can cause messages being transmitted to the client to get
dropped.

-}
autoAck : Subscription msg -> Subscription msg
autoAck =
    Stomp.Internal.Batch.map
        (\subscription ->
            { subscription | ack = AutoAck }
        )


{-| Set the message acknowledgment mode to "client".

When the ack mode is "client", then the client must send the server
acknowledgements for the messages it processes. If the connection fails before
a client sends an acknowledgement for the message the server will assume the
message has not been processed and may redeliver the message to another client.

-}
clientAck : Subscription msg -> Subscription msg
clientAck =
    Stomp.Internal.Batch.map
        (\subscription ->
            { subscription | ack = ClientAck }
        )


{-| Set the message acknowledgment mode to "client-individual".

When the ack mode is "client-individual", the acknowledgment operates just like
the "client" acknowledgment mode except that the `ack` or `nack` sent by the
client are not cumulative. This means that an `ack` or `nack` for a
subsequent message does not cause a previous message to get acknowledged.

-}
clientIndividualAck : Subscription msg -> Subscription msg
clientIndividualAck =
    Stomp.Internal.Batch.map
        (\subscription ->
            { subscription | ack = ClientIndividualAck }
        )


{-| Set the id of the subscription to differentiate between multiple
subscriptions to the same topic (default is to use the topic name).
-}
withSubscriptionId : String -> Subscription msg -> Subscription msg
withSubscriptionId id =
    Stomp.Internal.Batch.map
        (\subscription ->
            { subscription | id = id }
        )


{-| Batch multiple subscriptions together.
-}
batch : List (Subscription msg) -> Subscription msg
batch =
    Stomp.Internal.Batch.batch


{-| Return a subscription that does nothing.
-}
none : Subscription msg
none =
    Stomp.Internal.Batch.none
