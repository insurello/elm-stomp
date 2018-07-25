module Stomp.Subscription
    exposing
        ( Subscription
        , init
        , onMessage
        , autoAck
        , clientAck
        , clientIndividualAck
        , withSubscriptionId
        )

{-| A subscription on a topic.


# Subscriptions

@docs Subscription, init


# Response

@docs onMessage


# Subscription Identifier

@docs withSubscriptionId


# Acknowledgement

@docs autoAck, clientAck, clientIndividualAck

-}

import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Subscription exposing (AckMode(..))


{-| Describes a subscription.
-}
type alias Subscription msg =
    Stomp.Internal.Subscription.Sub msg


{-| Construct a subscription on a specific topic.
-}
init : String -> Subscription msg
init destination =
    { id = destination
    , destination = destination
    , onMessage = Nothing
    , ack = AutoAck
    }


{-| Set a callback to be triggered when a message is received.
-}
onMessage : Callback msg -> Subscription msg -> Subscription msg
onMessage callback subscription =
    { subscription | onMessage = Just callback }


{-| Set the message acknowledgment mode to "auto" (the default).

When the ack mode is "auto", then the client does not need to send the server
acknowledgements for the messages it receives. The server will assume the
client has received the message as soon as it sends it to the client. This
acknowledgment mode can cause messages being transmitted to the client to get
dropped.

-}
autoAck : Subscription msg -> Subscription msg
autoAck subscription =
    { subscription | ack = AutoAck }


{-| Set the message acknowledgment mode to "client".

When the ack mode is "client", then the client must send the server
acknowledgements for the messages it processes. If the connection fails before
a client sends an acknowledgement for the message the server will assume the
message has not been processed and may redeliver the message to another client.

-}
clientAck : Subscription msg -> Subscription msg
clientAck subscription =
    { subscription | ack = ClientAck }


{-| Set the message acknowledgment mode to "client-individual".

When the ack mode is "client-individual", the acknowledgment operates just like
the "client" acknowledgment mode except that the `ack` or `nack` sent by the
client are not cumulative. This means that an `ack` or `nack` for a
subsequent message does not cause a previous message to get acknowledged.

-}
clientIndividualAck : Subscription msg -> Subscription msg
clientIndividualAck subscription =
    { subscription | ack = ClientIndividualAck }


{-| Set the id of the subscription to differentiate between multiple
subscriptions to the same topic (default is to use the topic name).
-}
withSubscriptionId : String -> Subscription msg -> Subscription msg
withSubscriptionId id subscription =
    { subscription | id = id }
