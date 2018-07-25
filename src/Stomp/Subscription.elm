module Stomp.Subscription
    exposing
        ( Subscription
        , AckMode(..)
        , init
        , onMessage
        , ackMode
        , withSubscriptionId
        , map
        )

{-| A subscription on a topic.


# Subscriptions

@docs Subscription, init


# Response

@docs onMessage


# Options

@docs AckMode, ackMode, withSubscriptionId


# Internal

@docs map

-}

import Stomp.Internal.Callback exposing (Callback)


{-| Describes a subscription.
-}
type alias Subscription msg =
    Sub msg


type alias Sub msg =
    { id : String
    , destination : String
    , onMessage : Maybe (Callback msg)
    , ack : AckMode
    }


{-| The message acknowledgement mode to use. `AutoAck` (default), `ClientAck` (needs to manually acknowledge receiept) or `ClientIndividualAck` (needs to manually acknowledge each individual message).
-}
type AckMode
    = AutoAck
    | ClientAck
    | ClientIndividualAck


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


{-| Set the acknowledge mode to use (default is `AutoAck`).
-}
ackMode : AckMode -> Subscription msg -> Subscription msg
ackMode ack subscription =
    { subscription | ack = ack }


{-| Set the id of the subscription to differentiate between multiple
subscriptions to the same topic (default is to use the topic name).
-}
withSubscriptionId : String -> Subscription msg -> Subscription msg
withSubscriptionId id subscription =
    { subscription | id = id }


{-| -}
map : (a -> b) -> Subscription a -> Subscription b
map func sub =
    let
        mapCallback func callback =
            (\a -> func (callback a))
    in
        { sub | onMessage = Maybe.map (mapCallback func) sub.onMessage }
