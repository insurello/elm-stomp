module Stomp.Subscription
    exposing
        ( Subscription
        , AckMode(..)
        , init
        , onMessage
        , ackMode
        , map
        )

import Stomp.Internal.Callback exposing (Callback)


type alias Subscription msg =
    Sub msg


type alias Sub msg =
    { id : String
    , destination : String
    , onMessage : Maybe (Callback msg)
    , ack : AckMode
    }


type AckMode
    = AutoAck
    | ClientAck
    | ClientIndividualAck


init : String -> Subscription msg
init destination =
    { id = destination
    , destination = destination
    , onMessage = Nothing
    , ack = AutoAck
    }


onMessage : Callback msg -> Subscription msg -> Subscription msg
onMessage callback subscription =
    { subscription | onMessage = Just callback }


ackMode : AckMode -> Subscription msg -> Subscription msg
ackMode ack subscription =
    { subscription | ack = ack }


map : (a -> b) -> Subscription a -> Subscription b
map func sub =
    let
        mapCallback func callback =
            (\a -> func (callback a))
    in
        { sub | onMessage = Maybe.map (mapCallback func) sub.onMessage }
