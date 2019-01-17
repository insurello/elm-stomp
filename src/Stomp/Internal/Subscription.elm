module Stomp.Internal.Subscription exposing (AckMode(..), Sub, SubscriptionId, map, subscribe, unsubscribe)

import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Frame exposing (Frame, frame)


type alias SubscriptionId =
    String


type alias Sub msg =
    { id : SubscriptionId
    , destination : String
    , onMessage : Maybe (Callback msg)
    , ack : AckMode
    }


type AckMode
    = AutoAck
    | ClientAck
    | ClientIndividualAck


subscribe : Sub msg -> Frame
subscribe sub =
    let
        headers =
            [ ( "id", sub.id )
            , ( "destination", sub.destination )
            , ( "ack"
              , case sub.ack of
                    AutoAck ->
                        "auto"

                    ClientAck ->
                        "client"

                    ClientIndividualAck ->
                        "client-individual"
              )
            ]
    in
    frame "SUBSCRIBE" headers Nothing


unsubscribe : Sub msg -> Frame
unsubscribe sub =
    let
        headers =
            [ ( "id", sub.id ) ]
    in
    frame "UNSUBSCRIBE" headers Nothing


map : (a -> b) -> Sub a -> Sub b
map func sub =
    let
        mapCallback func_ callback =
            \a -> func_ (callback a)
    in
    { id = sub.id
    , destination = sub.destination
    , onMessage = Maybe.map (mapCallback func) sub.onMessage
    , ack = sub.ack
    }
