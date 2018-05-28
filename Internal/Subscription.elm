module Stomp.Internal.Subscription exposing (..)

import Stomp.Internal.Frame exposing (Frame, frame)
import Stomp.Subscription exposing (Subscription)


subscribe : Subscription msg -> Frame
subscribe sub =
    let
        headers =
            [ ( "id", sub.id )
            , ( "destination", sub.destination )
            , ( "ack"
              , case sub.ack of
                    Stomp.Subscription.AutoAck ->
                        "auto"

                    Stomp.Subscription.ClientAck ->
                        "client"

                    Stomp.Subscription.ClientIndividualAck ->
                        "client-individual"
              )
            ]
    in
        frame "SUBSCRIBE" headers Nothing


unsubscribe : Subscription msg -> Frame
unsubscribe sub =
    let
        headers =
            [ ( "id", sub.id ) ]
    in
        frame "UNSUBSCRIBE" headers Nothing
