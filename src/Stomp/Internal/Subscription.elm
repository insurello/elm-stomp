module Stomp.Internal.Subscription exposing (..)

import Stomp.Internal.Frame exposing (Frame, frame)
import Stomp.Internal.Callback exposing (Callback)


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
        mapCallback func callback =
            (\a -> func (callback a))
    in
        { sub | onMessage = Maybe.map (mapCallback func) sub.onMessage }
