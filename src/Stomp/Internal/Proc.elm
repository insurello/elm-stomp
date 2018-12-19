module Stomp.Internal.Proc exposing (CorrelationId, Proc, call, map)

import Json.Encode exposing (Value)
import Stomp.Internal.Body as Body
import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Frame exposing (Frame, Header, frame)


type alias Proc msg =
    { cmd : String
    , headers : List Header
    , body : Maybe Value
    , onResponse : Maybe (Callback msg)
    }


type alias CorrelationId =
    String


call : Proc msg -> CorrelationId -> Frame
call proc id =
    let
        headers =
            [ ( "destination", proc.cmd )
            , ( "reply-to", "/temp-queue/proc" )
            , ( "content-encoding", "utf8" )
            , ( "content-type", "application/json" )
            , ( "amqp-message-id", id )
            ]
                ++ proc.headers
    in
    frame "SEND" headers (Body.encode proc.body)


map : (a -> b) -> Proc a -> Proc b
map func proc =
    let
        mapCallback func_ callback =
            \a -> func_ (callback a)
    in
    { cmd = proc.cmd
    , headers = proc.headers
    , body = proc.body
    , onResponse = Maybe.map (mapCallback func) proc.onResponse
    }
