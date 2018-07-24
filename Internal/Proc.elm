module Stomp.Internal.Proc exposing (..)

import Json.Encode exposing (Value)
import Stomp.Internal.Body as Body
import Stomp.Internal.Frame exposing (Header, Frame, frame)
import Stomp.Internal.Callback exposing (Callback)


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
        replyTo =
            "/temp-queue/" ++ proc.cmd

        headers =
            [ ( "destination", "/queue/" ++ proc.cmd )
            , ( "reply-to", replyTo )
            , ( "content-type", "application/json" )
            , ( "correlation-id", id )
            ]
                ++ proc.headers
    in
        frame "SEND" headers (Body.encode proc.body)


map : (a -> b) -> Proc a -> Proc b
map func proc =
    let
        mapCallback func callback =
            (\a -> func (callback a))
    in
        { proc | onResponse = Maybe.map (mapCallback func) proc.onResponse }
