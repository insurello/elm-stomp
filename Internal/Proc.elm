module Stomp.Internal.Proc exposing (..)

import Stomp.Internal.Body as Body
import Stomp.Internal.Frame exposing (Frame, frame)
import Stomp.Proc exposing (RemoteProcedure)


type alias CorrelationId =
    String


call : RemoteProcedure msg -> CorrelationId -> Frame
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
