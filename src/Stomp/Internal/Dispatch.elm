module Stomp.Internal.Dispatch exposing (dispatch)

import Dict
import Stomp.Internal.Frame exposing (ServerFrame)
import Stomp.Internal.Message exposing (InternalMessage)
import Stomp.Internal.Proc exposing (CorrelationId)
import Stomp.Internal.Session exposing (Session)
import Stomp.Internal.Subscription exposing (SubscriptionId)


dispatch : Session msg -> Result String ServerFrame -> msg
dispatch session serverFrame =
    case serverFrame of
        Ok (Stomp.Internal.Frame.Message headers body) ->
            case Stomp.Internal.Message.init headers body of
                Ok message ->
                    dispatchMessage session message

                Err error ->
                    session.options.onError error

        Ok (Stomp.Internal.Frame.Connected headers) ->
            session.options.onConnected

        Ok (Stomp.Internal.Frame.Receipt receiptId) ->
            session.options.onDisconnected

        Ok (Stomp.Internal.Frame.Error error) ->
            session.options.onError (Maybe.withDefault "error" error)

        Ok Stomp.Internal.Frame.HeartBeat ->
            session.options.onError "heartbeat"

        Err error ->
            session.options.onError error


dispatchMessage : Session msg -> InternalMessage -> msg
dispatchMessage session message =
    let
        correlationId =
            message.headers
                |> Stomp.Internal.Frame.headerValue "correlation-id"

        subscription =
            message.headers
                |> Stomp.Internal.Frame.headerValue "subscription"
    in
    case ( subscription, correlationId ) of
        ( Just "/temp-queue/proc", Just id ) ->
            dispatchCallback session id message

        ( Just sub, _ ) ->
            dispatchSubscription session sub message

        _ ->
            session.options.onError "message dispatch failed"


dispatchCallback : Session msg -> CorrelationId -> InternalMessage -> msg
dispatchCallback session correlationId message =
    case Dict.get correlationId session.callbacks of
        Nothing ->
            session.options.onError "unknown correlation id"

        Just callback ->
            callback (Ok message)


dispatchSubscription : Session msg -> SubscriptionId -> InternalMessage -> msg
dispatchSubscription session subscriptionId message =
    case Dict.get subscriptionId session.subscriptions of
        Nothing ->
            session.options.onError "unknown subscription id"

        Just callback ->
            callback (Ok message)
