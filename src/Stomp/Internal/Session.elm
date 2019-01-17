module Stomp.Internal.Session exposing
    ( Options
    , Session
    , init
    , insertCallback
    , insertSubscription
    , removeCallback
    , removeSubscription
    )

import Dict exposing (Dict)
import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Connection exposing (Connection)
import Stomp.Internal.Frame exposing (Frame, frame)
import Stomp.Internal.Proc exposing (CorrelationId, Proc)
import Stomp.Internal.Subscription exposing (Sub, SubscriptionId)


type alias Session msg =
    { connection : Connection msg
    , options : Options msg
    , callbacks : Dict CorrelationId (Callback msg)
    , subscriptions : Dict SubscriptionId (Callback msg)
    , nextId : Int
    }


type alias Options msg =
    { onConnected : msg
    , onDisconnected : msg
    , onError : String -> msg
    , onHeartBeat : msg
    }


init : Connection msg -> Options msg -> Session msg
init connection options =
    Session connection options Dict.empty Dict.empty 1


insertCallback : Proc msg -> Session msg -> Session msg
insertCallback proc session =
    case proc.onResponse of
        Just callback ->
            let
                correlationId =
                    session.nextId |> String.fromInt

                newCallbacks =
                    Dict.insert correlationId callback session.callbacks
            in
            { session
                | callbacks = newCallbacks
                , nextId = session.nextId + 1
            }

        Nothing ->
            session


removeCallback : CorrelationId -> Session msg -> Session msg
removeCallback id session =
    { session | callbacks = Dict.remove id session.callbacks }


insertSubscription : Sub msg -> Session msg -> Session msg
insertSubscription sub session =
    case sub.onMessage of
        Just callback ->
            let
                newSubscriptions =
                    Dict.insert sub.id callback session.subscriptions
            in
            { session | subscriptions = newSubscriptions }

        Nothing ->
            session


removeSubscription : Sub msg -> Session msg -> Session msg
removeSubscription sub session =
    { session | subscriptions = Dict.remove sub.id session.subscriptions }
