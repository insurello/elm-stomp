effect module Stomp.Client
    where { command = InternalCmd, subscription = InternalSub }
    exposing
        ( listen
        , connect
        , disconnect
        , send
        , call
        , subscribe
        , unsubscribe
        , ack
        , nack
        , begin
        , commit
        , abort
        )

{-| A client manages the connection with the server and is used to send commands to the server.


# Connection

@docs listen, connect, disconnect


# Commands

@docs send, call, subscribe, unsubscribe


# Acknowledgement

@docs ack, nack


# Transactions

@docs begin, commit, abort

-}

import WebSocket.LowLevel as WebSocket exposing (WebSocket, BadOpen)
import Task exposing (Task)
import Platform
import Process
import Dict exposing (Dict)
import Json.Encode
import Debug
import Stomp.Internal.Batch
import Stomp.Internal.Body as Body
import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Frame exposing (ServerFrame, Frame, Header, frame)
import Stomp.Internal.Message
import Stomp.Internal.Socket
import Stomp.Internal.Session
import Stomp.Internal.Subscription
import Stomp.Internal.Proc
import Stomp.Subscription exposing (Subscription, AckMode)
import Stomp.Proc exposing (RemoteProcedure)
import Stomp.Message exposing (Message)


type alias Endpoint =
    String


type alias Options msg =
    Stomp.Internal.Session.Options msg


type InternalSub msg
    = SocketConnect Endpoint


subMap : (a -> b) -> InternalSub a -> InternalSub b
subMap func sub =
    case sub of
        SocketConnect endpoint ->
            SocketConnect endpoint


type InternalCmd msg
    = Send Endpoint Frame
    | Connect Endpoint (Stomp.Internal.Session.Options msg)
    | Disconnect Endpoint
    | Subscribe Endpoint (Stomp.Subscription.Subscription msg)
    | Unsubscribe Endpoint (Stomp.Subscription.Subscription msg)
    | Call Endpoint (Stomp.Internal.Proc.Proc msg)


cmdMap : (a -> b) -> InternalCmd a -> InternalCmd b
cmdMap func cmd =
    case cmd of
        Send endpoint body ->
            Send endpoint body

        Connect endpoint options ->
            Connect endpoint (Stomp.Internal.Session.map func options)

        Disconnect endpoint ->
            Disconnect endpoint

        Subscribe endpoint sub ->
            Subscribe endpoint (Stomp.Subscription.map func sub)

        Unsubscribe endpoint sub ->
            Unsubscribe endpoint (Stomp.Subscription.map func sub)

        Call endpoint proc ->
            Call endpoint (Stomp.Internal.Proc.map func proc)


type Msg msg
    = GoodOpen Endpoint WebSocket
    | BadOpen Endpoint BadOpen
    | Die Endpoint { code : Int, reason : String, wasClean : Bool }
    | Receive Endpoint ServerFrame
    | InvalidFrame Endpoint String


type alias State msg =
    { sockets : Dict Endpoint Stomp.Internal.Socket.InternalSocket
    , sessions : Dict Endpoint (Stomp.Internal.Session.Session msg)
    , subscriptions : Dict Endpoint (Dict String (Callback msg))
    , callbacks : Dict Stomp.Internal.Proc.CorrelationId (Callback msg)
    , nextId : Int
    }


{-| Open a managed connection to the server.

**Note:* If the connection goes down, the effect manager tries to reconnect
with an exponential backoff strategy. When the connection is reestablished the client will attempt to restore the previous session by automatically authenticating with the credentials from the previous session and automatically recreate all topic subscriptions.

-}
listen : Endpoint -> Sub msg
listen server =
    subscription (SocketConnect server)


attemptOpen :
    Platform.Router msg (Msg msg)
    -> Float
    -> Endpoint
    -> Task x Process.Id
attemptOpen router backoff endpoint =
    let
        goodOpen ws =
            Platform.sendToSelf router (GoodOpen endpoint ws)

        badOpen details =
            Platform.sendToSelf router (BadOpen endpoint details)

        actuallyAttemptOpen =
            open router endpoint
                |> Task.andThen goodOpen
                |> Task.onError badOpen

        after backoff =
            if backoff < 1 then
                Task.succeed ()
            else
                Process.sleep backoff
    in
        Process.spawn
            (after backoff
                |> Task.andThen (\_ -> actuallyAttemptOpen)
            )


open : Platform.Router msg (Msg msg) -> Endpoint -> Task BadOpen WebSocket
open router endpoint =
    let
        settings =
            { onMessage =
                \_ frame ->
                    case Stomp.Internal.Frame.decode frame of
                        Ok frame ->
                            Platform.sendToSelf router (Receive endpoint frame)

                        Err error ->
                            Platform.sendToSelf router (InvalidFrame endpoint error)
            , onClose =
                \details ->
                    Platform.sendToSelf router (Die endpoint details)
            }
    in
        WebSocket.open endpoint settings


init : Task Never (State msg)
init =
    Task.succeed (State Dict.empty Dict.empty Dict.empty Dict.empty 1)


onEffects :
    Platform.Router msg (Msg msg)
    -> List (InternalCmd msg)
    -> List (InternalSub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds subs state =
    let
        definedSockets =
            let
                insert sub dict =
                    case sub of
                        SocketConnect endpoint ->
                            Dict.insert endpoint Stomp.Internal.Socket.internalSocket dict
            in
                List.foldl insert Dict.empty subs

        handleSocketsUpdate sockets =
            Dict.merge
                addedSockets
                retainedSockets
                removedSockets
                definedSockets
                sockets
                (Task.succeed Dict.empty)

        addedSockets endpoint socket taskChain =
            taskChain
                |> Task.andThen
                    (\sockets ->
                        attemptOpen router 0 endpoint
                            |> Task.andThen
                                (\pid ->
                                    let
                                        socket =
                                            Stomp.Internal.Socket.internalSocket
                                                |> Stomp.Internal.Socket.opening 0 pid
                                    in
                                        Task.succeed (Dict.insert endpoint socket sockets)
                                )
                    )

        retainedSockets endpoint socketNew socketOld taskChain =
            let
                socket =
                    Stomp.Internal.Socket.update socketNew socketOld
            in
                taskChain |> Task.map (Dict.insert endpoint socket)

        removedSockets endpoint socket taskChain =
            Stomp.Internal.Socket.close socket |> Task.andThen (\_ -> taskChain)
    in
        processCommands cmds state
            |> Task.andThen
                (\state ->
                    handleSocketsUpdate state.sockets
                        |> Task.map (\sockets -> { state | sockets = sockets })
                )


processCommands : List (InternalCmd msg) -> State msg -> Task Never (State msg)
processCommands cmds state =
    case cmds of
        [] ->
            Task.succeed state

        (Send endpoint frame) :: rest ->
            sendFrame endpoint frame state
                |> Task.andThen (processCommands rest)

        (Connect endpoint options) :: rest ->
            sendFrame endpoint (Stomp.Internal.Session.connect options) state
                |> Task.map (insertSession endpoint (Stomp.Internal.Session.init options))
                |> Task.andThen (processCommands rest)

        (Disconnect endpoint) :: rest ->
            sendFrame endpoint (Stomp.Internal.Session.disconnect) state
                |> Task.andThen (processCommands rest)

        (Subscribe endpoint sub) :: rest ->
            sendFrame endpoint (Stomp.Internal.Subscription.subscribe sub) state
                |> Task.map (insertSubscription endpoint sub)
                |> Task.andThen (processCommands rest)

        (Unsubscribe endpoint sub) :: rest ->
            sendFrame endpoint (Stomp.Internal.Subscription.unsubscribe sub) state
                |> Task.andThen (processCommands rest)

        (Call endpoint proc) :: rest ->
            let
                correlationId =
                    state.nextId |> toString

                frame =
                    Stomp.Internal.Proc.call proc correlationId
            in
                sendFrame endpoint frame state
                    |> Task.map (insertCallback proc)
                    |> Task.andThen (processCommands rest)


sendFrame : Endpoint -> Frame -> State msg -> Task Never (State msg)
sendFrame endpoint frame state =
    case Dict.get endpoint state.sockets of
        Nothing ->
            Task.succeed state

        Just socket ->
            frame
                |> Stomp.Internal.Frame.encode
                |> Stomp.Internal.Socket.send socket
                |> Task.andThen
                    (\maybeBadSend ->
                        case maybeBadSend of
                            Nothing ->
                                Task.succeed state

                            Just badSend ->
                                Task.succeed state
                    )


insertSession :
    Endpoint
    -> Stomp.Internal.Session.Session msg
    -> State msg
    -> State msg
insertSession endpoint session state =
    { state | sessions = Dict.insert endpoint session state.sessions }


insertSubscription : Endpoint -> Subscription msg -> State msg -> State msg
insertSubscription endpoint sub state =
    case sub.onMessage of
        Just callback ->
            let
                update =
                    case Dict.get endpoint state.subscriptions of
                        Just subs ->
                            Dict.insert sub.id callback subs

                        Nothing ->
                            Dict.insert sub.id callback Dict.empty
            in
                { state
                    | subscriptions =
                        Dict.insert endpoint update state.subscriptions
                }

        Nothing ->
            state


insertCallback : Stomp.Internal.Proc.Proc msg -> State msg -> State msg
insertCallback proc state =
    case proc.onResponse of
        Just callback ->
            let
                correlationId =
                    state.nextId |> toString

                newCallbacks =
                    Dict.insert correlationId callback state.callbacks
            in
                { state
                    | callbacks = newCallbacks
                    , nextId = state.nextId + 1
                }

        Nothing ->
            state


onSelfMsg :
    Platform.Router msg (Msg msg)
    -> Msg msg
    -> State msg
    -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        GoodOpen endpoint ws ->
            case Dict.get endpoint state.sockets of
                Nothing ->
                    Task.succeed state

                Just internalSocket ->
                    let
                        newSocket =
                            Stomp.Internal.Socket.connected ws internalSocket

                        newState =
                            { state
                                | sockets =
                                    Dict.insert endpoint newSocket state.sockets
                            }

                        restartSession state =
                            case Dict.get endpoint state.sessions of
                                Just session ->
                                    let
                                        frame =
                                            Stomp.Internal.Session.connect session.options
                                    in
                                        sendFrame endpoint frame state

                                Nothing ->
                                    Task.succeed state
                    in
                        Task.succeed newState
                            |> Task.andThen restartSession

        BadOpen endpoint details ->
            case Dict.get endpoint state.sockets of
                Nothing ->
                    Task.succeed state

                Just internalSocket ->
                    let
                        backoff =
                            (Stomp.Internal.Socket.backoffIteration internalSocket) + 1

                        newSocket pid =
                            Stomp.Internal.Socket.opening backoff pid internalSocket

                        newState pid =
                            { state
                                | sockets =
                                    Dict.insert endpoint (newSocket pid) state.sockets
                            }
                    in
                        attemptOpen router (reconnectTimer backoff) endpoint
                            |> Task.map newState

        Die endpoint details ->
            case Dict.get endpoint state.sockets of
                Nothing ->
                    Task.succeed state

                Just internalSocket ->
                    let
                        backoff =
                            (Stomp.Internal.Socket.backoffIteration internalSocket) + 1

                        newSocket pid =
                            Stomp.Internal.Socket.opening backoff pid internalSocket

                        newState pid =
                            { state
                                | sockets =
                                    Dict.insert endpoint (newSocket pid) state.sockets
                            }
                    in
                        attemptOpen router (reconnectTimer backoff) endpoint
                            |> Task.map newState

        Receive endpoint frame ->
            case frame of
                Stomp.Internal.Frame.Connected headers ->
                    let
                        newSessions =
                            case Dict.get endpoint state.sessions of
                                Just session ->
                                    let
                                        newSession =
                                            Stomp.Internal.Session.connected session
                                    in
                                        Dict.insert endpoint newSession state.sessions

                                Nothing ->
                                    state.sessions

                        newState =
                            { state | sessions = newSessions }
                    in
                        Task.succeed newState
                            |> Task.andThen (notifySession router endpoint .onConnected)

                Stomp.Internal.Frame.Message headers body ->
                    dispatch router endpoint headers body state

                Stomp.Internal.Frame.Receipt receiptId ->
                    case receiptId of
                        "DISCONNECT" ->
                            Dict.get endpoint state.sockets
                                |> Maybe.map Stomp.Internal.Socket.close
                                |> Maybe.withDefault (Task.succeed ())
                                |> Task.map (\_ -> state)
                                |> Task.andThen (notifySession router endpoint .onDisconnected)

                        _ ->
                            Task.succeed state

                Stomp.Internal.Frame.Error error ->
                    case error of
                        Just error ->
                            notifySession router endpoint (\s -> s.onError error) state

                        Nothing ->
                            Task.succeed state

                Stomp.Internal.Frame.HeartBeat ->
                    Task.succeed state

        InvalidFrame endpoint error ->
            let
                _ =
                    Debug.log "invalid frame" error
            in
                Task.succeed state


reconnectTimer : Int -> Float
reconnectTimer failedAttempts =
    if failedAttempts < 1 then
        0
    else
        toFloat (10 * 2 ^ failedAttempts)


notifySession :
    Platform.Router msg (Msg msg)
    -> Endpoint
    -> (Stomp.Internal.Session.Options msg -> msg)
    -> State msg
    -> Task x (State msg)
notifySession router endpoint func state =
    Dict.get endpoint state.sessions
        |> Maybe.map (\session -> func session.options)
        |> Maybe.map (Platform.sendToApp router)
        |> Maybe.withDefault (Task.succeed ())
        |> Task.map (\_ -> state)


dispatch :
    Platform.Router msg (Msg msg)
    -> Endpoint
    -> List Header
    -> Maybe String
    -> State msg
    -> Task x (State msg)
dispatch router endpoint headers body state =
    let
        correlationId =
            Stomp.Internal.Frame.headerValue "correlation-id" headers

        subscription =
            Stomp.Internal.Frame.headerValue "subscription" headers

        message =
            Stomp.Internal.Message.init headers body
    in
        case correlationId of
            Just id ->
                dispatchCallback router id message state.callbacks
                    |> Task.andThen
                        (\_ ->
                            Task.succeed
                                { state
                                    | callbacks =
                                        Dict.remove id state.callbacks
                                }
                        )

            Nothing ->
                dispatchMessage router endpoint subscription message state.subscriptions
                    |> Task.andThen (\_ -> Task.succeed state)


dispatchMessage :
    Platform.Router msg (Msg msg)
    -> Endpoint
    -> Maybe String
    -> Result String Stomp.Internal.Message.InternalMessage
    -> Dict Endpoint (Dict String (Callback msg))
    -> Task x ()
dispatchMessage router endpoint subscription message subscriptions =
    let
        getSubscription =
            Maybe.map2
                (,)
                (Dict.get endpoint subscriptions)
                subscription
                |> Maybe.andThen
                    (\( subs, id ) ->
                        Dict.get id subs
                    )
    in
        case getSubscription of
            Nothing ->
                Task.succeed ()

            Just callback ->
                Platform.sendToApp router (callback message)


dispatchCallback :
    Platform.Router msg (Msg msg)
    -> Stomp.Internal.Proc.CorrelationId
    -> Result String Stomp.Internal.Message.InternalMessage
    -> Dict Stomp.Internal.Proc.CorrelationId (Callback msg)
    -> Task x ()
dispatchCallback router correlationId message callbacks =
    case Dict.get correlationId callbacks of
        Nothing ->
            Task.succeed ()

        Just callback ->
            Platform.sendToApp router (callback message)


{-| Create an authenticated session with the specified options.

    Stomp.Client.connect "http://stomp.example.com/"
        { vhost = "/"
        , login = "username"
        , passcode = "123456"
        , onConnected = Connected
        , onDisconnected = Disconnected
        , onError = Error
        }

-}
connect : Endpoint -> Options msg -> Cmd msg
connect endpoint options =
    command (Connect endpoint options)


{-| Disconnect from a server.

    Stomp.Client.disconnect "http://stomp.example.com/"

-}
disconnect : Endpoint -> Cmd msg
disconnect endpoint =
    command (Disconnect endpoint)


{-| Send a message to a specific topic.

    sendStrings : List String -> Cmd Msg
    sendStrings strings =
        let
            topic =
                "example.strings"

            headers =
                [ ( "x-example", "this is a header" ) ]

            body =
                Json.Encode.list (List.map Json.Encode.string strings)
        in
            Stomp.Client.send "http://stomp.example.com/" topic headers body

-}
send : Endpoint -> String -> List Header -> Maybe Json.Encode.Value -> Cmd msg
send server destination headers body =
    let
        headers_ =
            [ ( "destination", destination )
            , ( "content-type", "application/json" )
            ]
                ++ headers
    in
        frame "SEND" headers_ (Body.encode body)
            |> Send server
            |> command


{-| Send a remote procedure call to a server.

    type Msg
        = Strings (Result String Stomp.Message.Message)

    getStrings : Cmd Msg
    getStrings =
        Stomp.Proc.init "example.strings"
            |> Stomp.Proc.onResponse Strings
            |> Stomp.Client.call "http://stomp.example.com/"

-}
call : String -> RemoteProcedure msg -> Cmd msg
call server =
    Stomp.Internal.Batch.cmd
        (\proc ->
            command (Call server proc)
        )


{-| Subscribe to message from a server on a specific topic.

    type Msg
        = Strings (Result String Stomp.Message.Message)

    strings : Cmd Msg
    strings =
        Stomp.Subscription.init "example.strings"
            |> Stomp.Subscription.onMessage Strings
            |> Stomp.Client.subscribe "http://stomp.example.com/"

-}
subscribe : Endpoint -> Subscription msg -> Cmd msg
subscribe server sub =
    command (Subscribe server sub)


{-| Unsubscribe an existing subscription (uses subscription id to identify which subscription to unsubscribe).

    Stomp.Subscription.init "example.strings"
        |> Stomp.Subscription.withSubscriptionId "strings-1"
        |> Stomp.Client.unsubscribe "http://stomp.example.com/"

-}
unsubscribe : Endpoint -> Subscription msg -> Cmd msg
unsubscribe server sub =
    command (Unsubscribe server sub)


{-| Acknowledge that a message was consumed by the client when using `ClientAck` or `ClientIndividualAck` modes on a subscription.
-}
ack : Endpoint -> Message -> Maybe String -> Cmd msg
ack server message trx =
    case message.ack of
        Just ack ->
            let
                headers =
                    case trx of
                        Just trx ->
                            [ ( "id", ack )
                            , ( "transaction", trx )
                            ]

                        Nothing ->
                            [ ( "id", ack ) ]
            in
                frame "ACK" headers Nothing
                    |> Send server
                    |> command

        Nothing ->
            Cmd.none


{-| The opposite of `ack`.

It is used to tell the server that the client did not consume the message. The server can then either send the message to a different client, discard it, or put it in a dead letter queue. The exact behavior is server specific.

`nack` applies either to one single message (if the subscription's ack mode is `ClientIndividualAck`) or to all messages sent before and not yet `ack`'ed or `nack`'ed (if the subscription's ack mode is `ClientAck`).

-}
nack : Endpoint -> Message -> Maybe String -> Cmd msg
nack server message trx =
    case message.ack of
        Just ack ->
            let
                headers =
                    case trx of
                        Just trx ->
                            [ ( "id", ack )
                            , ( "transaction", trx )
                            ]

                        Nothing ->
                            [ ( "id", ack ) ]
            in
                frame "NACK" headers Nothing
                    |> Send server
                    |> command

        Nothing ->
            Cmd.none


{-| `begin` is used to start a transaction. Transactions in this case apply to sending and acknowledging - any messages sent or acknowledged during a transaction will be processed atomically based on the transaction.
-}
begin : Endpoint -> String -> Cmd msg
begin server trx =
    let
        headers =
            [ ( "transaction", trx ) ]
    in
        frame "BEGIN" headers Nothing
            |> Send server
            |> command


{-| `commit` is used to commit a transaction in progress.
-}
commit : Endpoint -> String -> Cmd msg
commit server trx =
    let
        headers =
            [ ( "transaction", trx ) ]
    in
        frame "COMMIT" headers Nothing
            |> Send server
            |> command


{-| `abort` is used to roll back a transaction in progress.
-}
abort : Endpoint -> String -> Cmd msg
abort server trx =
    let
        headers =
            [ ( "transaction", trx ) ]
    in
        frame "ABORT" headers Nothing
            |> Send server
            |> command
