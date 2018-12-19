module Stomp.Client exposing
    ( listen, connect, disconnect
    , send, call, subscribe, unsubscribe
    , ack, nack
    , begin, commit, abort
    , Connection, OnMessage, Session, init
    )

{-| A session manages the connection with the server and is used to send commands to the server.


# Connection

@docs listen, connect, disconnect


# Commands

@docs send, call, subscribe, unsubscribe


# Acknowledgement

@docs ack, nack


# Transactions

@docs begin, commit, abort

-}

import Stomp.Internal.Batch
import Stomp.Internal.Body as Body
import Stomp.Internal.Connection
import Stomp.Internal.Dispatch
import Stomp.Internal.Frame exposing (Header, frame)
import Stomp.Internal.Message
import Stomp.Internal.Proc
import Stomp.Internal.Session
    exposing
        ( insertCallback
        , insertSubscription
        , removeCallback
        , removeSubscription
        )
import Stomp.Internal.Subscription
import Stomp.Message exposing (Message)
import Stomp.Proc exposing (RemoteProcedure)
import Stomp.Subscription exposing (Subscription)


type alias Connection msg =
    Stomp.Internal.Connection.Connection msg


type alias OnMessage msg =
    Stomp.Internal.Connection.OnMessage msg


type alias Options msg =
    Stomp.Internal.Session.Options msg


type alias Session msg =
    Stomp.Internal.Session.Session msg


init : Connection msg -> Options msg -> Session msg
init connection options =
    Stomp.Internal.Session.init connection options


listen : OnMessage msg -> Session msg -> Sub msg
listen onMessage session =
    onMessage
        (\msg ->
            Stomp.Internal.Frame.decode msg
                |> Stomp.Internal.Dispatch.dispatch session
        )


connect : Session msg -> String -> String -> String -> Cmd msg
connect session login passcode vhost =
    let
        headers =
            [ ( "accept-version", "1.2" )
            , ( "heart-beat", "0,1000" )
            , ( "host", vhost )
            , ( "login", login )
            , ( "passcode", passcode )
            ]
    in
    frame "CONNECT" headers Nothing
        |> Stomp.Internal.Frame.encode
        |> session.connection


disconnect : Session msg -> ( Session msg, Cmd msg )
disconnect session =
    let
        headers =
            [ ( "receipt", "DISCONNECT" ) ]
    in
    frame "DISCONNECT" headers Nothing
        |> Stomp.Internal.Frame.encode
        |> (\frm -> ( session, session.connection frm ))


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
        Stomp.Session.send session topic headers body

-}
send : Session msg -> String -> List Header -> Body.Value -> ( Session msg, Cmd msg )
send session destination headers body =
    let
        headers_ =
            [ ( "destination", destination ) ]
                ++ headers
    in
    frame "SEND" headers_ (Body.encode body)
        |> Stomp.Internal.Frame.encode
        |> (\frm -> ( session, session.connection frm ))


{-| Send a remote procedure call to a server.

    type Msg
        = Strings (Result String Stomp.Message.Message)

    getStrings : Cmd Msg
    getStrings =
        Stomp.Proc.init "example.strings"
            |> Stomp.Proc.onResponse Strings
            |> Stomp.Session.call session

-}
call : Session msg -> RemoteProcedure msg -> ( Session msg, Cmd msg )
call session =
    Stomp.Internal.Batch.foldl
        (\proc ( session_, cmd ) ->
            let
                correlationId =
                    String.fromInt session_.nextId
            in
            Stomp.Internal.Proc.call proc correlationId
                |> Stomp.Internal.Frame.encode
                |> (\frm ->
                        ( insertCallback proc session_
                        , Cmd.batch [ cmd, session.connection frm ]
                        )
                   )
        )
        ( session, Cmd.none )


{-| Subscribe to message from a server on a specific topic.

    type Msg
        = Strings (Result String Stomp.Message.Message)

    strings : Cmd Msg
    strings =
        Stomp.Subscription.init "example.strings"
            |> Stomp.Subscription.onMessage Strings
            |> Stomp.Session.subscribe session

-}
subscribe : Session msg -> Subscription msg -> ( Session msg, Cmd msg )
subscribe session =
    Stomp.Internal.Batch.foldl
        (\sub ( session_, cmd ) ->
            Stomp.Internal.Subscription.subscribe sub
                |> Stomp.Internal.Frame.encode
                |> (\frm ->
                        ( insertSubscription sub session_
                        , Cmd.batch [ cmd, session.connection frm ]
                        )
                   )
        )
        ( session, Cmd.none )


{-| Unsubscribe an existing subscription (uses subscription id to identify which subscription to unsubscribe).

    Stomp.Subscription.init "example.strings"
        |> Stomp.Subscription.withSubscriptionId "strings-1"
        |> Stomp.Session.unsubscribe session

-}
unsubscribe : Session msg -> Subscription msg -> ( Session msg, Cmd msg )
unsubscribe session =
    Stomp.Internal.Batch.foldl
        (\sub ( session_, cmd ) ->
            Stomp.Internal.Subscription.unsubscribe sub
                |> Stomp.Internal.Frame.encode
                |> (\frm ->
                        ( removeSubscription sub session_
                        , Cmd.batch [ cmd, session.connection frm ]
                        )
                   )
        )
        ( session, Cmd.none )


{-| Acknowledge that a message was consumed by the session when using `SessionAck` or `SessionIndividualAck` modes on a subscription.
-}
ack : Session msg -> Message -> Maybe String -> ( Session msg, Cmd msg )
ack session message trx =
    case message.ack of
        Just id ->
            let
                headers =
                    case trx of
                        Just trx_ ->
                            [ ( "id", id )
                            , ( "transaction", trx_ )
                            ]

                        Nothing ->
                            [ ( "id", id ) ]
            in
            frame "ACK" headers Nothing
                |> Stomp.Internal.Frame.encode
                |> (\frm -> ( session, session.connection frm ))

        Nothing ->
            ( session, Cmd.none )


{-| The opposite of `ack`.

It is used to tell the server that the session did not consume the message. The server can then either send the message to a different session, discard it, or put it in a dead letter queue. The exact behavior is server specific.

`nack` applies either to one single message (if the subscription's ack mode is `SessionIndividualAck`) or to all messages sent before and not yet `ack`'ed or `nack`'ed (if the subscription's ack mode is `SessionAck`).

-}
nack : Session msg -> Message -> Maybe String -> ( Session msg, Cmd msg )
nack session message trx =
    case message.ack of
        Just id ->
            let
                headers =
                    case trx of
                        Just trx_ ->
                            [ ( "id", id )
                            , ( "transaction", trx_ )
                            ]

                        Nothing ->
                            [ ( "id", id ) ]
            in
            frame "NACK" headers Nothing
                |> Stomp.Internal.Frame.encode
                |> (\frm -> ( session, session.connection frm ))

        Nothing ->
            ( session, Cmd.none )


{-| `begin` is used to start a transaction. Transactions in this case apply to sending and acknowledging - any messages sent or acknowledged during a transaction will be processed atomically based on the transaction.
-}
begin : Session msg -> String -> ( Session msg, Cmd msg )
begin session trx =
    let
        headers =
            [ ( "transaction", trx ) ]
    in
    frame "BEGIN" headers Nothing
        |> Stomp.Internal.Frame.encode
        |> (\frm -> ( session, session.connection frm ))


{-| `commit` is used to commit a transaction in progress.
-}
commit : Session msg -> String -> ( Session msg, Cmd msg )
commit session trx =
    let
        headers =
            [ ( "transaction", trx ) ]
    in
    frame "COMMIT" headers Nothing
        |> Stomp.Internal.Frame.encode
        |> (\frm -> ( session, session.connection frm ))


{-| `abort` is used to roll back a transaction in progress.
-}
abort : Session msg -> String -> ( Session msg, Cmd msg )
abort session trx =
    let
        headers =
            [ ( "transaction", trx ) ]
    in
    frame "ABORT" headers Nothing
        |> Stomp.Internal.Frame.encode
        |> (\frm -> ( session, session.connection frm ))
