module Stomp.Proc exposing
    ( RemoteProcedure, init
    , withHeader, withHeaders, withPayload
    , onResponse, expectJson
    , batch, none
    , expiresAfter, map
    )

{-| A remote procedure call (the request/response pattern).

    import Stomp.Proc
    import Stomp.Client

    type Msg = Click | Response (Result String (List String))

    update : Msg -> State -> (State, Cmd Msg)
    update msg state =
        case msg of
            Click ->
                state ! [ getStrings ]

            Response (Ok strings) ->
                ...

            Response (Err _) ->
                ...

    getStrings : Cmd Msg
    getStrings =
        Stomp.Proc.init "example.strings"
            |> Stomp.Proc.onResponse Response
            |> Stomp.Client.call server


# Remote Procedures

@docs RemoteProcedure, init


# Headers and Payload

@docs withHeader, withHeaders, withPayload


# Response

@docs onResponse, expectJson


# Batching

@docs batch, none

-}

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Stomp.Internal.Batch exposing (Batch)
import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Frame exposing (Header)
import Stomp.Internal.Proc exposing (Proc)
import Stomp.Message


{-| Describes a remote procedure call.
-}
type alias RemoteProcedure msg =
    Batch (Proc msg)


{-| Construct a remote procedure call with the specified command (queue name).
-}
init : String -> RemoteProcedure msg
init cmd =
    Stomp.Internal.Batch.singleton
        { cmd = cmd
        , headers = []
        , body = Nothing
        , onResponse = Nothing
        }


expiresAfter : Float -> RemoteProcedure msg -> RemoteProcedure msg
expiresAfter milliseconds =
    withHeader ( "expiration", String.fromFloat milliseconds )


{-| Add a header to the request message.
-}
withHeader : Header -> RemoteProcedure msg -> RemoteProcedure msg
withHeader header =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | headers = proc.headers ++ [ header ] }
        )


{-| Add multiple headers to the request message.
-}
withHeaders : List Header -> RemoteProcedure msg -> RemoteProcedure msg
withHeaders headers =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | headers = proc.headers ++ headers }
        )


{-| Add a payload to the request message.
-}
withPayload : Value -> RemoteProcedure msg -> RemoteProcedure msg
withPayload body =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | body = Just body }
        )


{-| Set a callback to be triggered when the response message is received.
-}
onResponse : Callback msg -> RemoteProcedure msg -> RemoteProcedure msg
onResponse callback =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | onResponse = Just callback }
        )


{-| Set a callback to be triggered when the response message is received and a JSON decoder to be used to decode the message body.
-}
expectJson : (Result String a -> msg) -> Decoder a -> RemoteProcedure msg -> RemoteProcedure msg
expectJson callback decoder =
    let
        decodeMessage message =
            message
                |> Result.andThen
                    (\msg ->
                        case Stomp.Message.payload decoder msg of
                            Ok payload ->
                                Ok payload

                            Err error ->
                                Err error
                    )
    in
    onResponse (\a -> callback (decodeMessage a))


{-| Batch multiple remote procedure calls together.
-}
batch : List (RemoteProcedure msg) -> RemoteProcedure msg
batch =
    Stomp.Internal.Batch.batch


{-| Return a remote procedure call that does nothing.
-}
none : RemoteProcedure msg
none =
    Stomp.Internal.Batch.none


map : (a -> b) -> RemoteProcedure a -> RemoteProcedure b
map func =
    Stomp.Internal.Batch.map (Stomp.Internal.Proc.map func)
