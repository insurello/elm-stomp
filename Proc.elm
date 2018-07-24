module Stomp.Proc
    exposing
        ( RemoteProcedure
        , init
        , withHeader
        , withHeaders
        , withPayload
        , onResponse
        , batch
        , none
        )

import Json.Encode exposing (Value)
import Stomp.Internal.Proc exposing (Proc)
import Stomp.Internal.Frame exposing (Header)
import Stomp.Internal.Callback exposing (Callback)
import Stomp.Internal.Batch exposing (Batch)


type alias RemoteProcedure msg =
    Batch (Proc msg)


init : String -> RemoteProcedure msg
init cmd =
    Stomp.Internal.Batch.identity
        { cmd = cmd
        , headers = []
        , body = Nothing
        , onResponse = Nothing
        }


withHeader : Header -> RemoteProcedure msg -> RemoteProcedure msg
withHeader header =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | headers = proc.headers ++ [ header ] }
        )


withHeaders : List Header -> RemoteProcedure msg -> RemoteProcedure msg
withHeaders headers =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | headers = proc.headers ++ headers }
        )


withPayload : Value -> RemoteProcedure msg -> RemoteProcedure msg
withPayload body =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | body = Just body }
        )


onResponse : Callback msg -> RemoteProcedure msg -> RemoteProcedure msg
onResponse callback =
    Stomp.Internal.Batch.map
        (\proc ->
            { proc | onResponse = Just callback }
        )


batch : List (RemoteProcedure msg) -> RemoteProcedure msg
batch =
    Stomp.Internal.Batch.batch


none : RemoteProcedure msg
none =
    Stomp.Internal.Batch.none
