module Stomp.Internal.Connection exposing (Connection, OnMessage)

import Json.Encode


type alias Connection msg =
    Json.Encode.Value -> Cmd msg


type alias OnMessage msg =
    (Json.Encode.Value -> msg) -> Sub msg
