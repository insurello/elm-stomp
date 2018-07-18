module Stomp.Proc
    exposing
        ( RemoteProcedure
        , init
        , withHeader
        , withHeaders
        , withPayload
        , onResponse
        , map
        )

import Json.Encode exposing (Value)
import Stomp.Internal.Frame exposing (Header)
import Stomp.Internal.Callback exposing (Callback)


type alias RemoteProcedure msg =
    Proc msg


type alias Proc msg =
    { cmd : String
    , headers : List Header
    , body : Maybe Value
    , onResponse : Maybe (Callback msg)
    }


init : String -> Proc msg
init cmd =
    { cmd = cmd
    , headers = []
    , body = Nothing
    , onResponse = Nothing
    }


withHeader : Header -> Proc msg -> Proc msg
withHeader header proc =
    { proc | headers = proc.headers ++ [ header ] }


withHeaders : List Header -> Proc msg -> Proc msg
withHeaders headers proc =
    { proc | headers = proc.headers ++ headers }


withPayload : Value -> Proc msg -> Proc msg
withPayload body proc =
    { proc | body = Just body }


onResponse : Callback msg -> Proc msg -> Proc msg
onResponse callback proc =
    { proc | onResponse = Just callback }


map : (a -> b) -> Proc a -> Proc b
map func proc =
    let
        mapCallback func callback =
            (\a -> func (callback a))
    in
        { proc | onResponse = Maybe.map (mapCallback func) proc.onResponse }
