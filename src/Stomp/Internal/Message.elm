module Stomp.Internal.Message exposing (InternalMessage, init)

import Json.Decode
import Stomp.Internal.Body as Body
import Stomp.Internal.Frame exposing (Header)


type alias InternalMessage =
    { id : String
    , ack : Maybe String
    , redelivered : Bool
    , headers : List Header
    , payload : Body.Value
    }


init : List Header -> Maybe String -> Result String InternalMessage
init headers body =
    let
        messageId =
            Stomp.Internal.Frame.headerValue "message-id" headers
                |> Result.fromMaybe "Missing message-id"

        contentType =
            Stomp.Internal.Frame.headerValue "content-type" headers
                |> Maybe.withDefault "application/json"

        ack =
            Stomp.Internal.Frame.headerValue "ack" headers

        redelivered =
            case Stomp.Internal.Frame.headerValue "redelivered" headers of
                Just "true" ->
                    True

                Just "false" ->
                    False

                _ ->
                    False
    in
    Result.map2
        (\messageId_ payload ->
            { id = messageId_
            , ack = ack
            , redelivered = redelivered
            , headers = headers
            , payload = payload
            }
        )
        messageId
        (Body.decoder body contentType
            |> Result.mapError Json.Decode.errorToString
        )
