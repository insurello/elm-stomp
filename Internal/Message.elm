module Stomp.Internal.Message exposing (InternalMessage, init)

import Json.Encode
import Json.Decode exposing (Value)
import Stomp.Internal.Frame exposing (Header)


type alias InternalMessage =
    { id : String
    , ack : Maybe String
    , redelivered : Bool
    , headers : List Header
    , payload : Maybe Value
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
            (\messageId payload ->
                { id = messageId
                , ack = ack
                , redelivered = redelivered
                , headers = headers
                , payload = payload
                }
            )
            (messageId)
            (decodeBody body contentType)


decodeBody : Maybe String -> String -> Result String (Maybe Value)
decodeBody body contentType =
    case body of
        Just str ->
            case contentType of
                "application/json" ->
                    Json.Decode.decodeString Json.Decode.value str
                        |> Result.map Just

                _ ->
                    Result.Ok (Just (Json.Encode.string str))

        Nothing ->
            Result.Ok Nothing
