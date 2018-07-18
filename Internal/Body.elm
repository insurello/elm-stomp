module Stomp.Internal.Body exposing (Value, decoder, encode)

import Json.Encode
import Json.Decode


type alias Value =
    Maybe Json.Decode.Value


decoder : Maybe String -> String -> Result String (Maybe Json.Decode.Value)
decoder body contentType =
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


encode : Maybe Json.Encode.Value -> Maybe String
encode body =
    Maybe.map (Json.Encode.encode 0) body
