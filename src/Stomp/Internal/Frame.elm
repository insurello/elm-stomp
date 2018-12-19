module Stomp.Internal.Frame exposing
    ( Frame
    , Header
    , ServerFrame(..)
    , decode
    , encode
    , frame
    , headerValue
    )

import Json.Decode
import Json.Encode


type alias Frame =
    { command : String
    , headers : List Header
    , body : Maybe String
    }


type alias Header =
    ( String, String )


type ServerFrame
    = Connected (List Header)
    | Message (List Header) (Maybe String)
    | Receipt String
    | Error (Maybe String)
    | HeartBeat


frame : String -> List Header -> Maybe String -> Frame
frame command headers body =
    Frame command headers body


headerValue : String -> List Header -> Maybe String
headerValue key headers =
    headers
        |> List.filterMap
            (\( k, v ) ->
                if k == key then
                    Just v

                else
                    Nothing
            )
        |> List.head


decode : Json.Encode.Value -> Result String ServerFrame
decode value =
    let
        headersDecoder =
            Json.Decode.keyValuePairs Json.Decode.string

        decoder =
            Json.Decode.map3 Frame
                (Json.Decode.field "command" Json.Decode.string)
                (Json.Decode.field "headers" headersDecoder)
                (Json.Decode.maybe
                    (Json.Decode.field "body" Json.Decode.string)
                )

        decodeFrame =
            Json.Decode.decodeValue decoder
    in
    decodeFrame value
        |> Result.mapError Json.Decode.errorToString
        |> Result.andThen
            (\frame_ ->
                case ( frame_.command, frame_.headers, frame_.body ) of
                    ( "CONNECTED", headers, _ ) ->
                        Ok (Connected headers)

                    ( "MESSAGE", headers, body ) ->
                        Ok (Message headers body)

                    ( "RECEIPT", [ ( "receipt-id", receiptId ) ], _ ) ->
                        Ok (Receipt receiptId)

                    ( "ERROR", headers, body ) ->
                        Ok (Error body)

                    ( "", [], Nothing ) ->
                        Ok HeartBeat

                    _ ->
                        Err "Invalid frame"
            )


encode : Frame -> Json.Encode.Value
encode { command, headers, body } =
    let
        headersValue =
            Json.Encode.object
                (List.map (\( k, v ) -> ( k, Json.Encode.string v )) headers)

        bodyStr =
            Maybe.withDefault "{}" body
    in
    Json.Encode.object
        [ ( "command", Json.Encode.string command )
        , ( "headers", headersValue )
        , ( "body", Json.Encode.string bodyStr )
        ]
