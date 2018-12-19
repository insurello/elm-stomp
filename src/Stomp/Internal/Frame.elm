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
import Regex


type alias Frame =
    ( String, List Header, Maybe String )


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
    ( command, headers, body )


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


decode : Json.Decode.Value -> Result String ServerFrame
decode value =
    Json.Decode.decodeValue Json.Decode.string value
        |> Result.mapError Json.Decode.errorToString
        |> Result.map parseFrame
        |> Result.andThen
            (\frm ->
                case frm of
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
encode ( command, headers, body ) =
    let
        contentHeaders =
            if body == Nothing then
                [ ( "content-length", "0" ) ]

            else
                [ ( "content-encoding", "utf8" )
                , ( "content-type", "application/json" )
                ]

        headerLines =
            (headers ++ contentHeaders)
                |> List.filter (\( k, _ ) -> k /= "")
                |> List.map (\( k, v ) -> escape k ++ ":" ++ escape v)
                |> String.join "\n"

        bodyStr =
            Maybe.withDefault "" body
    in
    command
        ++ "\n"
        ++ headerLines
        ++ "\n\n"
        ++ bodyStr
        ++ "\u{0000}"
        |> Json.Encode.string


replaceAll : ( Maybe Regex.Regex, String ) -> String -> String
replaceAll ( from, to ) =
    case from of
        Just regex ->
            Regex.replace regex (\_ -> to)

        Nothing ->
            Regex.replace Regex.never (\_ -> to)


escape : String -> String
escape str =
    List.foldl replaceAll
        str
        [ ( Regex.fromString "\\", "\\\\" )
        , ( Regex.fromString "\u{000D}", "\\r" )
        , ( Regex.fromString "\n", "\\n" )
        , ( Regex.fromString ":", "\\c" )
        ]


unescape : String -> String
unescape str =
    List.foldl replaceAll
        str
        [ ( Regex.fromString "\\r", "\u{000D}" )
        , ( Regex.fromString "\\n", "\n" )
        , ( Regex.fromString "\\c", ":" )
        , ( Regex.fromString "\\\\", "\\" )
        ]


parseFrame : String -> Frame
parseFrame str =
    str
        |> String.lines
        |> List.foldl
            (\a b ->
                case b of
                    ( "", [], Nothing ) ->
                        ( a, [], Nothing )

                    ( cmd, headers, Nothing ) ->
                        case a of
                            "" ->
                                ( cmd, headers, Just "" )

                            _ ->
                                case parseHeader a of
                                    Just h ->
                                        ( cmd, headers ++ [ h ], Nothing )

                                    Nothing ->
                                        ( cmd, headers, Nothing )

                    ( cmd, headers, Just body ) ->
                        ( cmd, headers, Just (body ++ a) )
            )
            ( "", [], Nothing )
        |> (\( cmd, headers, body ) ->
                ( cmd, headers, Maybe.map (readBody headers) body )
           )


readBody : List Header -> String -> String
readBody headers body =
    let
        contentType =
            headers
                |> headerValue "content-type"

        contentLength =
            headers
                |> headerValue "content-length"
                |> Maybe.andThen (\v -> v |> String.toInt)
    in
    case ( contentType, contentLength ) of
        ( Just "application/octet-stream", Just bytes ) ->
            body
                |> String.toList
                |> List.take bytes
                |> String.fromList

        _ ->
            body
                |> String.split "\u{0000}"
                |> List.head
                |> Maybe.withDefault ""


parseHeader : String -> Maybe Header
parseHeader str =
    case String.split ":" str of
        [ k, v ] ->
            Just ( k |> unescape, v |> unescape )

        _ ->
            Nothing
