module Stomp.Internal.Frame
    exposing
        ( frame
        , headerValue
        , encode
        , decode
        , Frame
        , Header
        , ServerFrame(..)
        )

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


decode : String -> Result String ServerFrame
decode frame =
    case parseFrame frame of
        ( "CONNECTED", headers, _ ) ->
            Ok (Connected headers)

        ( "MESSAGE", headers, body ) ->
            Ok (Message headers body)

        ( "RECEIPT", [ ( "receipt-id", receiptId ) ], _ ) ->
            Ok (Receipt receiptId)

        ( "ERROR", headers, body ) ->
            Ok (Error body)

        ( "", [], Nothing ) ->
            Ok (HeartBeat)

        _ ->
            Err "Invalid frame"


encode : Frame -> String
encode ( command, headers, body ) =
    let
        headerLines =
            headers
                |> List.filter (\( k, _ ) -> k /= "")
                |> List.map (\( k, v ) -> (escape k) ++ ":" ++ (escape v))
                |> String.join "\n"

        bodyStr =
            Maybe.withDefault "" body
    in
        command ++ "\n" ++ headerLines ++ "\n\n" ++ bodyStr ++ "\x00"


replaceAll : ( String, String ) -> String -> String
replaceAll ( from, to ) =
    Regex.replace Regex.All (from |> Regex.escape |> Regex.regex) (\_ -> to)


escape : String -> String
escape str =
    List.foldl replaceAll
        str
        [ ( "\\", "\\\\" )
        , ( "\x0D", "\\r" )
        , ( "\n", "\\n" )
        , ( ":", "\\c" )
        ]


unescape : String -> String
unescape str =
    List.foldl replaceAll
        str
        [ ( "\\r", "\x0D" )
        , ( "\\n", "\n" )
        , ( "\\c", ":" )
        , ( "\\\\", "\\" )
        ]


parseFrame : String -> Frame
parseFrame frame =
    frame
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
                |> Maybe.andThen (\v -> v |> String.toInt |> Result.toMaybe)
    in
        case ( contentType, contentLength ) of
            ( Just "application/octet-stream", Just bytes ) ->
                body
                    |> String.toList
                    |> List.take bytes
                    |> String.fromList

            _ ->
                body
                    |> String.split "\x00"
                    |> List.head
                    |> Maybe.withDefault ""


parseHeader : String -> Maybe Header
parseHeader str =
    case String.split ":" str of
        [ k, v ] ->
            Just ( k |> unescape, v |> unescape )

        _ ->
            Nothing
