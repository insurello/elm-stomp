module StompTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz exposing (..)
import Stomp.Internal.Frame as Frame


invalidFrame : Fuzzer String
invalidFrame =
    let
        options =
            { retries = 10
            , fallback = \frame -> frame ++ "x"
            , condition =
                \frame ->
                    not (String.all (\c -> c == '\n') frame)
            }
    in
        conditional options string


suite : Test
suite =
    describe "Frame"
        [ describe "encode"
            [ test "should encode frame" <|
                \_ ->
                    Expect.equal
                        "SEND\ndestination:test\n\nHi\x00"
                        (Frame.encode ( "SEND", [ ( "destination", "test" ) ], Just ("Hi") ))
            , test "should escape headers" <|
                \_ ->
                    Expect.equal
                        "SEND\n\\r\\n\\c\\\\:\\r\\n\\c\\\\\n\n\x00"
                        (Frame.encode ( "SEND", [ ( "\x0D\n:\\", "\x0D\n:\\" ) ], Nothing ))
            ]
        , describe "decode"
            [ test "should decode CONNECTED frame" <|
                \_ ->
                    case Frame.decode "CONNECTED\nversion:1.2\n\n\x00" of
                        Ok (Frame.Connected headers) ->
                            Expect.equal [ ( "version", "1.2" ) ] headers

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should decode MESSAGE frame" <|
                \_ ->
                    case Frame.decode "MESSAGE\n\n\x00" of
                        Ok (Frame.Message _ _) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should decode RECEIPT frame" <|
                \_ ->
                    case Frame.decode "RECEIPT\nreceipt-id:123\n\n\x00" of
                        Ok (Frame.Receipt receiptId) ->
                            Expect.equal "123" receiptId

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should fail to decode RECEIPT frame without receipt-id header" <|
                \_ ->
                    case Frame.decode "RECEIPT\nreceipt-id:123\n\n\x00" of
                        Ok (Frame.Receipt receiptId) ->
                            Expect.equal "123" receiptId

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should decode ERROR frame" <|
                \_ ->
                    case Frame.decode "ERROR\n\nerror message\x00" of
                        Ok (Frame.Error msg) ->
                            Expect.equal (Just "error message") msg

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should unescape frame headers" <|
                \_ ->
                    case Frame.decode "MESSAGE\n\\r\\n\\c\\\\:\\r\\n\\c\\\\\n\n\x00" of
                        Ok (Frame.Message headers _) ->
                            Expect.equal [ ( "\x0D\n:\\", "\x0D\n:\\" ) ] headers

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should read content-length bytes of frame body" <|
                \_ ->
                    case Frame.decode "MESSAGE\ncontent-length:12\ncontent-type:application/octet-stream\n\nhello\x00world" of
                        Ok (Frame.Message _ body) ->
                            Expect.equal (Just "hello\x00world") body

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , test "should handle frame body with multi-byte characters" <|
                \_ ->
                    case Frame.decode "MESSAGE\ncontent-length:6\n\nI♥NY\x00" of
                        Ok (Frame.Message _ body) ->
                            Expect.equal (Just "I♥NY") body

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , test "should read body until zero byte if no content-length header" <|
                \_ ->
                    case Frame.decode "MESSAGE\n\nhello\x00world" of
                        Ok (Frame.Message _ body) ->
                            Expect.equal (Just "hello") body

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , fuzz invalidFrame "should fail to decode invalid frame" <|
                \frame ->
                    case Frame.decode frame of
                        Err _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , test "should decode EOL frame as heart beat" <|
                \_ ->
                    case Frame.decode "\n" of
                        Ok Frame.HeartBeat ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected EOL to be decoded as heart beat."
            , test "should decode empty frame as heart beat" <|
                \_ ->
                    case Frame.decode "" of
                        Ok Frame.HeartBeat ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected empty frame to be decoded as heart beat."
            ]
        , describe "headerValue"
            [ test "should use first value if the same header is repeated" <|
                \_ ->
                    Expect.equal
                        (Just "one")
                        (Frame.headerValue "test" [ ( "test", "one" ), ( "test", "two" ) ])
            ]
        ]
