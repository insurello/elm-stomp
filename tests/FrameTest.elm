module FrameTest exposing (suite)

import Expect exposing (Expectation)
import Json.Encode
import Stomp.Internal.Frame as Frame
import Test exposing (..)


suite : Test
suite =
    describe "Frame"
        [ describe "encode"
            [ test "should encode frame" <|
                \_ ->
                    Expect.equal
                        (Json.Encode.string "SEND\ndestination:test\ncontent-encoding:utf8\ncontent-type:application/json\n\nHi\u{0000}")
                        (Frame.encode ( "SEND", [ ( "destination", "test" ) ], Just "Hi" ))
            , test "should escape headers" <|
                \_ ->
                    Expect.equal
                        (Json.Encode.string "SEND\n\\r\\n\\c\\\\:\\r\\n\\c\\\\\ncontent-length:0\n\n\u{0000}")
                        (Frame.encode ( "SEND", [ ( "\u{000D}\n:\\", "\u{000D}\n:\\" ) ], Nothing ))
            ]
        , describe "decode"
            [ test "should decode CONNECTED frame" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "CONNECTED\nversion:1.2\n\n\u{0000}") of
                        Ok (Frame.Connected headers) ->
                            Expect.equal [ ( "version", "1.2" ) ] headers

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should decode MESSAGE frame" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "MESSAGE\n\n\u{0000}") of
                        Ok (Frame.Message _ _) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should decode RECEIPT frame" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "RECEIPT\nreceipt-id:123\n\n\u{0000}") of
                        Ok (Frame.Receipt receiptId) ->
                            Expect.equal "123" receiptId

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should fail to decode RECEIPT frame without receipt-id header" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "RECEIPT\nreceipt-id:123\n\n\u{0000}") of
                        Ok (Frame.Receipt receiptId) ->
                            Expect.equal "123" receiptId

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should decode ERROR frame" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "ERROR\n\nerror message\u{0000}") of
                        Ok (Frame.Error msg) ->
                            Expect.equal (Just "error message") msg

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should unescape frame headers" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "MESSAGE\n\\r\\n\\c\\\\:\\r\\n\\c\\\\\n\n\u{0000}") of
                        Ok (Frame.Message headers _) ->
                            Expect.equal [ ( "\u{000D}\n:\\", "\u{000D}\n:\\" ) ] headers

                        _ ->
                            Expect.fail "Invalid frame"
            , test "should read content-length bytes of frame body" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "MESSAGE\ncontent-length:12\ncontent-type:application/octet-stream\n\nhello\u{0000}world") of
                        Ok (Frame.Message _ body) ->
                            Expect.equal (Just "hello\u{0000}world") body

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , test "should handle frame body with multi-byte characters" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "MESSAGE\ncontent-length:6\n\nI♥NY\u{0000}") of
                        Ok (Frame.Message _ body) ->
                            Expect.equal (Just "I♥NY") body

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , test "should read body until zero byte if no content-length header" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "MESSAGE\n\nhello\u{0000}world") of
                        Ok (Frame.Message _ body) ->
                            Expect.equal (Just "hello") body

                        _ ->
                            Expect.fail "Did not fail on invalid frame."
            , test "should decode EOL frame as heart beat" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "\n") of
                        Ok Frame.HeartBeat ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected EOL to be decoded as heart beat."
            , test "should decode empty frame as heart beat" <|
                \_ ->
                    case Frame.decode (Json.Encode.string "") of
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
