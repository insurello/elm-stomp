module Stomp.Client exposing
  ( AckMode(..)
  , listen
  , connect
  , disconnect
  , send
  , subscribe
  , unsubscribe
  , ack
  , nack
  , begin
  , commit
  , abort
  )

import WebSocket
import Json.Encode
import Stomp.Frame exposing (frame, encode, Header)


type alias HeartBeatRate =
  ( Int, Int )


type AckMode
  = AutoAck
  | ClientAck
  | ClientIndividualAck


listen : String -> (String -> msg) -> Sub msg
listen server msg =
  WebSocket.listen server msg


connect : String -> String -> String -> String -> HeartBeatRate -> Cmd msg
connect server login passcode vhost (cx, cy) =
  let
      headers =
        [ ("accept-version", "1.2")
        , ("host", vhost)
        , ("login", login)
        , ("passcode", passcode)
        , ("heart-beat", (toString cx) ++ "," ++ (toString cy))
        ]
  in
    frame "CONNECT" headers Nothing
      |> encode
      |> WebSocket.send server


disconnect : String -> String -> Cmd msg
disconnect server receipt =
  let
    headers =
      [ ("receipt", receipt) ]
  in
    frame "DISCONNECT" headers Nothing
      |> encode
      |> WebSocket.send server


send : String -> String -> List Header -> Maybe Json.Encode.Value -> Cmd msg
send server destination headers body =
  let
    headers_ =
      [ ("destination", destination)
      , ("content-type", "application/json")
      ] ++ headers
  in
    frame "SEND" headers_ (Maybe.map (Json.Encode.encode 0) body)
      |> encode
      |> WebSocket.send server


subscribe : String -> String -> String -> AckMode -> Cmd msg
subscribe server id destination ack =
  let
    headers =
      [ ("id", id)
      , ("destination", destination)
      , ("ack", case ack of
          AutoAck -> "auto"
          ClientAck -> "client"
          ClientIndividualAck -> "client-individual"
        )
      ]
  in
    frame "SUBSCRIBE" headers Nothing
      |> encode
      |> WebSocket.send server


unsubscribe : String -> String -> Cmd msg
unsubscribe server id =
  let
    headers =
      [ ("id", id) ]
  in
    frame "UNSUBSCRIBE" headers Nothing
      |> encode
      |> WebSocket.send server


ack : String -> String -> Maybe String -> Cmd msg
ack server id trx =
  let
    headers =
      case trx of
        Just trx ->
          [ ("id", id)
          , ("transaction", trx)
          ]
        Nothing ->
          [ ("id", id) ]
  in
    frame "ACK" headers Nothing
      |> encode
      |> WebSocket.send server


nack : String -> String -> Maybe String -> Cmd msg
nack server id trx =
  let
    headers =
      case trx of
        Just trx ->
          [ ("id", id)
          , ("transaction", trx)
          ]
        Nothing ->
          [ ("id", id) ]
  in
    frame "NACK" headers Nothing
      |> encode
      |> WebSocket.send server


begin : String -> String -> Cmd msg
begin server trx =
  let
    headers =
      [ ("transaction", trx) ]
  in
    frame "BEGIN" headers Nothing
      |> encode
      |> WebSocket.send server


commit : String -> String -> Cmd msg
commit server trx =
  let
    headers =
      [ ("transaction", trx) ]
  in
    frame "COMMIT" headers Nothing
      |> encode
      |> WebSocket.send server


abort : String -> String -> Cmd msg
abort server trx =
  let
    headers =
      [ ("transaction", trx) ]
  in
    frame "ABORT" headers Nothing
      |> encode
      |> WebSocket.send server
