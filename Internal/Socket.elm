module Stomp.Internal.Socket exposing (..)

import WebSocket.LowLevel as WS
import Process
import Task exposing (Task)


type Connection
  = Closed
  | Opening Int Process.Id
  | Connected WS.WebSocket


type alias InternalSocket =
  { connection : Connection }


internalSocket : InternalSocket
internalSocket =
    { connection = Closed }


update : InternalSocket -> InternalSocket -> InternalSocket
update newSocket oldSocket =
  oldSocket


opening : Int -> Process.Id -> InternalSocket -> InternalSocket
opening backoff pid socket =
    { socket | connection = (Opening backoff pid) }


connected : WS.WebSocket -> InternalSocket -> InternalSocket
connected ws socket =
    { socket | connection = (Connected ws) }


backoffIteration : InternalSocket -> Int
backoffIteration socket =
  case socket.connection of
    Opening n _ -> n
    _ -> 0


close : InternalSocket -> Task x ()
close { connection } =
  case connection of
    Opening _ pid ->
      Process.kill pid

    Connected socket ->
      WS.close socket

    Closed ->
      Task.succeed ()


send : InternalSocket -> String -> Task x (Maybe WS.BadSend)
send { connection } frame =
  case connection of
    Opening _ _ ->
      Task.succeed (Nothing)

    Closed ->
      Task.succeed (Nothing)

    Connected socket ->
      WS.send socket frame
