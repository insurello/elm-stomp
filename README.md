# Stomp

An Elm client for the STOMP protocol (version 1.2).

https://stomp.github.io/

## Example usage

This example connects to a broker and subscribes to a topic (example.strings).
It also sends a remote procedure call to the same topic when the user clicks.

```elm
import Stomp.Client
import Stomp.Message
import Stomp.Proc
import Stomp.Subscription

import Html exposing (..)
import Html.Events exposing (..)

port socket : Stomp.Client.Connection msg
port onMessage : Stomp.Client.OnMessage msg

type alias State =
    { session : Stomp.Client.Session Msg
    , result : List String
    }

type Msg
    = Connected
    | Disconnected
    | Error String
    | HeartBeat
    | Click
    | Strings (Result String (List String))

subscriptions : State -> Sub Msg
subscriptions state =
    Stomp.Client.listen onMessage state.session

init : ( State, Cmd Msg )
init =
    let
        session =
            Stomp.Client.init socket
                { onConnected = Connected
                , onDisconnected = Disconnected
                , onError = Error
                , onHeartBeat = HeartBeat
                }
    in
    ( State session List.empty
    , Stomp.Client.connect session "guest" "guest" "/"
    )

update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Connected ->
            ( state, subscribe state.session )

        Disconnected ->
            ( state, Cmd.none )

        Error _ ->
            ( state, Cmd.none )

        HeartBeat ->
            ( state, Cmd.none )

        Click ->
            ( state, getString state.session )

        Strings (Ok strings) ->
            ( { state | result = strings }, Cmd.none )

        Strings (Err _) ->
            ( state, Cmd.none )

subscribe : Stomp.Client.Session Msg -> Cmd Msg
subscribe session =
    Stomp.Subscription.init "example.strings"
        |> Stomp.Subscription.expectJson Strings decoder
        |> Stomp.Client.subscribe session

getStrings : Stomp.Client.Session Msg -> Cmd Msg
getStrings session =
    Stomp.Proc.init "example.strings"
        |> Stomp.Proc.expectJson Strings decoder
        |> Stomp.Client.call session

decoder : Json.Decode.Decoder (List String)
decoder =
  Json.Decode.list Json.Decode.string

view : State -> Html Msg
view state =
  div []
    [ ul [] (List.map (\str -> li [] [ text str ]) state.result)
    , button
        [ onClick Click ]
        [ text "Fetch strings" ]
    ]
```

## Ports

To actually connect to a server your application need to be wired up to a WebSocket using two ports. One for outgoing messages and another one for incoming.

```javascript
var socket = new WebSocket("ws://stomp.example.com/", ["v12.stomp"]);
socket.onopen = function() {
  var app = Elm.Main.init();
  app.ports.socket.subscribe(function(data) {
    socket.send(data);
  });
  socket.onmessage = function(event) {
    app.ports.onMessage.send(event.data);
  };
};
```
