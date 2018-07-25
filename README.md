# Stomp

An Elm client for the STOMP protocol.

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

type alias State =
    List String

type Msg
    = Connected
    | Disconnected
    | Error
    | Click
    | Strings (Result String Stomp.Message.Message)

server : String
server =
    "http://stomp.example.com/"

subscriptions : Sub Msg
subscriptions =
    Stomp.Client.listen server

init : ( State, Cmd Msg )
init =
    List.empty !
      [ Stomp.Client.connect "http://stomp.example.com/"
          { vhost = "/"
          , login = "username"
          , passcode = "123456"
          , onConnected = Connected
          , onDisconnected = Disconnected
          , onError = Error
          }
      ]

update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Connected ->
            state ! [ subscribe ]

        Disconnected ->
            state ! []

        Error _ ->
            state ! []

        Click ->
            state ! [ getStrings ]

        Strings (Ok strings) ->
            strings ! []

        Strings (Err _) ->
            state ! []

subscribe : Cmd Msg
subscribe =
    Stomp.Subscription.init "example.strings"
        |> Stomp.Subscription.onMessage Strings
        |> Stomp.Client.subscribe "http://stomp.example.com/"

getStrings : Cmd Msg
getStrings =
    Stomp.Proc.init "example.strings"
        |> Stomp.Proc.onResponse Strings
        |> Stomp.Client.call "http://stomp.example.com/"

view : State -> Html Msg
view state =
  div []
    [ ul [] (List.map (\str -> li [] [ text str ]) state)
    , button
        [ onClick Click ]
        [ text "Fetch strings" ]
    ]
```
