module Stomp.Internal.Session exposing (..)

import Stomp.Internal.Frame exposing (Frame, frame)


type alias Session msg =
    { options : Options msg
    , connected : Bool
    , error : Maybe String
    }


type alias Options msg =
    { vhost : String
    , login : String
    , passcode : String
    , onConnected : msg
    , onDisconnected : msg
    , onError : String -> msg
    }


init : Options msg -> Session msg
init options =
    { options = options
    , connected = False
    , error = Nothing
    }


connected : Session msg -> Session msg
connected session =
    { session | connected = True, error = Nothing }


error : String -> Session msg -> Session msg
error err session =
    { session | error = Just err, connected = False }


map : (a -> b) -> Options a -> Options b
map func options =
    { vhost = options.vhost
    , login = options.login
    , passcode = options.passcode
    , onConnected = func options.onConnected
    , onDisconnected = func options.onDisconnected
    , onError = (\err -> func (options.onError err))
    }


connect : Options msg -> Frame
connect { login, passcode, vhost } =
    let
        headers =
            [ ( "accept-version", "1.2" )
            , ( "host", vhost )
            , ( "login", login )
            , ( "passcode", passcode )
            ]
    in
        frame "CONNECT" headers Nothing


disconnect : Frame
disconnect =
    let
        headers =
            [ ( "receipt", "DISCONNECT" ) ]
    in
        frame "DISCONNECT" headers Nothing
