module Stomp.Internal.Session exposing (Options, Session, connected, error, init, map)

import Stomp.Internal.Frame exposing (Frame, frame)


type alias Session msg =
    { options : Options msg
    , connected : Bool
    , error : Maybe String
    }


type alias Options msg =
    { onConnected : msg
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
    { onConnected = func options.onConnected
    , onDisconnected = func options.onDisconnected
    , onError = \err -> func (options.onError err)
    }
