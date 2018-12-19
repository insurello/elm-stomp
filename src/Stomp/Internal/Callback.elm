module Stomp.Internal.Callback exposing (Callback, map)

import Stomp.Internal.Message exposing (InternalMessage)


type alias Callback msg =
    Result String InternalMessage -> msg


map : (a -> b) -> Callback a -> Callback b
map func callback =
    \a -> func (callback a)
