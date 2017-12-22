module Stomp.Internal.Callback exposing (Callback)

import Stomp.Internal.Message exposing (InternalMessage)

type alias Callback msg =
  (Result String InternalMessage -> msg)
