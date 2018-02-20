module Subscription exposing (subscriptions)

import Time exposing (Time, second)
import Type exposing (..)


subscriptions : World -> Sub Msg
subscriptions model =
    Time.every (Time.second * 2) Tick
