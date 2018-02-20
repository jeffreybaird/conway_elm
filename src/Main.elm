module Main exposing (..)

import Type exposing (..)
import Model exposing (init)
import View exposing (view)
import Html exposing (program)
import Update exposing (update)
import Subscription exposing (subscriptions)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
