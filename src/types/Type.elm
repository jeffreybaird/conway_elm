module Type exposing (..)

import Time exposing (Time, second)


type alias Model =
    { time : Time, grid : Grid }


type alias Grid =
    List Row


type alias Row =
    List Cell


type alias Cell =
    { position : Coordinate
    , alive : Status
    , neighbors : List Coordinate
    }


type alias Coordinate =
    ( Int, Int )


type alias World =
    { time : Time, grid : Grid, seed : Int }


type Msg
    = Tick Time
    | RandN Int
    | Reset
    | Step


type Status
    = Dead
    | Alive
