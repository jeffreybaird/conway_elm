module Update exposing (update)

import Random.Pcg exposing (Seed, int, initialSeed)
import Random
import Model exposing (buildGridWithSeed, findNeighbors)
import Type exposing (..)


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick newTime ->
            let
                newModel =
                    { world | time = newTime, grid = (toggleGrid world.grid) }
            in
                ( newModel, Cmd.none )

        Reset ->
            ( world, Random.generate RandN (Random.int 1 10021121) )

        RandN n ->
            let
                newModel =
                    { world | grid = (buildGridWithSeed 20 20 n) }
            in
                ( newModel, Cmd.none )

        Step ->
            let
                newModel =
                    { world | grid = (toggleGrid world.grid) }
            in
                ( newModel, Cmd.none )


toggleGrid : Grid -> Grid
toggleGrid grid =
    List.map (toggleRow grid) grid


toggleRow : Grid -> Row -> Row
toggleRow grid row =
    List.map (toggleCell grid) row


toggleCell : Grid -> Cell -> Cell
toggleCell grid cell =
    let
        newLabel =
            neighborCells cell.position grid
                |> statusBasedOnNeighbors cell
    in
        { cell | alive = newLabel }


statusBasedOnNeighbors : Cell -> List Cell -> Status
statusBasedOnNeighbors currentCell cells =
    let
        position =
            currentCell.position

        status =
            currentCell.alive

        neighbors =
            List.map (\c -> c.alive) cells

        liveNeighbors =
            List.filter (\c -> c.alive == Alive) cells
    in
        if (status == Dead) && ((List.length liveNeighbors) == 3) then
            Alive
        else if (status == Alive) && ((List.length liveNeighbors)) < 2 then
            Dead
        else if status == Alive && ((List.length liveNeighbors) == 2 || (List.length liveNeighbors) == 3) then
            Alive
        else if status == Alive && (List.length liveNeighbors) > 3 then
            Dead
        else
            Dead


neighborCells : Coordinate -> Grid -> List Cell
neighborCells position grid =
    let
        neighbors =
            findNeighbors position

        parseRow : Row -> List Cell
        parseRow row =
            List.filter (\c -> List.member c.position neighbors) row
    in
        List.map parseRow grid
            |> List.foldr (++) []
