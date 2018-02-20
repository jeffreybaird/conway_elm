module Model exposing (init, buildGridWithSeed, findNeighbors)

import Type exposing (..)
import Random.Pcg exposing (Seed, int, initialSeed)


-- Model


initSeed : Int -> Seed
initSeed n =
    initialSeed n


initialWorld : World
initialWorld =
    let
        help =
            buildGridWithSeed 20 20 10
    in
        { time = 0, grid = buildGridWithSeed 20 20 10, seed = 1 }


buildCell : Int -> Int -> Int -> Cell
buildCell xPosition yPosition status =
    let
        newStatus =
            if status == 1 then
                Alive
            else
                Dead
    in
        { position = ( xPosition, yPosition ), alive = newStatus, neighbors = (findNeighbors ( xPosition, yPosition )) }


findNeighbors : Coordinate -> List Coordinate
findNeighbors position =
    let
        ( x, y ) =
            position
    in
        [ ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x + 1, y + 1 )
        , ( x, y + 1 )
        , ( x - 1, y + 1 )
        ]
            |> List.filter (\( z, n ) -> z > 0 && n > 0)


addCellToRow : Cell -> Row -> Row
addCellToRow cell row =
    cell :: row


buildRow : Int -> Int -> Int -> Seed -> Row
buildRow length rowNumber n seed =
    recursiveRowBuilder length rowNumber n seed []


recursiveRowBuilder : Int -> Int -> Int -> Seed -> Row -> Row
recursiveRowBuilder xCoord yCoord status seed tempRow =
    if xCoord <= 0 then
        tempRow
    else
        let
            ( n, nextSeed ) =
                Random.Pcg.step (int 1 2) seed
        in
            recursiveRowBuilder (xCoord - 1) yCoord n nextSeed (addCellToRow (buildCell xCoord yCoord status) tempRow)


addRowToGrid : Grid -> Row -> Grid
addRowToGrid grid row =
    row :: grid


recursiveGridBuilder : Int -> Int -> Int -> Seed -> Grid -> Grid
recursiveGridBuilder xCoord yCoord n seed tempGrid =
    let
        ( nextStatus, newSeed ) =
            Random.Pcg.step (int 1 2) seed
    in
        if yCoord <= 0 then
            tempGrid
        else
            buildRow xCoord yCoord n seed
                |> addRowToGrid tempGrid
                |> recursiveGridBuilder xCoord (yCoord - 1) nextStatus newSeed


buildGridWithSeed : Int -> Int -> Int -> Grid
buildGridWithSeed columns rows n =
    let
        seed0 =
            initSeed n

        ( status, seed1 ) =
            Random.Pcg.step (int 1 2) seed0
    in
        recursiveGridBuilder columns rows status seed1 []


init : ( World, Cmd Msg )
init =
    ( initialWorld, Cmd.none )
