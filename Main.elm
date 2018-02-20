module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes
import Svg.Events exposing (onClick)
import Debug
import Time exposing (Time, second)
import Random.Pcg exposing (..)
import Random


type alias Model =
    { time : Time, grid : Grid }


type alias Grid =
    List Row


type alias Row =
    List Cell


type alias Cell =
    { position : Coordinate
    , alive : Bool
    }


type alias Coordinate =
    ( Int, Int )


type alias World =
    { time : Time, grid : Grid, seed : Int }


type Msg
    = Tick Time
    | RandN Int
    | Reset


type Status
    = Dead
    | Alive



-- Model


initSeed : Int -> Seed
initSeed n =
    initialSeed n


initialWorld : World
initialWorld =
    { time = 0, grid = buildGridWithSeed 10 10 10, seed = 1 }


buildCell : Int -> Int -> Int -> Cell
buildCell xPosition yPosition status =
    let
        newStatus =
            if status == 1 then
                True
            else
                False
    in
        { position = ( xPosition, yPosition ), alive = newStatus }


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



-- View


cellColor : Cell -> String
cellColor cell =
    if cell.alive == True then
        "blue"
    else
        "white"


viewGridSvg : Grid -> Html Msg
viewGridSvg grid =
    let
        svgGrid =
            buildSvgGrid [] grid
    in
        div []
            [ div []
                [ Svg.svg
                    [ Svg.Attributes.width "1000", Svg.Attributes.height "1000" ]
                    svgGrid
                ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]


buildSvgGrid : List (Svg.Svg Msg) -> Grid -> List (Svg.Svg Msg)
buildSvgGrid svgGrid oldGrid =
    List.map viewRowSvg oldGrid
        |> List.foldr (++) svgGrid


viewRowSvg : Row -> List (Svg.Svg Msg)
viewRowSvg cells =
    List.map makeRect cells


makeRect cell =
    let
        ( x, y ) =
            cell.position
    in
        Svg.rect
            [ Svg.Attributes.x (toString (100 * x))
            , Svg.Attributes.y (toString (100 * y))
            , Svg.Attributes.width "100"
            , Svg.Attributes.height "100"
            , Svg.Attributes.fill (cellColor cell)
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "2"
            ]
            []


makeId : Coordinate -> String
makeId position =
    let
        ( x, y ) =
            position
    in
        String.append "," (toString y)
            |> String.append (toString x)


view : World -> Html Msg
view model =
    let
        switch =
            cos (turns (Time.inMinutes model.time))
    in
        model.grid
            |> toggleGrid
            |> viewGridSvg


toggleGrid : Grid -> Grid
toggleGrid grid =
    List.map toggleRow grid


toggleRow : Row -> Row
toggleRow row =
    List.map toggleCell row


toggleCell : Cell -> Cell
toggleCell cell =
    let
        ( x, y ) =
            cell.position

        newLabel =
            not cell.alive
    in
        { cell | alive = newLabel }



-- Update


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
                    { world | grid = (buildGridWithSeed 10 10 n) }
            in
                ( newModel, Cmd.none )


subscriptions : World -> Sub Msg
subscriptions model =
    Time.every second Tick


init : ( World, Cmd Msg )
init =
    ( initialWorld, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
