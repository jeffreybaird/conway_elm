module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes
import Svg.Events exposing (onClick)
import Debug
import Time exposing (Time, second)


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
    { grid : Grid }



-- Model


initialWorld : World
initialWorld =
    { grid = buildGrid 10 10 }


buildCell : Int -> Int -> Bool -> Cell
buildCell xPosition yPosition class =
    { position = ( xPosition, yPosition ), alive = class }


addCellToRow : Cell -> Row -> Row
addCellToRow cell row =
    cell :: row


buildRow : Int -> Int -> Row
buildRow length rowNumber =
    recursiveRowBuilder length rowNumber []


recursiveRowBuilder : Int -> Int -> Row -> Row
recursiveRowBuilder xCoord yCoord tempRow =
    if xCoord <= 0 then
        tempRow
    else
        recursiveRowBuilder (xCoord - 1) yCoord (addCellToRow (buildCell xCoord yCoord True) tempRow)


addRowToGrid : Grid -> Row -> Grid
addRowToGrid grid row =
    row :: grid


recursiveGridBuilder : Int -> Int -> Grid -> Grid
recursiveGridBuilder xCoord yCoord tempGrid =
    if yCoord <= 0 then
        tempGrid
    else
        buildRow xCoord yCoord
            |> addRowToGrid tempGrid
            |> recursiveGridBuilder xCoord
                (yCoord - 1)


buildGrid : Int -> Int -> Grid
buildGrid columns rows =
    recursiveGridBuilder columns rows []



-- View


cellColor : Cell -> String
cellColor cell =
    if cell.alive == True then
        "blue"
    else
        "white"


viewGridSvg : Grid -> List (Svg.Svg Msg)
viewGridSvg grid =
    let
        svgGrid =
            buildSvgGrid [] grid
    in
        [ Svg.svg
            [ Svg.Attributes.width "1000", Svg.Attributes.height "1000" ]
            svgGrid
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
            , Svg.Events.onClick Togglecell
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
view world =
    viewGridSvg world.grid
        |> div []



-- Update


type Msg
    = Togglecell
    | Tick Time


subscriptions : Time -> Sub Msg
subscriptions model =
    Time.every second Tick


update : Msg -> World -> World
update msg world =
    case msg of
        Togglecell ->
            { world | grid = (toggle world.grid) }


toggle : Grid -> Grid
toggle grid =
    List.map toggleRow grid


toggleRow row =
    List.map toggleCell row


toggleCell cell =
    { cell | alive = (not cell.alive) }


main =
    Html.program
        { init = initialWorld
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
