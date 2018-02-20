module View exposing (..)

import Type exposing (..)
import Html exposing (..)
import Html.Attributes
import Svg
import Svg.Attributes
import Svg.Events exposing (onClick)
import Time exposing (Time, second)


cellColor : Cell -> String
cellColor cell =
    if cell.alive == Alive then
        "blue"
    else
        "white"


viewGridSvg : Grid -> Html Msg
viewGridSvg grid =
    let
        svgGrid =
            buildSvgGrid [] grid

        height =
            (List.length grid * 100)
                + 100
                |> toString
    in
        div []
            [ div []
                [ Svg.svg
                    [ Svg.Attributes.width height, Svg.Attributes.height height ]
                    svgGrid
                ]
            , button [ onClick Reset ] [ text "Reset" ]
            , button [ onClick Step ] [ text "Step" ]
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
            , Svg.Attributes.id ((toString x) ++ (toString y))
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
    model.grid
        |> viewGridSvg
