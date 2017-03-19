module SvgView exposing (..)

import Array exposing (Array)
import Html as H
import Html.Attributes as A
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Position =
  { x : Int
  , y : Int
  }


view : Int -> Array String -> Svg msg
view dp positions =
  H.div
    [ A.class "svg-container" ]
    [ horizontal
    , vertical
    , viewSvg dp positions
    ]


horizontal : Svg msg
horizontal =
  H.div [ A.class "horizontal" ] []


vertical : Svg msg
vertical =
  H.div [ A.class "vertical" ] []


viewSvg : Int -> Array String -> Svg msg
viewSvg dp positions =
  svg
    [ width (toString dp)
    , height (toString dp)
    , viewBox (viewBoxValue dp)
    , preserveAspectRatio "none"
    ]
    [ viewPath positions ]


viewBoxValue : Int -> String
viewBoxValue dp =
  [ -dp // 2, -dp // 2, dp, dp ]
    |> List.map toString
    |> String.join " "


viewPath : Array String -> Svg msg
viewPath positions =
  positions
    |> Array.filter (String.isEmpty >> not)
    |> Array.indexedMap (\index pos ->
        (if index == 0 then "M" else "L") ++ pos
      )
    |> Array.toList
    |> String.join ""
    |> (flip (++) "z")
    |> (\value -> Svg.path [ stroke "black", d value ] [] )
