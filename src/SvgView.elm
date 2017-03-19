module SvgView exposing (..)

import Array exposing (Array)
import Html as H
import Html.Attributes as A
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Position =
  { x : Float
  , y : Float
  }


view : Int -> Maybe Int -> List Position -> Svg msg
view dp rotation positions =
  let
    pathString =
      formatPath (copyRotation (Maybe.withDefault 1 rotation) positions)
  in
    H.div
      [ A.class "svg-container" ]
      [ horizontal
      , vertical
      , viewSvg dp pathString
      , H.div [] [ text pathString ]
      ]


copyRotation : Int -> List Position -> List Position
copyRotation rotation positions =
  let
    theta =
      (2 * pi) / toFloat rotation
  in
    List.range 0 (rotation - 1)
      |> List.map (toFloat >> (*) theta)
      |> List.concatMap (\theta -> List.map (rotate theta) positions)


rotate : Float -> Position -> Position
rotate theta { x, y } =
  Position
    (x * cos theta - y * sin theta)
    (x * sin theta + y * cos theta)


horizontal : Svg msg
horizontal =
  H.div [ A.class "horizontal" ] []


vertical : Svg msg
vertical =
  H.div [ A.class "vertical" ] []


viewSvg : Int -> String -> Svg msg
viewSvg dp pathString =
  svg
    [ width (toString dp)
    , height (toString dp)
    , viewBox (viewBoxValue dp)
    , preserveAspectRatio "none"
    ]
    [ Svg.path [ stroke "black", d pathString ] [] ]


viewBoxValue : Int -> String
viewBoxValue dp =
  [ -dp // 2, -dp // 2, dp, dp ]
    |> List.map toString
    |> String.join " "


formatPath : List Position -> String
formatPath positions =
  if List.length positions < 2 then
    ""
  else
    positions
      |> List.indexedMap (\index pos ->
          (if index == 0 then "M" else "L") ++ toFixed pos.x ++ "," ++ toFixed pos.y
        )
      |> String.join ""
      |> (flip (++) "z")


toFixed : Float -> String
toFixed =
  (*) 10.0 >> round >> toFloat >> flip (/) 10.0 >> toString
