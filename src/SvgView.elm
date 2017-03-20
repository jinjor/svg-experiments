module SvgView exposing (..)

import Array exposing (Array)
import Html as H
import Html.Attributes as A
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Layer exposing (Layer, Position)


noShift : Position
noShift =
  Position 0 0


shift : Int -> Position
shift dp =
  Position (toFloat <| dp//2) (toFloat <| dp//2)


getPositions : Layer -> List Position
getPositions layer =
  layer.positions
    |> Array.toList
    |> List.filterMap identity


view : Int -> Array Layer -> Svg msg
view dp layers =
  let
    pathString =
      layers
        |> Array.toList
        |> List.map (\layer -> formatPath noShift (copyRotation (Maybe.withDefault 1 layer.rotation) (getPositions layer)))
        |> String.join ""

    pathStringShifted =
      layers
        |> Array.toList
        |> List.map (\layer -> formatPath (shift dp) (copyRotation (Maybe.withDefault 1 layer.rotation) (getPositions layer)))
        |> String.join ""

    editor =
      H.div
        []
        [ viewSvg True dp pathString
        , H.div [] [ text pathString ]
        ]

    preview =
      H.div
        []
        [ viewSvg False dp pathStringShifted
        , H.div [] [ text pathStringShifted ]
        ]
  in
    H.div [] [ editor, preview ]


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


viewSvg : Bool -> Int -> String -> Svg msg
viewSvg editor dp pathString =
  svg
    [ width (toString dp)
    , height (toString dp)
    , viewBox (viewBoxValue editor dp)
    , preserveAspectRatio "none"
    , class (if editor  then "svg editor" else "svg preview")
    ]
    ( Svg.path [ stroke "black", d pathString ] [] ::
      ( if editor then [ horizontal dp, vertical dp ] else [] )
    )


horizontal : Int -> Svg msg
horizontal =
  guideLine False


vertical : Int -> Svg msg
vertical =
  guideLine True


guideLine : Bool -> Int -> Svg msg
guideLine vertical dp =
  Svg.path
    [ stroke "#ddd"
    , strokeWidth (toFixed <| toFloat dp/200)
    , strokeDasharray "1"
    , strokeDashoffset "0.5"
    , if vertical then
        d ("M0," ++ toString (-dp//2) ++  "V" ++ toString dp)
      else
        d ("M" ++ toString (-dp//2) ++  ",0H" ++ toString dp)
    ] []


viewBoxValue : Bool -> Int -> String
viewBoxValue editor dp =
  ( if editor then
      [ -dp // 2, -dp // 2, dp, dp ]
    else
      [ 0, 0, dp, dp ]
  )
    |> List.map toString
    |> String.join " "


formatPath : Position -> List Position -> String
formatPath shift positions =
  if List.length positions < 2 then
    ""
  else
    positions
      |> List.indexedMap (\index pos ->
          (if index == 0 then "M" else "L") ++ toFixed (shift.x + pos.x) ++ "," ++ toFixed (shift.y + pos.y)
        )
      |> String.join ""
      |> (flip (++) "z")


toFixed : Float -> String
toFixed =
  (*) 10.0 >> round >> toFloat >> flip (/) 10.0 >> toString
