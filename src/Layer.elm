module Layer exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UI exposing (..)


type alias Position =
  { x : Float
  , y : Float
  }


type alias Layer =
  { rotation : Maybe Int
  , positions : Array (Maybe Position)
  }


init : Layer
init =
  Layer Nothing (Array.fromList <| List.repeat 10 Nothing)


update : Msg -> Layer -> Layer
update msg layer =
  case msg of
    InputRotation rotation ->
      { layer | rotation = rotation }

    InputPosition index value ->
      { layer | positions = Array.set index value layer.positions }


type Msg
  = InputRotation (Maybe Int)
  | InputPosition Int (Maybe Position)


form : Layer -> Html Msg
form layer =
  div [ class "layer-form" ]
    [ optionsForm layer
    , positionsForm layer
    ]


positionsForm : Layer -> Html Msg
positionsForm layer =
  layer.positions
    |> Array.indexedMap positionInput
    |> Array.toList
    |> div [ class "layer-form-positions" ]


optionsForm : Layer -> Html Msg
optionsForm layer =
  div
    [ class "layer-form-options" ]
    [ labeledInput
        (String.toInt >> Result.toMaybe >> InputRotation)
        "Rotation"
        (layer.rotation |> Maybe.map toString |> Maybe.withDefault "")
    ]


positionInput : Int -> Maybe Position -> Html Msg
positionInput index position =
  labeledInput
    (toPosition >> InputPosition index)
    (toString index)
    ( position
        |> Maybe.map (\p -> toString p.x ++ "," ++ toString p.y)
        |> Maybe.withDefault ""
    )


toPosition : String -> Maybe Position
toPosition s =
  case String.split "," s of
    x :: y :: _ ->
      case (String.toFloat x, String.toFloat y) of
        (Ok x, Ok y) ->
          Just (Position x y)

        _ ->
          Nothing

    _ ->
      Nothing
