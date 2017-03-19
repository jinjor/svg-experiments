module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SvgView exposing (Position)


main : Program Never Model Msg
main =
  beginnerProgram { model = model, view = view, update = update }


type alias Model =
  { dp : Int -- 18dp, 24dp, 36dp, 48dp
  , positions : Array String
  , rotation : Maybe Int
  }


model : Model
model =
  Model 48 (Array.fromList <| List.repeat 10 "") Nothing


type Msg
  = Input Int String
  | Rotation (Maybe Int)


update : Msg -> Model -> Model
update msg model =
  case msg of
    Input index value ->
      { model | positions = Array.set index value model.positions }

    Rotation rotation ->
      { model | rotation = rotation }


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "SVG Icon Editor" ]
    , container model
    ]


container : Model -> Html Msg
container model =
  div [ class "container" ]
    [ node "style" [] [ text styles ]
    , optionsForm model
    , positionsForm model
    , SvgView.view model.dp model.rotation (toPositionList model.positions)
    ]


toPositionList : Array String -> List Position
toPositionList array =
  array
    |> Array.toList
    |> List.map (String.split ",")
    |> List.filterMap (\list ->
      case list of
        x :: y :: _ ->
          case (String.toFloat x, String.toFloat y) of
            (Ok x, Ok y) ->
              Just (Position x y)

            _ ->
              Nothing

        _ ->
          Nothing
      )


positionsForm : Model -> Html Msg
positionsForm model =
  model.positions
    |> Array.indexedMap (paramInput)
    |> Array.toList
    |> div [ class "form-positions" ]


optionsForm : Model -> Html Msg
optionsForm model =
  div
    [ class "form-options" ]
    [ labeledInput
        (String.toInt >> Result.toMaybe >> Rotation)
        "Rotation"
    ]


paramInput : Int -> String -> Html Msg
paramInput index _ =
  labeledInput (Input index) (toString index)


labeledInput : (String -> msg) -> String -> Html msg
labeledInput toMsg str =
  div
    [ classList [ ("labeled-input", True) ] ]
    [ label [] [ text str ]
    , input [ onInput toMsg ] []
    ]


styles : String
styles = """
.container {
  display: flex;
}
.container > * {
  margin: 10px;
}
.form-positions {
  width: 100px;
  height: 100%;
}
.form-options {
  width: 200px;
  height: 100%;
}
.svg-container {
  border: solid 1px #aaa;
  position: relative;
  width: 200px;
  height: 200px;
}
svg {
  width: 200px;
  height: 200px;
}
.horizontal {
  border-top: dashed 1px #ddd;
  top: 50%;
  width: 100%;
  position: absolute;
}
.vertical {
  border-left: dashed 1px #ddd;
  left: 50%;
  height: 100%;
  position: absolute;
}
.labeled-input {
  display: flex;
  line-height: 30px;
  margin-bottom: 5px;
}
.labeled-input > label {
  width: 100px;
  text-align: right;
  padding-right: 10px;
}
.labeled-input > input {
  width: 100%;
}
"""
