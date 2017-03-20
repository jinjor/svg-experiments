module Main exposing (..)

import Array exposing (Array)
import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import SvgView
import Layer exposing (Layer)
import UI exposing (..)


main : Program Never Model Msg
main =
  beginnerProgram { model = init, view = view, update = update }


type alias Model =
  { dp : Int -- 18dp, 24dp, 36dp, 48dp
  , layers : Array Layer
  , selectedLayer : Int
  }


init : Model
init =
  Model 48 (Array.fromList <| List.repeat 5 Layer.init) 0


type Msg
  = NoOp
  | InputDp Int
  | UpdateLayer Int Layer.Msg
  | SelectLayer Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    InputDp dp ->
      { model | dp = dp }

    UpdateLayer layerIndex msg ->
      { model
          | layers =
              model.layers
                |> Array.get layerIndex
                |> Maybe.map (Layer.update msg)
                |> Maybe.map (\layer -> Array.set layerIndex layer model.layers)
                |> Maybe.withDefault model.layers
      }

    SelectLayer index ->
      { model | selectedLayer = index }


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
    , globalOptionForm model
    , layerForms model.selectedLayer model.layers
    , SvgView.view model.dp model.layers
    ]


globalOptionForm : Model -> Html Msg
globalOptionForm model =
  div [ class "global-options" ]
    [ labeledInput
        (String.toInt >> Result.toMaybe >> Maybe.map InputDp >> Maybe.withDefault NoOp)
        "size"
        (toString model.dp)
    , layerSelect (Array.length model.layers) model.selectedLayer
    ]


layerSelect : Int -> Int -> Html Msg
layerSelect length selectedLayer =
  div []
    ( List.range 0 (length - 1)
        |> List.map (layerOption selectedLayer)
    )


layerOption : Int -> Int -> Html Msg
layerOption selectedLayer index =
  div
    [ onClick (SelectLayer index)
    , classList [ ("layer-option", True), ("layer-option-selected", index == selectedLayer) ]
    ]
    [ text ("Layer " ++ toString (index + 1)) ]


layerForms : Int -> Array Layer -> Html Msg
layerForms selectedLayer layers =
  Array.get selectedLayer layers
    |> Maybe.map (\layer -> [ (toString selectedLayer, Layer.form layer |> Html.map (UpdateLayer selectedLayer)) ] )
    |> Maybe.withDefault []
    |> Keyed.node "div" []


styles : String
styles = """
.container {
  display: flex;
}
.container > * {
  margin: 10px;
}
.global-options {
  width: 150px;
}
.layer-option {
  background-color: #eee;
  line-height: 30px;
  margin-bottom: 4px;
  border-radius: 3px;
  padding-left: 10px;
}
.layer-option-selected {
  background-color: #68d;
}
.layer-form {
  width: 200px;
  height: 100%;
}
.svg {
  border: solid 1px #aaa;
}
.svg.editor {
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
