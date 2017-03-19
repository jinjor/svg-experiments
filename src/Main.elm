module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SvgView


main : Program Never Model Msg
main =
  beginnerProgram { model = model, view = view, update = update }


type alias Model =
  { dp : Int -- 18dp, 24dp, 36dp, 48dp
  , positions : Array String
  }


model : Model
model =
  Model 48 (Array.fromList <| List.repeat 10 "")


type Msg
  = Input Int String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Input index value ->
      { model | positions = Array.set index value model.positions }


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "SVG Icon Editor" ]
    , container model
    ]


container : Model -> Html Msg
container model =
  div [ class "container" ]
    [ form model
    , SvgView.view model.dp model.positions
    , node "style" [] [ text styles ]
    ]

form : Model -> Html Msg
form model =
  model.positions
    |> Array.indexedMap (paramInput)
    |> Array.toList
    |> div [ class "form" ]


paramInput : Int -> String -> Html Msg
paramInput index _ =
  div [ class "param" ]
    [ label [] [ text (toString index) ]
    , input [ onInput (Input index) ] []
    ]


styles : String
styles = """
.container {
  display: flex;
}
.container > * {
  margin: 10px;
}
.form {
  width: 30%;
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
.param {
  display: flex;
  line-height: 30px;
  margin-bottom: 5px;
}
.param > label {
  width: 100px;
  text-align: right;
  padding-right: 10px;
}
.param > input {
  width: 100%;
}
"""
