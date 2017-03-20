module UI exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


labeledInput : (String -> msg) -> String -> String -> Html msg
labeledInput toMsg labelText val =
  div
    [ classList [ ("labeled-input", True) ] ]
    [ label [] [ text labelText ]
    , input [ onInput toMsg, defaultValue val ] []
    ]
