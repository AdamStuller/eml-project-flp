module Projekt exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Tabulka  = (String, List (String, String))


-- MAIN

