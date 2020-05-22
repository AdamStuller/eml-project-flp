module Projekt exposing (..)
import Table exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }




-- MODEL
type alias Model = 
    { memory: Memory
    , curr_name: String 
    , output: String
    }

init: Model
init = { memory= [( "marosova_tabulka" ,[ ("Johan", "skala"), ("key", "value")])]
    , curr_name = ""
    , output = ""}

-- UPDATE 
type Msg = Empty String
    | Name String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Empty name -> 
            { model | memory = empty name model.memory }
        
        Name name -> 
            { model | curr_name = name }

-- VIEW

view : Model -> Html Msg
view model =
    div [style "marginLeft" "20px"]
    [ h3 [][text "Praca so zoznamom"]
    , div [][text "Vstupy:"]
    , viewInput "text" "Meno zoznamu" model.curr_name Name
    , br [][]
    , br [][]
    , div [][text "Definovane zoznamy: "]
    , viewNames model.memory
    ]


----------------------- Helper functions --------------------

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewNames : Memory -> Html msg
viewNames memory =
  case memory of
    [] ->
      div [ style "color" "red" ] [ text "Ziadny zoznam nie je definovany." ]
    _ :: _ ->
      div [][text (listToString identity (get_names memory))]


listToString : (a -> String) -> List a -> String
listToString fun list =
  let
    lts f li =
      case li of
        [] -> ""
        first :: rest ->
          f first
          ++ if rest == [] then "" else ", "
          ++ lts f rest
  in
    "[ " ++ lts fun list ++ " ]"