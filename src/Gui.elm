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
    , name_input: String 
    , key_input: String
    , value_input: String
    , output: String
    , equal_output: Bool
    , compare1: String
    , compare2: String
    }

init: Model
init = { memory= [( "dummy_table1" ,[ ("key1", "value1"), ("key2", "value2")]), ( "dummy_table2" ,[])]
    , name_input = ""
    , key_input = ""
    , value_input = ""
    , output = ""
    , equal_output = False
    , compare1 = ""
    , compare2 = ""}

-- UPDATE 
type Msg = Empty 
    | Insert
    | Show
    | ShowRep
    | IsIn
    | ValueOf
    | Remove
    | NameInput String
    | KeyInput String
    | ValueInput String
    | Compare
    | Compare1Input String
    | Compare2Input String
    | Card 
    | Dom

update : Msg -> Model -> Model
update msg model =
    case msg of
        Empty -> 
            { model | memory = empty model.name_input model.memory }

        Insert -> 
            { model | memory = insert model.name_input model.key_input model.value_input model.memory }
        
        NameInput name -> 
            { model | name_input = name }

        KeyInput key  -> 
            { model | key_input = key }

        ValueInput value  -> 
            { model | value_input = value }

        Compare1Input c1 -> 
            { model | compare1 = c1 }

        Compare2Input c2 -> 
            { model | compare2 = c2 }

        Compare -> 
            { model | equal_output = equal model.compare1 model.compare2 model.memory }

        Show -> 
            { model | output = show model.name_input model.memory }

        ShowRep -> 
            { model | output = showRep model.name_input model.memory }

        IsIn -> 
            { model | output = boolToString <| isIn model.name_input model.key_input model.memory }

        ValueOf -> 
            { model | output = value_of model.name_input model.key_input model.memory }

        Remove -> 
            { model | memory = remove model.name_input model.key_input model.memory }

        Card -> 
            { model | output = String.fromInt <| card model.name_input model.memory }

        Dom -> 
            { model | output = listToString identity (dom model.name_input model.memory) }

-- VIEW

view : Model -> Html Msg
view model =
    div [style "marginLeft" "20px"]
    [ h3 [][text "Praca so zoznamom"]
    , div [][text "Vstupy:"]
    , br [][]
    , viewInput "text" "Meno zoznamu" model.name_input NameInput
    , br [][]
    , viewInput "text" "Kluc prvku" model.key_input KeyInput
    , br [][]
    , viewInput "text" "Hodnota prvku" model.value_input ValueInput
    , br [][]
    , p [] [text <| "Vystup: " ++ model.output]
    , br [][]
    , button [ onClick Empty ] [ text "Empty" ]
    , br [][]
    , button [ onClick Insert ] [ text "Insert" ]
    , br [][]
    , button [ onClick Show ] [ text "Show" ]
    , br [][]
    , button [ onClick ShowRep ] [ text "ShowRep" ]
    , br [][]
    , button [ onClick IsIn ] [ text "IsIn" ]
    , br [][]
    , button [ onClick ValueOf ] [ text "ValueOf" ]
    , br [][]
    , button [ onClick Remove ] [ text "Remove" ]
    , br [][]
    , button [ onClick Card ] [ text "Card" ]
    , br [][]
    , button [ onClick Dom ] [ text "Dom" ]
    , br [][]
    , br [][]
    , div [][text "Definovane zoznamy: "]
    , viewNames model.memory
    , br [][]
    , br [][]
    ,h3 [][text "Porovnavanie zoznamov"]
    , div [][text "Vstupy:"]
    , br [][]
    , viewInput "text" "Zoznam 1" model.compare1 Compare1Input
    , br [][]
    , viewInput "text" "Zoznam 2" model.compare2 Compare2Input
    , br [][]
    , br [][]
    , button [ onClick Compare ] [ text "Porovnaj" ]
    , br [][]
    , br [][]
    , div [][text <| "Zadane zoznamy sa rovnaju: " ++ boolToString model.equal_output]

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

boolToString: Bool -> String
boolToString bool =
    if bool 
    then "Ano"
    else "Nie"