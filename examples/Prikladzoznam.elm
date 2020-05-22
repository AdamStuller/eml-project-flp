-- Priklad prace s udajovym typom zoznam.
-- Ivan Kapustik


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = (Stavy, List (List String))

type alias Stavy =
  { meno : String    -- Meno zoznamu
  , prvok : String   -- Hodnota prvku (pre insert) alebo meno druheho zoznamu pre append
  , zoznam : String  -- zoznam pre fromList
  , vystup : String  -- Vystup, generovany tlacidlom
  }
  

init : Model
init = (Stavy "" "" "" "", [["a","a","b","c"],["b","a","b","c"]])


-- UPDATE

type Msg
      -- Vstupy
  = Meno String
  | Prvok String
  | Zoznam String
      -- Tlacidla
  | Empty
  | Vypis


update : Msg -> Model -> Model
update msg (st, li) =
  case msg of
    Meno text ->
      ({ st | meno = text, vystup = "" }, li)

    Prvok text ->
      ({ st | prvok = text, vystup = "" }, li)

    Zoznam text ->
      ({ st | zoznam = text, vystup = "" }, li)

    Empty ->
      if st.meno == "" 
      then (Stavy "" "" "" "Treba vlozit meno noveho zoznamu", li)
      else (Stavy "" "" "" ("Vytvoreny novy zoznam: " ++ st.meno)
            , empty st.meno li)

    Vypis ->
      (Stavy "" "" "" (if st.meno == "" 
                       then "Treba vlozit meno zoznamu" 
                       else if List.member st.meno (getFirsts li)
                       then "Hodnota zoznamu " ++ st.meno ++ " je " ++ vypis st.meno li
                       else "Neznamy zoznam "  ++ st.meno)
       , li)


-- VIEW


view : Model -> Html Msg
view (st, li) =
  div [style "marginLeft" "20px"]
    [ h3 [][text "Praca so zoznamom"]
    , div [][text "Vstupy:"]
    , viewInput "text" "Meno zoznamu" st.meno Meno
    , br [][]
    , viewInput "text" "Prvok/druhy zoznam" st.prvok Prvok
    , br [][]
    , viewInput "text" "Zoznam prvkov" st.zoznam Zoznam
    , br [][]
    , br [][]
    , button [ onClick Empty ] [ text "Empty" ]
    , button [ onClick Vypis ] [ text "Vypis" ]
    , br [][]
    , br [][]
    , div [][text "Definovane zoznamy: "]
    , viewNames li
--    , div [][text <| listToString identity (List.map (listToString identity) li)]
    , p [] [text st.vystup]
    ]


-- POMOCNE FUNKCIE

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewNames : List (List String) -> Html msg
viewNames lists =
  case lists of
    [] ->
      div [ style "color" "red" ] [ text "Ziadny zoznam nie je definovany." ]
    _ :: _ ->
      div [][text (listToString identity (getFirsts lists))]


getFirsts : List (List a) -> List a
getFirsts list =
  case list of
    [] -> []
    first1 :: rest1 ->
      case first1 of
        [] -> getFirsts rest1
        first :: _ -> first :: getFirsts rest1


listToString : (a -> String) -> List a -> String
listToString fun list =
  let
    lts f li =
      case li of
        [] -> ""
        first :: rest ->
          (f first)
          ++ if rest == [] then "" else ", "
          ++ lts f rest
  in
    "[" ++ (lts fun list) ++ "]"


-- FUNKCIE K UDAJOVEMU TYPU

empty : String -> List (List String) -> List (List String)
empty meno list =
  case list of
    [] -> [[meno]]
    first1 :: rest1 ->
      case first1 of
        [] -> empty meno rest1
        first :: _ ->
          if first == meno
          then [meno] :: rest1
          else first1 :: empty meno rest1


vypis : String -> List (List String) -> String
vypis meno lists =
 case lists of
   [] -> ""
   first1 :: rest1 ->
     case first1 of
       [] -> vypis meno rest1
       first :: rest ->
         if first == meno
         then listToString identity rest
         else vypis meno rest1


