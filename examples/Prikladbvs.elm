-- Priklad prace s udajovym typom binarny vyhladavaci strom.
-- Ivan Kapustik


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = (Stavy, List (String, BVS String))

type BVS string = EmptyBVS | Node String (BVS string)(BVS string)

type alias Stavy =
  { meno : String    -- Meno stromu
  , prvok : String   -- Hodnota prvku (pre insert) alebo meno druheho stromu pre append
  , zoznam : String  -- strom pre fromList
  , vystup : String  -- Vystup, generovany tlacidlom
  }
  

init : Model
init = (Stavy "" "" "" "", [("a", Node "b" EmptyBVS EmptyBVS)
                           ,("b", Node "4" (Node "1" EmptyBVS EmptyBVS) 
                                           (Node "7" EmptyBVS EmptyBVS) )
                           ])


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
      then (Stavy "" "" "" "Treba vlozit meno noveho stromu", li)
      else (Stavy "" "" "" ("Vytvoreny novy strom: " ++ st.meno)
            , empty st.meno li)

    Vypis ->
      (Stavy "" "" "" (if st.meno == "" 
                       then "Treba vlozit meno stromu" 
                       else if List.member st.meno (List.map Tuple.first li)
                       then "Hodnota stromu " ++ st.meno ++ " je " ++ vypis st.meno li
                       else "Neznamy strom "  ++ st.meno)
       , li)


-- VIEW


view : Model -> Html Msg
view (st, li) =
  div [style "marginLeft" "20px"]
    [ h3 [][text "Praca so stromom"]
    , div [][text "Vstupy:"]
    , viewInput "text" "Meno stromu" st.meno Meno
    , br [][]
    , viewInput "text" "Prvok/druhy strom" st.prvok Prvok
    , br [][]
    , viewInput "text" "Zoznam prvkov" st.zoznam Zoznam
    , br [][]
    , br [][]
    , button [ onClick Empty ] [ text "Empty" ]
    , button [ onClick Vypis ] [ text "Vypis" ]
    , br [][]
    , br [][]
    , div [][text "Definovane stromy: "]
    , viewNames li
    , p [] [text st.vystup]
    ]


-- POMOCNE FUNKCIE

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewNames : List (String, BVS String) -> Html msg
viewNames lists =
  case lists of
    [] ->
      div [ style "color" "red" ] [ text "Ziadny strom nie je definovany." ]
    _ :: _ ->
      div [][text (listToString identity (List.map Tuple.first lists))]


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


treeToString tree =
  case tree of
    EmptyBVS -> "EmptyBVS"
    Node val l r ->
      "Node " ++ val ++ " (" ++ treeToString l ++ " " ++ treeToString r ++ ")"


-- FUNKCIE K UDAJOVEMU TYPU

empty : String -> List (String, BVS String) -> List (String, BVS String)
empty meno list =
  case list of
    [] -> [(meno, EmptyBVS)]
    (mBVS, valBVS) :: rest ->
      if mBVS == meno
      then (meno, EmptyBVS) :: rest
      else (mBVS, valBVS) :: empty meno rest


vypis : String -> List (String, BVS String) -> String
vypis meno lists =
 case lists of
   [] -> ""
   (mBVS, valBVS) :: rest ->
     if mBVS == meno
     then treeToString valBVS
     else vypis meno rest


