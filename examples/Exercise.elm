module Exercise exposing (..)
import Html exposing (text)
main = text
    <| Debug.toString
    <| ( "sasa", "sas")
    -- <| preOrder
    -- <| Value 3 (Value 2 (Value 1 Empty Empty) Empty) (Value 5 Empty Empty)

type alias Item = ( String, String )

type alias Table = 
  
    
convIfEvenRecursive: List Int -> List (Int, Bool)
convIfEvenRecursive list = 
  case list of
    [] -> []
    first :: rest -> if modBy 2 first == 0
                     then (first, True) :: convIfEvenRecursive rest
                     else (first, False) :: convIfEvenRecursive rest
    
convIfEvenMap: List Int -> List (Int, Bool)
convIfEvenMap list = 
   List.map (\ e -> if modBy 2 e == 0 then (e, True) else (e, False)) list


betweenRecursive: number -> number -> List number -> List number
betweenRecursive min max list =
  case list of 
    [] -> []
    first :: rest -> if first > min && first < max 
                     then first :: betweenRecursive min max rest
                     else betweenRecursive min max rest
    
betweenIterative : number -> number -> List number -> List number
betweenIterative min max list =
  List.filter (\ e -> e > min && e < max) list 
  
type BVS number = Empty
                | Value number (BVS number) (BVS number)
                
depth : BVS number -> Int
depth bvs = 
  case bvs of 
    Empty -> 0
    Value _ l r  -> 1 + max (depth l ) (depth r)
    
count : BVS number -> Int
count bvs =
  case bvs of
    Empty -> 0
    Value _ l r -> 1 + (count l) + (count r)
    
member: number -> BVS number -> Bool
member num bvs =
  case bvs of
    Empty -> False
    Value c l r -> if c == num 
                   then True
                   else member num l || member num r
                   
insert: number -> BVS number -> BVS number
insert num bvs =
  case bvs of
    Empty -> Value num Empty Empty
    Value c l r -> if c > num 
                   then Value c (insert num l) r
                   else 
                     if c == num 
                     then bvs
                     else Value c l (insert num r)
                   
bvsfromListRecursive: List number -> BVS number
bvsfromListRecursive list =
  case list of 
    [] -> Empty
    first :: rest -> insert first (bvsfromListRecursive rest)
    
bvsfromListFunctional : List number -> BVS number
bvsfromListFunctional list = List.foldr insert Empty list 


preOrder: BVS number -> List number
preOrder bvs =
  case bvs of
    Empty -> []
    Value c l r -> (preOrder l) ++ [c] ++ (preOrder r)
