module Table exposing (Memory, Table, empty, insert, show, showRep, isIn, value_of, remove, card, dom, equal, get_names)
import Set

-- One item in table -> Tuple of key and value
type alias Item = ( String, String )

-- Name of table
type alias Name = String 

-- Table type -> Consists of Name and List of Items
type alias Table = ( Name, List Item )

-- All tables, this is modified by all functions
type alias Memory = List Table

-- Functions for data type

-- Checks if given [table] has given [name]
is_table: String -> Table -> Bool
is_table name table =
    Tuple.first table == name 

-- Returns header from table
get_name: Table -> Name
get_name table =
    Tuple.first table

-- Returns items from table
get_items: Table -> List Item
get_items table =
    Tuple.second table

-- Takes [name] of searched for [table] and whole memory, returns [table] if there or Nothing if not
get_table: String -> Memory -> Maybe Table
get_table name memory =
    case memory of 
        [] -> Nothing
        first :: rest -> 
            if is_table name first
            then Just first
            else get_table name rest   

-- Maps tables from memory to list of names
get_names: Memory -> List String
get_names memory =
    List.map (\ table -> get_name table ) memory

------------------------- Empty ---------------------------------------------------
-- Takes name of table, memory with tables and returns new memory, with empty table with [name]
-- If table is present, new empty table replaces it. If not present new table is added.
empty: String -> Memory -> Memory
empty name memory =
    case memory of
        [] -> [( name, [])]
        first :: rest -> 
            if is_table name first 
            then ( name, []) :: rest
            else first :: empty name rest

------------------------------------------------------------------------------------
    
-------------------------- insert -----------------------------
-- Helper function
-- Inserts new entry to items in table, tables [items] and [key] with [value].
-- Returns new list of items 
insert_to_items:  String -> String -> List Item -> List Item
insert_to_items  key value items =
    case items of
        [] -> [(key, value)] 
        first :: rest -> 
            if Tuple.first first == key
            then (key, value) :: rest 
            else first :: insert_to_items key value rest

-- Inserts item into given memory, takes [name] of table, [key] nad [value] for entry and [memory]
insert: String -> String -> String -> Memory -> Memory
insert name key value memory =
    case memory of 
        [] -> []
        first :: rest -> 
            if is_table name first
            then ( get_name first, insert_to_items key value (get_items first) ) :: rest
            else first:: insert name key value rest

-------------------------------------------------------------------
   
----------------------- show and showRep --------------------------------------

-------------------------- helpers -----------------------------------------

-- Transforms [item] to string. Pattern: key: value 
show_item_dummy: Item -> String
show_item_dummy ( key, value ) =
    key ++ ": " ++  value ++ ", "

-- Transforms [item] to string. Pattern: ( "key": "value")
show_item_real: Item -> String
show_item_real ( key, value ) =
    "( \"" ++ key ++ "\", \"" ++ value ++ "\" )," 

-- Transforms [items] from list of items to string. Pattern is  set by [show_function]
show_items:  ( Item -> String ) ->  List Item -> String
show_items show_function items = 
    case items of
        [] -> ""
        first::rest ->
            show_function first ++ show_items  show_function rest

------------------------- main functions------------------------------------

-- Transforms table to string. Takes name of table in memory. If no table with such name in memory "" is returned.
-- Show only items, naive representation
show: String -> Memory -> String
show name memory = 
    let
        table = get_table name memory
    in 
        case table of
            Nothing -> ""
            Just t -> 
                show_items show_item_dummy
                <| get_items t

-- Transforms table to string. Takes name of table in memory. If no table with such name in memory "" is returned.
-- Shows header as well, real representation
showRep: String -> Memory -> String
showRep name memory = 
    let
        table = get_table name memory
    in 
        case table of
            Nothing -> ""
            Just t -> 
                "( \"" ++ get_name t ++ "\", [" ++ show_items show_item_real (get_items t) ++ " ])" 
        
----------------------------------------------------------------------------------

------------------------------ isIn ----------------------------------------------

-- Checks if [key] is present list of [items]
is_in_items: String -> List Item -> Bool
is_in_items key items =
    case items of 
        [] -> False
        (key1,_)::rest ->
            if key1 == key
            then True
            else is_in_items key rest

-- Checks if [key] is in table with [name]
isIn: String -> String -> Memory -> Bool
isIn name key memory =
    let 
        table = get_table name memory
    in
        case table of
            Nothing -> False
            Just t -> is_in_items key (get_items t)

----------------------------- value ---------------------------------------------

-- Gets value belonging to [key] from [items] or Nothing, if no such key in items
value_from_items: String -> List Item -> String
value_from_items key items =
    case items of
        [] -> ""
        (key1,v)::rest ->
            if key1 == key 
            then v
            else value_from_items key rest

-- Gets value belongin to [key] in table with [name] from [memory] or Nothing if either no such table or key in table
value_of: String -> String -> Memory -> String
value_of name key memory =
    let 
        table = get_table name memory
    in
        case table of
            Nothing -> ""
            Just t -> value_from_items key (get_items t)
            

--------------------------------------------------------------------------------
------------------------- Remove ----------------------------------------------

-- Removes item identified by [key] from [items] if present
remove_from_items: String -> List Item -> List Item
remove_from_items key items =
    case items of   
        [] -> []
        (key1,value)::rest ->  
            if  key == key1
            then remove_from_items key rest
            else (key1,value) :: remove_from_items key rest

-- removes item identified by [key] from table named [name] from [memory]
remove: String -> String -> Memory -> Memory
remove name key memory =
    case memory of 
            [] -> []
            first :: rest -> 
                if is_table name first
                then ( get_name first, remove_from_items key (get_items first) ) :: rest
                else first :: remove name key rest

-----------------------------------------------------------------------------------

------------------------------ card --------------------------------------------

-- Returns number of items in table identified by [name] in [memory]
card: String -> Memory -> Int
card name memory =
    let 
        table = get_table name memory
    in  
        case table of
            Nothing -> 0
            Just t -> 
                List.length (get_items t)

------------------------------------------------------------------------------------

--------------------------------- dom ---------------------------------------------

-- Gets list of keys from table by [name]
dom: String -> Memory -> List String
dom name memory =
    let 
        table = get_table  name memory
    in  
        case table of 
            Nothing -> []
            Just t -> List.map (\ (key, _)  -> key) ( get_items t )

--------------------------------------------------------------------------------------

-------------------------------- equal --------------------------------------------

-- Compares two lists of items. Uses Set.
equal_list: List Item -> List Item -> Bool
equal_list items1 items2 =
    let 
        len1 = List.length items1
        len2 = List.length items2
    in
        if len1 > len2 
        then Set.isEmpty <| Set.diff (Set.fromList items1) (Set.fromList items2)
        else Set.isEmpty <| Set.diff (Set.fromList items2) (Set.fromList items1)
        

-- Compares two tables from [memory]. If either of tables with submitted [names] not present in memory, returns False. 
equal: String -> String -> Memory -> Bool
equal name1 name2 memory =
    let 
        table1 = get_table name1 memory
        table2 = get_table name2 memory
    in
        case table1 of  
            Nothing -> False
            Just t1 ->
                case table2 of 
                    Nothing -> False
                    Just t2 -> equal_list (get_items t1) (get_items  t2)

----------------------------------------------------------------------------------------
