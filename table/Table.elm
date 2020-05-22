import Html exposing (text)
main = text
    <| Debug.toString
    -- <| remove "marosova_tabulka"  "Johan"
    <| dom "adamova_tabulka"
    -- <| showRep
    -- <| get_table "marosova_tabulka"
    -- <| empty "adamova_tabulka"
    <| insert "marosova_tabulka" "mama" "maros" 
    <| insert "marosova_tabulka" "mama" "ema" 
    <| [({ name = "marosova_tabulka" },[("key", "value"), ("Johan", "skala")]), ({ name = "adamova_tabulka" },[("key", "value")])]



-- Model for application 


-- One item in table -> Tuple of key and value
type alias Item = ( String, String )

-- Header of table, contains usefull information
type alias Header = { name: String }  

-- Table type -> Consists of Header and List of Items
type alias Table = ( Header, List Item )

-- All tables, this is modified by all functions
type alias Memory = List Table

-- Functions for data type

-- Checks if given [table] has given [name]
is_table: String -> Table -> Bool
is_table name table =
    (Tuple.first table).name == name 

-- Returns header from table
get_header: Table -> Header
get_header table =
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


------------------------- Empty ---------------------------------------------------
-- Takes name of table, memory with tables and returns new memory, with empty table with [name]
-- If table is present, new empty table replaces it. If not present new table is added.
empty: String -> Memory -> Memory
empty name memory =
    case memory of
        [] -> [(Header name, [])]
        first :: rest -> 
            if is_table name first 
            then (Header name, []) :: rest
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
            then ( get_header first, insert_to_items key value (get_items first) ) :: rest
            else first:: insert name key value rest

-------------------------------------------------------------------
   
----------------------- show ad showRep --------------------------------------

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
            (show_function first) ++ show_items  show_function rest

-- Transforms header to string, shows only name of table
show_header: Header -> String
show_header header =
    header.name

------------------------- main functions------------------------------------

-- Transforms table to string. Takes table of Nothing (get_table returns Maybe Table)
-- Show only items, naive representation
show: Maybe Table -> String
show table = 
    case table of
        Nothing -> ""
        Just t -> 
            show_items show_item_dummy
            <| get_items t

-- Transforms table to string. Takes table of Nothing (get_table returns Maybe Table)
-- Shows header as well, real representation
showRep:  Maybe Table -> String
showRep table =
    case table of
        Nothing -> ""
        Just t -> 
            "( \"" ++ show_header (get_header t) ++ "\", [" ++ show_items show_item_real (get_items t) ++ " ])" 
        
----------------------------------------------------------------------------------

------------------------------ isIn ----------------------------------------------

-- Checks if [key] is present list of [items]
is_in_items: String -> List Item -> Bool
is_in_items key items =
    case items of 
        [] -> False
        (key1,value)::rest ->
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
value_from_items: String -> List Item -> Maybe String
value_from_items key items =
    case items of
        [] -> Nothing
        (key1,v)::rest ->
            if key1 == key 
            then Just v
            else value_from_items key rest

-- Gets value belongin to [key] in table with [name] from [memory] or Nothing if either no such table or key in table
value_of: String -> String -> Memory -> Maybe String
value_of name key memory =
    let 
        table = get_table name memory
    in
        case table of
            Nothing -> Nothing
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
                then ( get_header first, remove_from_items key (get_items first) ) :: rest
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
            Just t -> List.map (\ (key, value)  -> key) ( get_items t )

--------------------------------------------------------------------------------------

