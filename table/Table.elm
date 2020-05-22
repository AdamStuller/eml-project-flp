import Html exposing (text)
main = text
    <| Debug.toString
    <| empty "margaretina_tabulka" [({ name = "marosova_tabulka" },[("key", "value")]), ({ name = "adamova_tabulka" },[("key", "value")])]



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

is_table: String -> Table -> Bool
is_table name table =
    (Tuple.first table).name == name 

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
    

-- get_table: String -> Memory -> Maybe Table
-- get_table name memory =
--     case memory of 
--         [] -> Nothing
--         first :: rest -> 
--             if (Tuple.first first).name == name     


-- insert: String -> String -> String -> Table
-- insert name key value memory =
--     case memory of
--         [] -> []
--         first :: rest -> 
--             if (Tuple.first first).name == name 

