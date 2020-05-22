import Html exposing (text)
main = text
    <| Debug.toString
    <| Header  "tabulka1"

-- One item in table -> Tuple of key and value
type alias Item = ( String, String )

-- Header of table, contains usefull information
type alias Header = {
    name: String
}

-- Table type -> Consists of Header and List of Items
type alias Table = (
    Header, 
    List Item
)