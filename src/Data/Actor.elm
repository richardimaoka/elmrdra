module Data.Actor exposing (Actor, name, rename)


type Actor
    = Actor String


rename : String -> Actor -> Actor
rename str _ =
    Actor str


name : Actor -> String
name (Actor str) =
    str
