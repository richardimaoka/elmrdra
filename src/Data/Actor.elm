module Data.Actor exposing (Actor, create, name, rename)


type Actor
    = Actor String


create : String -> Actor
create actorName =
    Actor actorName


rename : String -> Actor -> Actor
rename str _ =
    Actor str


name : Actor -> String
name (Actor str) =
    str
