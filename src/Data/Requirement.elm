module Data.Requirement exposing (Requirement, text, update)


type Requirement
    = Requirement String


update : String -> Requirement -> Requirement
update str _ =
    Requirement str


text : Requirement -> String
text (Requirement str) =
    str
