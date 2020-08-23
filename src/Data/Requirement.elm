module Data.Requirement exposing (Requirement, create, text, updateContent)


type Requirement
    = Requirement String


create : String -> Requirement
create contentStr =
    Requirement contentStr


updateContent : String -> Requirement -> Requirement
updateContent str _ =
    Requirement str


text : Requirement -> String
text (Requirement str) =
    str
