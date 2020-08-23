module Data.Requirement exposing (Requirement, text, updateContent)


type Requirement
    = Requirement String


updateContent : String -> Requirement -> Requirement
updateContent str _ =
    Requirement str


text : Requirement -> String
text (Requirement str) =
    str
