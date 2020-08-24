module Data.ExternalSystem exposing (ExternalSystem, create, name, rename)


type ExternalSystem
    = ExternalSystem String


create : String -> ExternalSystem
create externalSystemName =
    ExternalSystem externalSystemName


rename : String -> ExternalSystem -> ExternalSystem
rename str _ =
    ExternalSystem str


name : ExternalSystem -> String
name (ExternalSystem str) =
    str
