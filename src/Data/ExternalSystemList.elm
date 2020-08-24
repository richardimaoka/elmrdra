module Data.ExternalSystemList exposing
    ( ExternalSystemList
    , append
    , empty
    , fromListOfNames
    , get
    , getArray
    , insert
    , push
    , remove
    , rename
    , sort
    )

import Array exposing (Array)
import Data.ArrayExtend as ArrayExtend exposing (insert, remove, sort)
import Data.ExternalSystem as ExternalSystem exposing (ExternalSystem)


type ExternalSystemList
    = ExternalSystemList (Array ExternalSystem)


internalUpdate : (Array ExternalSystem -> Array ExternalSystem) -> ExternalSystemList -> ExternalSystemList
internalUpdate arrayUpdater (ExternalSystemList array) =
    --do not expose this function
    ExternalSystemList (arrayUpdater array)



-- exposed functions


empty : ExternalSystemList
empty =
    ExternalSystemList Array.empty


get : Int -> ExternalSystemList -> Maybe ExternalSystem
get index list =
    case list of
        ExternalSystemList array ->
            Array.get index array


getArray : ExternalSystemList -> Array String
getArray list =
    case list of
        ExternalSystemList array ->
            Array.map (\elem -> ExternalSystem.name elem) array


fromListOfNames : List String -> ExternalSystemList
fromListOfNames list =
    ExternalSystemList <| (Array.fromList list |> Array.map ExternalSystem.create)


rename : Int -> String -> ExternalSystemList -> ExternalSystemList
rename index content list =
    internalUpdate (ArrayExtend.update index (ExternalSystem.rename content)) list


push : ExternalSystem -> ExternalSystemList -> ExternalSystemList
push requirement list =
    internalUpdate (Array.push requirement) list


insert : Int -> ExternalSystem -> ExternalSystemList -> ExternalSystemList
insert index requirement list =
    internalUpdate (ArrayExtend.insert index requirement) list


remove : Int -> ExternalSystemList -> ExternalSystemList
remove index list =
    internalUpdate (ArrayExtend.remove index) list


sort : Int -> Int -> ExternalSystemList -> ExternalSystemList
sort fromIndex toIndex list =
    internalUpdate (ArrayExtend.sort fromIndex toIndex) list


append : ExternalSystemList -> ExternalSystemList -> ExternalSystemList
append (ExternalSystemList array1) (ExternalSystemList array2) =
    ExternalSystemList <| Array.append array1 array2
