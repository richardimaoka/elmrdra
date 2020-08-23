module Data.RequirementList exposing
    ( RequirementList
    , append
    , empty
    , fromListOfContents
    , get
    , insert
    , push
    , remove
    , sort
    , updateContent
    )

import Array exposing (Array)
import Data.ArrayExtend as ArrayExtend exposing (insert, remove, sort)
import Data.Requirement as Requirement exposing (Requirement)


type RequirementList
    = RequirementList (Array Requirement)


empty : RequirementList
empty =
    RequirementList Array.empty


get : Int -> RequirementList -> Maybe Requirement
get index list =
    case list of
        RequirementList array ->
            Array.get index array


fromListOfContents : List String -> RequirementList
fromListOfContents list =
    RequirementList <| (Array.fromList list |> Array.map Requirement.create)


internalUpdate : (Array Requirement -> Array Requirement) -> RequirementList -> RequirementList
internalUpdate arrayUpdater (RequirementList array) =
    --do not expose this function
    RequirementList (arrayUpdater array)


updateContent : Int -> String -> RequirementList -> RequirementList
updateContent index content list =
    internalUpdate (ArrayExtend.update index (Requirement.updateContent content)) list


push : Requirement -> RequirementList -> RequirementList
push requirement list =
    internalUpdate (Array.push requirement) list


insert : Int -> Requirement -> RequirementList -> RequirementList
insert index requirement list =
    internalUpdate (ArrayExtend.insert index requirement) list


remove : Int -> RequirementList -> RequirementList
remove index list =
    internalUpdate (ArrayExtend.remove index) list


sort : Int -> Int -> RequirementList -> RequirementList
sort fromIndex toIndex list =
    internalUpdate (ArrayExtend.sort fromIndex toIndex) list


append : RequirementList -> RequirementList -> RequirementList
append (RequirementList array1) (RequirementList array2) =
    RequirementList <| Array.append array1 array2
