module Data.RequirementList exposing (RequirementList, insert, push, remove, set, sort)

import Array exposing (Array)
import Data.ArrayExtend as ArrayExtend exposing (insert, remove, sort)
import Data.Requirement exposing (Requirement)


type RequirementList
    = RequirementList (Array Requirement)


internalUpdate : (Array Requirement -> Array Requirement) -> RequirementList -> RequirementList
internalUpdate updator (RequirementList array) =
    --do not expose this function
    RequirementList (updator array)


set : Int -> Requirement -> RequirementList -> RequirementList
set index requirement list =
    internalUpdate (Array.set index requirement) list


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
