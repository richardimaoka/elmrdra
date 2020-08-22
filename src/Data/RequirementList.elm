module Data.RequirementList exposing (RequirementList, insert, push, remove, sort, toStringList, updateAt)

import Array exposing (Array)
import Data.ArrayExtend as ArrayExtend exposing (insert, remove, sort)
import Data.Requirement exposing (Requirement, text, update)


type RequirementList
    = RequirementList (Array Requirement)


toStringList : RequirementList -> List String
toStringList list =
    case list of
        RequirementList array ->
            Array.toList array |> List.map text


length : RequirementList -> Int
length list =
    -- do not expose this method
    case list of
        RequirementList array ->
            Array.length array


updateAt : Int -> String -> RequirementList -> RequirementList
updateAt index str list =
    case list of
        RequirementList array ->
            case Array.get index array of
                Nothing ->
                    list

                Just requirement ->
                    RequirementList <|
                        Array.set index (update str requirement) array


push : Requirement -> RequirementList -> RequirementList
push requirement list =
    case list of
        RequirementList array ->
            RequirementList <| Array.push requirement array


insert : Int -> Requirement -> RequirementList -> RequirementList
insert index requirement list =
    case list of
        RequirementList array ->
            RequirementList <| ArrayExtend.insert index requirement array


remove : Int -> RequirementList -> RequirementList
remove index list =
    case list of
        RequirementList array ->
            RequirementList <| ArrayExtend.remove index array


sort : Int -> Int -> RequirementList -> RequirementList
sort fromIndex toIndex list =
    case list of
        RequirementList array ->
            RequirementList <| ArrayExtend.sort fromIndex toIndex array
