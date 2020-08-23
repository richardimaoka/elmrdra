module Data.ActorList exposing
    ( ActorList
    , empty
    , fromListOfNames
    , get
    , insert
    , push
    , remove
    , rename
    , sort
    )

import Array exposing (Array)
import Data.Actor as Actor exposing (Actor)
import Data.ArrayExtend as ArrayExtend exposing (insert, remove, sort)


type ActorList
    = ActorList (Array Actor)


empty : ActorList
empty =
    ActorList Array.empty


fromListOfNames : List String -> ActorList
fromListOfNames list =
    ActorList <| (Array.fromList list |> Array.map Actor.create)


internalUpdate : (Array Actor -> Array Actor) -> ActorList -> ActorList
internalUpdate arrayUpdater (ActorList array) =
    --do not expose this function
    ActorList (arrayUpdater array)


get : Int -> ActorList -> Maybe Actor
get index list =
    case list of
        ActorList array ->
            Array.get index array


rename : Int -> String -> ActorList -> ActorList
rename index name list =
    internalUpdate (ArrayExtend.update index (Actor.rename name)) list


push : Actor -> ActorList -> ActorList
push actor list =
    internalUpdate (Array.push actor) list


insert : Int -> Actor -> ActorList -> ActorList
insert index actor list =
    internalUpdate (ArrayExtend.insert index actor) list


remove : Int -> ActorList -> ActorList
remove index list =
    internalUpdate (ArrayExtend.remove index) list


sort : Int -> Int -> ActorList -> ActorList
sort fromIndex toIndex list =
    internalUpdate (ArrayExtend.sort fromIndex toIndex) list
