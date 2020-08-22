module Data.ActorList exposing (ActorList, insert, push, remove, set, sort)

import Array exposing (Array)
import Data.Actor exposing (Actor)
import Data.ArrayExtend as ArrayExtend exposing (insert, remove, sort)


type ActorList
    = ActorList (Array Actor)


internalUpdate : (Array Actor -> Array Actor) -> ActorList -> ActorList
internalUpdate updator (ActorList array) =
    --do not expose this function
    ActorList (updator array)


set : Int -> Actor -> ActorList -> ActorList
set index actor list =
    internalUpdate (Array.set index actor) list


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
