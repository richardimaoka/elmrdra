module Data.ActorRequirementList exposing (ActorRequirementList, insert, push, remove, set, sort)

import Array exposing (Array)
import Data.Actor exposing (Actor)
import Data.ArrayExtend as ArrayExtend
import Data.Requirement exposing (Requirement)


type ActorRequirementList
    = ActorRequirementList
        (Array
            { actor : Actor
            , requirements : Array Requirement
            }
        )


internalUpdate : Int -> (Array Requirement -> Array Requirement) -> ActorRequirementList -> ActorRequirementList
internalUpdate actorIndex updator (ActorRequirementList array) =
    --do not expose this function
    case Array.get actorIndex array of
        Nothing ->
            ActorRequirementList array

        Just record ->
            ActorRequirementList
                (Array.set
                    actorIndex
                    { record | requirements = updator record.requirements }
                    array
                )


set : ( Int, Int ) -> Requirement -> ActorRequirementList -> ActorRequirementList
set ( actorIndex, requirementIndex ) requirement list =
    internalUpdate actorIndex (Array.set requirementIndex requirement) list


push : Int -> Requirement -> ActorRequirementList -> ActorRequirementList
push actorIndex requirement list =
    internalUpdate actorIndex (Array.push requirement) list


insert : ( Int, Int ) -> Requirement -> ActorRequirementList -> ActorRequirementList
insert ( actorIndex, requirementIndex ) requirement list =
    internalUpdate actorIndex (ArrayExtend.insert requirementIndex requirement) list


remove : ( Int, Int ) -> ActorRequirementList -> ActorRequirementList
remove ( actorIndex, requirementIndex ) list =
    internalUpdate actorIndex (ArrayExtend.remove requirementIndex) list


sort : Int -> Int -> Int -> ActorRequirementList -> ActorRequirementList
sort actorIndex fromRequirementIndex toRequirementIndex list =
    internalUpdate actorIndex (ArrayExtend.sort fromRequirementIndex toRequirementIndex) list
