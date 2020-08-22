module Data.ActorRequirementList exposing (ActorRequirementList, insert, push, remove, set, sort)

import Array exposing (Array)
import Data.Actor exposing (Actor)
import Data.Requirement exposing (Requirement)
import Data.RequirementList as RequirementList exposing (RequirementList)


type ActorRequirementList
    = ActorRequirementList
        (Array
            { actor : Actor
            , requirements : RequirementList
            }
        )


internalUpdate : Int -> (RequirementList -> RequirementList) -> ActorRequirementList -> ActorRequirementList
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
    internalUpdate
        actorIndex
        (RequirementList.set requirementIndex requirement)
        list


push : Int -> Requirement -> ActorRequirementList -> ActorRequirementList
push actorIndex requirement list =
    internalUpdate
        actorIndex
        (RequirementList.push requirement)
        list


insert : ( Int, Int ) -> Requirement -> ActorRequirementList -> ActorRequirementList
insert ( actorIndex, requirementIndex ) requirement list =
    internalUpdate
        actorIndex
        (RequirementList.insert requirementIndex requirement)
        list


remove : ( Int, Int ) -> ActorRequirementList -> ActorRequirementList
remove ( actorIndex, requirementIndex ) list =
    internalUpdate
        actorIndex
        (RequirementList.remove requirementIndex)
        list


sort : Int -> Int -> Int -> ActorRequirementList -> ActorRequirementList
sort actorIndex fromRequirementIndex toRequirementIndex list =
    internalUpdate
        actorIndex
        (RequirementList.sort fromRequirementIndex toRequirementIndex)
        list
