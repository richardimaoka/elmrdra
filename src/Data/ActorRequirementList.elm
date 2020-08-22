module Data.ActorRequirementList exposing (ActorRequirementList)

import Array exposing (Array)
import Data.Actor exposing (Actor)
import Data.Requirement exposing (Requirement)
import Data.RequirementList exposing (RequirementList, insert, push, remove, sort, updateAt)


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


updateRequirementAt : ( Int, Int ) -> String -> ActorRequirementList -> ActorRequirementList
updateRequirementAt ( actorIndex, requirementIndex ) str list =
    internalUpdate actorIndex (updateAt requirementIndex str) list


pushRequirement : Int -> Requirement -> ActorRequirementList -> ActorRequirementList
pushRequirement actorIndex requirement list =
    internalUpdate actorIndex (push requirement) list


insertRequirement : ( Int, Int ) -> Requirement -> ActorRequirementList -> ActorRequirementList
insertRequirement ( actorIndex, requirementIndex ) requirement list =
    internalUpdate actorIndex (insert requirementIndex requirement) list


removeRequirement : ( Int, Int ) -> ActorRequirementList -> ActorRequirementList
removeRequirement ( actorIndex, requirementIndex ) list =
    internalUpdate actorIndex (remove requirementIndex) list


sortRequirements : Int -> Int -> Int -> ActorRequirementList -> ActorRequirementList
sortRequirements actorIndex fromRequirementIndex toRequirementIndex list =
    internalUpdate actorIndex (sort fromRequirementIndex toRequirementIndex) list
