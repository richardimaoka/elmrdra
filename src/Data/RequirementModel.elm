module Data.RequirementModel exposing (ActorRequirement)

import Array exposing (Array)
import Data.Actor exposing (Actor, name)
import Data.ActorRequirement exposing (ActorRequirement, toRecord)
import Data.Requirement exposing (Requirement, text, update)
import Data.RequirementList exposing (RequirementList, insert, push, remove, sort, toStringList, updateAt)


type RequirementModel
    = RequirementModel
        { actorRequirements : Array ActorRequirement
        , placeholderActors : Array Actor
        , placeholderRequirements : RequirementList
        }


toRecordList : RequirementModel -> List { actor : String, requirements : List String }
toRecordList requirementModel =
    case requirementModel of
        RequirementModel record ->
            record.actorRequirements |> Array.map toRecord |> Array.toList


updateRequirementAt : ( Int, Int ) -> String -> RequirementModel -> RequirementModel
updateRequirementAt ( actorIndex, requirementIndex ) str requirementModel =
    case requirementModel of
        RequirementModel record ->
            case Array.get actorIndex record.actorRequirements of
                Nothing ->
                    requirementModel

                Just actorRequirement ->
                    { requirementModel
                        | actorRequirements = Array.set actorIndex (updateAt requirementIndex str actorRequirement)
                    }


pushRequirement : Requirement -> ActorRequirement -> ActorRequirement
pushRequirement requirement actorRequirement =
    case actorRequirement of
        ActorRequirement record ->
            ActorRequirement { record | requirements = push requirement record.requirements }


insertRequirement : Int -> Requirement -> ActorRequirement -> ActorRequirement
insertRequirement index requirement actorRequirement =
    case actorRequirement of
        ActorRequirement record ->
            ActorRequirement { record | requirements = insert index requirement record.requirements }


removeRequirement : Int -> ActorRequirement -> ActorRequirement
removeRequirement index actorRequirement =
    case actorRequirement of
        ActorRequirement record ->
            ActorRequirement { record | requirements = remove index record.requirements }


sortRequirements : Int -> Int -> ActorRequirement -> ActorRequirement
sortRequirements fromIndex toIndex list =
    case list of
        ActorRequirement record ->
            ActorRequirement { record | requirements = sort fromIndex toIndex record.requirements }
