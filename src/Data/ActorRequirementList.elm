module Data.ActorRequirementList exposing
    ( ActorRequirementList
    , empty
    , fromDict
    , getArray
    , getRequirement
    , getRequirementsForActor
    , insertActor
    , insertRequirement
    , pushActor
    , pushRequirement
    , removeActor
    , removeRequirement
    , renameActor
    , sortActor
    , sortRequirements
    , updateRequirementContent
    )

import Array exposing (Array)
import Data.Actor as Actor exposing (Actor)
import Data.ArrayExtend as ArrayExtend
import Data.Requirement exposing (Requirement)
import Data.RequirementList as RequirementList exposing (RequirementList)
import Dict exposing (Dict)


type ActorRequirementList
    = ActorRequirementList
        (Array
            { actor : Actor
            , requirements : RequirementList
            }
        )


internalRequirementUpdate : Int -> (RequirementList -> RequirementList) -> ActorRequirementList -> ActorRequirementList
internalRequirementUpdate actorIndex listUpdator (ActorRequirementList array) =
    --do not expose this function
    case Array.get actorIndex array of
        Nothing ->
            ActorRequirementList array

        Just record ->
            ActorRequirementList
                (Array.set
                    actorIndex
                    { record | requirements = listUpdator record.requirements }
                    array
                )



-- exposed functions


empty : ActorRequirementList
empty =
    ActorRequirementList Array.empty


fromDict : Dict String (List String) -> ActorRequirementList
fromDict actorNamesAndRequirements =
    ActorRequirementList <|
        Dict.foldl
            (\actorName requirementContents array ->
                Array.push { actor = Actor.create actorName, requirements = RequirementList.fromListOfContents requirementContents } array
            )
            Array.empty
            actorNamesAndRequirements


getArray : ActorRequirementList -> Array ( String, Array String )
getArray list =
    case list of
        ActorRequirementList array ->
            Array.map (\elem -> ( Actor.name elem.actor, RequirementList.getArray elem.requirements )) array



-- actor functions


renameActor : Int -> String -> ActorRequirementList -> ActorRequirementList
renameActor index name (ActorRequirementList array) =
    Maybe.withDefault
        (ActorRequirementList array)
        (Array.get index array
            |> Maybe.map (\record -> { record | actor = Actor.rename name record.actor, requirements = record.requirements })
            |> Maybe.map (\updatedRecord -> ActorRequirementList <| Array.set index updatedRecord array)
        )


pushActor : Actor -> ActorRequirementList -> ActorRequirementList
pushActor actor (ActorRequirementList array) =
    ActorRequirementList <| Array.push { actor = actor, requirements = RequirementList.empty } array


insertActor : Int -> Actor -> ActorRequirementList -> ActorRequirementList
insertActor index actor (ActorRequirementList array) =
    ActorRequirementList <| ArrayExtend.insert index { actor = actor, requirements = RequirementList.empty } array


sortActor : Int -> Int -> ActorRequirementList -> ActorRequirementList
sortActor fromIndex toIndex (ActorRequirementList array) =
    ActorRequirementList <| ArrayExtend.sort fromIndex toIndex array


removeActor : Int -> ActorRequirementList -> ActorRequirementList
removeActor index (ActorRequirementList array) =
    ActorRequirementList <| ArrayExtend.remove index array



-- requirement functions


getRequirementsForActor : Int -> ActorRequirementList -> Maybe RequirementList
getRequirementsForActor actorIndex list =
    case list of
        ActorRequirementList array ->
            Array.get actorIndex array
                |> Maybe.map
                    (\record -> record.requirements)


getRequirement : ( Int, Int ) -> ActorRequirementList -> Maybe Requirement
getRequirement ( actorIndex, requirementIndex ) list =
    case list of
        ActorRequirementList array ->
            Array.get actorIndex array
                |> Maybe.andThen
                    (\record -> RequirementList.get requirementIndex record.requirements)


updateRequirementContent : ( Int, Int ) -> String -> ActorRequirementList -> ActorRequirementList
updateRequirementContent ( actorIndex, requirementIndex ) content list =
    internalRequirementUpdate
        actorIndex
        (RequirementList.updateContent requirementIndex content)
        list


pushRequirement : Int -> Requirement -> ActorRequirementList -> ActorRequirementList
pushRequirement actorIndex requirement list =
    internalRequirementUpdate
        actorIndex
        (RequirementList.push requirement)
        list


insertRequirement : ( Int, Int ) -> Requirement -> ActorRequirementList -> ActorRequirementList
insertRequirement ( actorIndex, requirementIndex ) requirement list =
    internalRequirementUpdate
        actorIndex
        (RequirementList.insert requirementIndex requirement)
        list


removeRequirement : ( Int, Int ) -> ActorRequirementList -> ActorRequirementList
removeRequirement ( actorIndex, requirementIndex ) list =
    internalRequirementUpdate
        actorIndex
        (RequirementList.remove requirementIndex)
        list


sortRequirements : Int -> Int -> Int -> ActorRequirementList -> ActorRequirementList
sortRequirements actorIndex fromRequirementIndex toRequirementIndex list =
    internalRequirementUpdate
        actorIndex
        (RequirementList.sort fromRequirementIndex toRequirementIndex)
        list
