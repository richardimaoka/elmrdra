module Data.RequirementModel exposing
    ( RequirementModel
    , dropInActor
    , dropInRequirement
    , dropOffRequirement
    , empty
    , initialize
    , pushActor
    , pushPlaceHolderActor
    , pushPlaceHolderRequirement
    , pushRequirement
    , removeActor
    , removePlaceHolderActor
    , removePlaceHolderRequirement
    , removeRequirement
    , renameActor
    , renamePlaceHolderActor
    , sortActor
    , sortPlaceHolderActor
    , sortPlaceHolderRequirement
    , sortRequirement
    , updatePlaceHolderRequirementContent
    , updateRequirementContent
    )

import Data.Actor as Actor exposing (Actor)
import Data.ActorList as ActorList exposing (ActorList)
import Data.ActorRequirementList as ActorRequirementList exposing (ActorRequirementList, empty)
import Data.Requirement exposing (Requirement)
import Data.RequirementList as RequirementList exposing (RequirementList)
import Dict exposing (Dict)


type RequirementModel
    = RequirementModel Record


type alias Record =
    --do not expose this
    { actorRequirements : ActorRequirementList
    , placeHolderActors : ActorList
    , placeHolderRequirements : RequirementList
    }


empty : RequirementModel
empty =
    RequirementModel
        { actorRequirements = ActorRequirementList.empty
        , placeHolderActors = ActorList.empty
        , placeHolderRequirements = RequirementList.empty
        }


initialize : Dict String (List String) -> List String -> List String -> RequirementModel
initialize dict placeHolderActorNames placeHolderRequirementContents =
    RequirementModel
        { actorRequirements = ActorRequirementList.fromDict dict
        , placeHolderActors = ActorList.fromListOfNames placeHolderActorNames
        , placeHolderRequirements = RequirementList.fromListOfContents placeHolderRequirementContents
        }


internalWrapper : (Record -> Record) -> RequirementModel -> RequirementModel
internalWrapper generator model =
    -- do not expose this
    case model of
        RequirementModel record ->
            RequirementModel <| generator record



-- actor functions


removeActor : Int -> RequirementModel -> RequirementModel
removeActor index model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.removeActor index record.actorRequirements
            }
        )
        model


dropInActor : Int -> Int -> RequirementModel -> RequirementModel
dropInActor placeHolderIndex actorIndex model =
    internalWrapper
        (\record ->
            let
                maybeActor =
                    ActorList.get placeHolderIndex record.placeHolderActors
            in
            case maybeActor of
                Nothing ->
                    record

                Just actor ->
                    { record
                        | actorRequirements = ActorRequirementList.insertActor actorIndex actor record.actorRequirements
                        , placeHolderActors = ActorList.remove placeHolderIndex record.placeHolderActors
                    }
        )
        model



-- dropOffActor : Int -> Int -> RequirementModel -> RequirementModel
-- dropOffActor actorIndex placeHolderIndex model =
--     case model of
--         RequirementModel record ->
--             case ActorRequirementList.getRequirementsForActor actorIndex record.actorIndex of
--                 Nothing ->
--                     model
--                 Just requirementList ->
--                     { record
--                         | actorRequirements = ActorRequirementList.removeActor actorIndex record.actorRequirements
--                         , placeHolderActors = ActorList.remove placeHolderIndex record.placeHolderActors
--                         , placeHolderRequirements = record.placeHolderRequirements |> RequirementList.append requirementList
--                     }


pushActor : Actor -> RequirementModel -> RequirementModel
pushActor actor model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.pushActor actor record.actorRequirements
            }
        )
        model


sortActor : Int -> Int -> RequirementModel -> RequirementModel
sortActor fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.sortActor fromIndex toIndex record.actorRequirements
            }
        )
        model


renameActor : Int -> String -> RequirementModel -> RequirementModel
renameActor index name model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.renameActor index name record.actorRequirements
            }
        )
        model



-- placeHolder actor functions


removePlaceHolderActor : Int -> RequirementModel -> RequirementModel
removePlaceHolderActor index model =
    internalWrapper
        (\record ->
            { record
                | placeHolderActors = ActorList.remove index record.placeHolderActors
            }
        )
        model


pushPlaceHolderActor : Actor -> RequirementModel -> RequirementModel
pushPlaceHolderActor actor model =
    internalWrapper
        (\record ->
            { record
                | placeHolderActors = ActorList.push actor record.placeHolderActors
            }
        )
        model


sortPlaceHolderActor : Int -> Int -> RequirementModel -> RequirementModel
sortPlaceHolderActor fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | placeHolderActors = ActorList.sort fromIndex toIndex record.placeHolderActors
            }
        )
        model


renamePlaceHolderActor : Int -> String -> RequirementModel -> RequirementModel
renamePlaceHolderActor index name model =
    internalWrapper
        (\record ->
            { record
                | placeHolderActors = ActorList.rename index name record.placeHolderActors
            }
        )
        model



-- requirement functions


removeRequirement : ( Int, Int ) -> RequirementModel -> RequirementModel
removeRequirement ( actorIndex, requirementIndex ) model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.removeRequirement ( actorIndex, requirementIndex ) record.actorRequirements
            }
        )
        model


dropInRequirement : Int -> ( Int, Int ) -> RequirementModel -> RequirementModel
dropInRequirement placeHolderIndex ( actorIndex, requirementIndex ) model =
    internalWrapper
        (\record ->
            let
                maybeRequirement =
                    RequirementList.get placeHolderIndex record.placeHolderRequirements
            in
            case maybeRequirement of
                Nothing ->
                    record

                Just requirement ->
                    { record
                        | actorRequirements = ActorRequirementList.insertRequirement ( actorIndex, requirementIndex ) requirement record.actorRequirements
                        , placeHolderRequirements = RequirementList.remove placeHolderIndex record.placeHolderRequirements
                    }
        )
        model


dropOffRequirement : ( Int, Int ) -> Int -> RequirementModel -> RequirementModel
dropOffRequirement ( actorIndex, requirementIndex ) placeHolderIndex model =
    internalWrapper
        (\record ->
            let
                maybeRequirement =
                    ActorRequirementList.getRequirement ( actorIndex, requirementIndex ) record.actorRequirements
            in
            case maybeRequirement of
                Nothing ->
                    record

                Just requirement ->
                    { record
                        | actorRequirements = ActorRequirementList.removeRequirement ( actorIndex, requirementIndex ) record.actorRequirements
                        , placeHolderRequirements = RequirementList.insert placeHolderIndex requirement record.placeHolderRequirements
                    }
        )
        model


pushRequirement : Int -> Requirement -> RequirementModel -> RequirementModel
pushRequirement actorIndex requirement model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.pushRequirement actorIndex requirement record.actorRequirements
            }
        )
        model


sortRequirement : Int -> Int -> Int -> RequirementModel -> RequirementModel
sortRequirement actorIndex fromRequirementIndex toRequirementIndex model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.sortRequirements actorIndex fromRequirementIndex toRequirementIndex record.actorRequirements
            }
        )
        model


updateRequirementContent : ( Int, Int ) -> String -> RequirementModel -> RequirementModel
updateRequirementContent ( actorIndex, requirementIndex ) content model =
    internalWrapper
        (\record ->
            { record
                | actorRequirements = ActorRequirementList.updateRequirementContent ( actorIndex, requirementIndex ) content record.actorRequirements
            }
        )
        model



-- placeHolder requirement functions


removePlaceHolderRequirement : Int -> RequirementModel -> RequirementModel
removePlaceHolderRequirement index model =
    internalWrapper
        (\record ->
            { record
                | placeHolderRequirements = RequirementList.remove index record.placeHolderRequirements
            }
        )
        model


pushPlaceHolderRequirement : Requirement -> RequirementModel -> RequirementModel
pushPlaceHolderRequirement requirement model =
    internalWrapper
        (\record ->
            { record
                | placeHolderRequirements = RequirementList.push requirement record.placeHolderRequirements
            }
        )
        model


sortPlaceHolderRequirement : Int -> Int -> RequirementModel -> RequirementModel
sortPlaceHolderRequirement fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | placeHolderRequirements = RequirementList.sort fromIndex toIndex record.placeHolderRequirements
            }
        )
        model


updatePlaceHolderRequirementContent : Int -> String -> RequirementModel -> RequirementModel
updatePlaceHolderRequirementContent index content model =
    internalWrapper
        (\record ->
            { record
                | placeHolderRequirements = RequirementList.updateContent index content record.placeHolderRequirements
            }
        )
        model
