module Data.SystemContextModel exposing
    ( SystemContextModel
    , dropInActor
    , dropInExternalSystem
    , dropOffActor
    , dropOffExternalSystem
    , empty
    , getActors
    , getExplanation
    , getExternalSystems
    , getPurpose
    , getSystemName
    , getUnboundActors
    , getUnboundExternalSystems
    , initialize
    , pushActor
    , pushExternalSystem
    , pushUnboundActor
    , pushUnboundExternalSystem
    , removeActor
    , removeExternalSystem
    , removeUnboundActor
    , removeUnboundExternalSystem
    , renameActor
    , renameExternalSystem
    , renameUnboundActor
    , renameUnboundExternalSystem
    , setExplanation
    , setPurpose
    , setSystemName
    , sortActor
    , sortExternalSystem
    , sortUnboundActor
    , sortUnboundExternalSystem
    )

import Array exposing (Array)
import Data.Actor exposing (Actor)
import Data.ActorList as ActorList exposing (ActorList)
import Data.ExternalSystem exposing (ExternalSystem)
import Data.ExternalSystemList as ExternalSystemList exposing (ExternalSystemList)


type SystemContextModel
    = SystemContextModel Record


type alias Record =
    --do not expose this
    { systemName : String
    , purpose : String
    , explanation : String
    , actors : ActorList
    , externalSystems : ExternalSystemList
    , unboundActors : ActorList
    , unboundExternalSystems : ExternalSystemList
    }


internalWrapper : (Record -> Record) -> SystemContextModel -> SystemContextModel
internalWrapper generator model =
    -- do not expose this
    case model of
        SystemContextModel record ->
            SystemContextModel <| generator record



-- exposed functions


getPurpose : SystemContextModel -> String
getPurpose model =
    case model of
        SystemContextModel record ->
            record.purpose


getExplanation : SystemContextModel -> String
getExplanation model =
    case model of
        SystemContextModel record ->
            record.explanation


getSystemName : SystemContextModel -> String
getSystemName model =
    case model of
        SystemContextModel record ->
            record.systemName


getActors : SystemContextModel -> Array String
getActors model =
    case model of
        SystemContextModel record ->
            ActorList.getArray record.actors


getExternalSystems : SystemContextModel -> Array String
getExternalSystems model =
    case model of
        SystemContextModel record ->
            ExternalSystemList.getArray record.externalSystems


getUnboundActors : SystemContextModel -> Array String
getUnboundActors model =
    case model of
        SystemContextModel record ->
            ActorList.getArray record.unboundActors


getUnboundExternalSystems : SystemContextModel -> Array String
getUnboundExternalSystems model =
    case model of
        SystemContextModel record ->
            ExternalSystemList.getArray record.unboundExternalSystems


empty : SystemContextModel
empty =
    SystemContextModel
        { systemName = ""
        , purpose = ""
        , explanation = ""
        , actors = ActorList.empty
        , externalSystems = ExternalSystemList.empty
        , unboundActors = ActorList.empty
        , unboundExternalSystems = ExternalSystemList.empty
        }


initialize : String -> String -> String -> List String -> List String -> List String -> List String -> SystemContextModel
initialize systenName purpose explanation actors externalSystems unboundActors unboundExternalSystems =
    SystemContextModel
        { systemName = systenName
        , purpose = purpose
        , explanation = explanation
        , actors = ActorList.fromListOfNames actors
        , externalSystems = ExternalSystemList.fromListOfNames externalSystems
        , unboundActors = ActorList.fromListOfNames unboundActors
        , unboundExternalSystems = ExternalSystemList.fromListOfNames unboundExternalSystems
        }



-- systen name functions


setSystemName : String -> SystemContextModel -> SystemContextModel
setSystemName newSystemName model =
    internalWrapper
        (\record -> { record | systemName = newSystemName })
        model



-- purpose functions


setPurpose : String -> SystemContextModel -> SystemContextModel
setPurpose newPurpose model =
    internalWrapper
        (\record -> { record | purpose = newPurpose })
        model



-- explanation  functions


setExplanation : String -> SystemContextModel -> SystemContextModel
setExplanation newExplanation model =
    internalWrapper
        (\record -> { record | explanation = newExplanation })
        model



-- actor functions


removeActor : Int -> SystemContextModel -> SystemContextModel
removeActor index model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.remove index record.actors
            }
        )
        model


dropInActor : Int -> Int -> SystemContextModel -> SystemContextModel
dropInActor unboundIndex actorIndex model =
    internalWrapper
        (\record ->
            let
                maybeUnboundActor =
                    ActorList.get unboundIndex record.unboundActors
            in
            case maybeUnboundActor of
                Nothing ->
                    record

                Just unboundActor ->
                    { record
                        | actors = ActorList.insert actorIndex unboundActor record.actors
                        , unboundActors = ActorList.remove unboundIndex record.unboundActors
                    }
        )
        model


dropOffActor : Int -> Int -> SystemContextModel -> SystemContextModel
dropOffActor actorIndex unboundIndex model =
    internalWrapper
        (\record ->
            let
                maybeActor =
                    ActorList.get actorIndex record.actors
            in
            case maybeActor of
                Nothing ->
                    record

                Just actor ->
                    { record
                        | actors = ActorList.remove actorIndex record.actors
                        , unboundActors = ActorList.insert unboundIndex actor record.unboundActors
                    }
        )
        model


pushActor : Actor -> SystemContextModel -> SystemContextModel
pushActor actor model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.push actor record.actors
            }
        )
        model


sortActor : Int -> Int -> SystemContextModel -> SystemContextModel
sortActor fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.sort fromIndex toIndex record.actors
            }
        )
        model


renameActor : Int -> String -> SystemContextModel -> SystemContextModel
renameActor index name model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.rename index name record.actors
            }
        )
        model



-- unbound actor functions


removeUnboundActor : Int -> SystemContextModel -> SystemContextModel
removeUnboundActor index model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.remove index record.actors
            }
        )
        model


pushUnboundActor : Actor -> SystemContextModel -> SystemContextModel
pushUnboundActor actor model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.push actor record.actors
            }
        )
        model


sortUnboundActor : Int -> Int -> SystemContextModel -> SystemContextModel
sortUnboundActor fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.sort fromIndex toIndex record.actors
            }
        )
        model


renameUnboundActor : Int -> String -> SystemContextModel -> SystemContextModel
renameUnboundActor index name model =
    internalWrapper
        (\record ->
            { record
                | actors = ActorList.rename index name record.actors
            }
        )
        model



-- external system functions


removeExternalSystem : Int -> SystemContextModel -> SystemContextModel
removeExternalSystem index model =
    internalWrapper
        (\record ->
            { record
                | externalSystems = ExternalSystemList.remove index record.externalSystems
            }
        )
        model


dropInExternalSystem : Int -> Int -> SystemContextModel -> SystemContextModel
dropInExternalSystem unboundIndex externalSystemIndex model =
    internalWrapper
        (\record ->
            let
                maybeUnboundExternalSystem =
                    ExternalSystemList.get unboundIndex record.unboundExternalSystems
            in
            case maybeUnboundExternalSystem of
                Nothing ->
                    record

                Just unboundExternalSystem ->
                    { record
                        | externalSystems = ExternalSystemList.insert externalSystemIndex unboundExternalSystem record.externalSystems
                        , unboundExternalSystems = ExternalSystemList.remove unboundIndex record.unboundExternalSystems
                    }
        )
        model


dropOffExternalSystem : Int -> Int -> SystemContextModel -> SystemContextModel
dropOffExternalSystem externalSystemIndex unboundIndex model =
    internalWrapper
        (\record ->
            let
                maybeExternalSystem =
                    ExternalSystemList.get externalSystemIndex record.externalSystems
            in
            case maybeExternalSystem of
                Nothing ->
                    record

                Just actor ->
                    { record
                        | externalSystems = ExternalSystemList.remove externalSystemIndex record.externalSystems
                        , unboundExternalSystems = ExternalSystemList.insert unboundIndex actor record.unboundExternalSystems
                    }
        )
        model


pushExternalSystem : ExternalSystem -> SystemContextModel -> SystemContextModel
pushExternalSystem actor model =
    internalWrapper
        (\record ->
            { record
                | externalSystems = ExternalSystemList.push actor record.externalSystems
            }
        )
        model


sortExternalSystem : Int -> Int -> SystemContextModel -> SystemContextModel
sortExternalSystem fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | externalSystems = ExternalSystemList.sort fromIndex toIndex record.externalSystems
            }
        )
        model


renameExternalSystem : Int -> String -> SystemContextModel -> SystemContextModel
renameExternalSystem index name model =
    internalWrapper
        (\record ->
            { record
                | externalSystems = ExternalSystemList.rename index name record.externalSystems
            }
        )
        model



-- unbound external system functions


removeUnboundExternalSystem : Int -> SystemContextModel -> SystemContextModel
removeUnboundExternalSystem index model =
    internalWrapper
        (\record ->
            { record
                | unboundExternalSystems = ExternalSystemList.remove index record.unboundExternalSystems
            }
        )
        model


pushUnboundExternalSystem : ExternalSystem -> SystemContextModel -> SystemContextModel
pushUnboundExternalSystem externalSystem model =
    internalWrapper
        (\record ->
            { record
                | unboundExternalSystems = ExternalSystemList.push externalSystem record.unboundExternalSystems
            }
        )
        model


sortUnboundExternalSystem : Int -> Int -> SystemContextModel -> SystemContextModel
sortUnboundExternalSystem fromIndex toIndex model =
    internalWrapper
        (\record ->
            { record
                | unboundExternalSystems = ExternalSystemList.sort fromIndex toIndex record.unboundExternalSystems
            }
        )
        model


renameUnboundExternalSystem : Int -> String -> SystemContextModel -> SystemContextModel
renameUnboundExternalSystem index name model =
    internalWrapper
        (\record ->
            { record
                | unboundExternalSystems = ExternalSystemList.rename index name record.unboundExternalSystems
            }
        )
        model
