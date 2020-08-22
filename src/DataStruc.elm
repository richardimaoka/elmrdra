module DataStruc exposing (Actor(..))


type Requirement
    = Requirement String


updateRequirement : String -> Requirement -> Requirement
updateRequirement text _ =
    Requirement text


type Actor
    = Actor String


updateActorName : String -> Actor -> Actor
updateActorName name _ =
    Actor name


type ExternalSystem
    = ExternalSystem String


updateExternalSystemName : String -> ExternalSystem -> ExternalSystem
updateExternalSystemName name _ =
    ExternalSystem name


type BizElement
    = BizElement String


updateBizElementName : String -> BizElement -> BizElement
updateBizElementName name _ =
    BizElement name


type RequirementList
    = RequirementList (Array String)


toListOfRequirement : RequirementList -> List String
toListOfRequirement (RequirementList array) =
    Array.toList array


insertRequirement : Index -> Requirement -> RequirementList -> RequirementList
insertRequirement requirement list =
    list


lengthOfRequirementList : RequirementList -> Int
lengthOfRequirementList (RequirementList array) =
    -- do not expose this method
    Array.length array


pushRequirement : Requirement -> RequirementList -> RequirementList
pushRequirement requirement list =
    insertRequirement (lengthOfRequirementList list - 1) requirement list


removeRequirement : Int -> RequirementList -> RequirementList
removeRequirement inndex list =
    list


sortRequirement : Int -> Int -> RequirementList -> RequirementList
sortRequirement fronIndex toIndex list =
    list
