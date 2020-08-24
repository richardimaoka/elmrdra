module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Dom as Dom
import Data.SystemContextModel as SystemContextModel
    exposing
        ( SystemContextModel
        , getActors
        , getExplanation
        , getExternalSystems
        , getPurpose
        , getSystemName
        , getUnboundActors
        , getUnboundExternalSystems
        , initialize
        )
import Dict exposing (fromList)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (class, draggable, id, style, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onDoubleClick, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width, x, y)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    div
        [ class "flex"
        , class "p-1"
        ]
        [ viewMainPanel model
        , viewUnboundPanel model
        ]


viewMainPanel : Model -> Html Msg
viewMainPanel model =
    div
        [ style "max-width" "600px"
        , class "p-1"
        ]
        [ viewDescriptionPanel model
        , viewListingPanel model
        ]


viewUnboundPanel : Model -> Html Msg
viewUnboundPanel model =
    div
        [ style "max-width" "600px"
        , class "p-1"
        , class "flex"
        ]
        [ viewActorList <| getUnboundActors model.systemContext
        , viewExternalSystemList <| getUnboundExternalSystems model.systemContext
        ]


viewDescriptionPanel : Model -> Html Msg
viewDescriptionPanel model =
    div
        [ class "p-1" ]
        [ viewSystemName <| getSystemName model.systemContext
        , viewPurpose <| getPurpose model.systemContext
        , viewExplanation <| getExplanation model.systemContext
        ]


viewPurpose : String -> Html Msg
viewPurpose purpose =
    if String.isEmpty purpose then
        div
            [ style "min-height" "30px"
            , style "opacity" "0.3"
            ]
            [ text "please fill in the purpose" ]

    else
        div
            [ style "min-height" "30px"
            ]
            [ text purpose ]


viewSystemName : String -> Html Msg
viewSystemName systemName =
    if String.isEmpty systemName then
        div
            [ style "min-height" "30px"
            , style "opacity" "0.3"
            ]
            [ text "please fill in the system name" ]

    else
        div
            [ style "min-height" "30px"
            ]
            [ text systemName ]


viewExplanation : String -> Html Msg
viewExplanation explanation =
    if String.isEmpty explanation then
        div
            [ style "min-height" "30px"
            , style "opacity" "0.3"
            ]
            [ text "please fill in the explanation" ]

    else
        div
            [ style "min-height" "30px"
            ]
            [ text explanation ]


viewListingPanel : Model -> Html Msg
viewListingPanel model =
    div [ class "flex" ]
        [ viewActorList <| getActors model.systemContext
        , viewExternalSystemList <| getExternalSystems model.systemContext
        ]


viewActorList : Array String -> Html Msg
viewActorList actors =
    div
        [ class "m-4"
        , class "p-4"
        , style "width" "300px"
        ]
        --(List.append
        (Array.toIndexedList actors
            |> List.map
                (\( index, actor ) ->
                    viewActor index actor
                )
        )



--  [ buttonAddRequirement actorIndex (Array.length requirements) ]
--)


viewActor : Int -> String -> Html Msg
viewActor actorIndex actorName =
    div
        [ class "m-4"
        , class "p-4"
        , style "width" "200px"
        , draggable "true"
        ]
        [ viewActorSvg
        , viewStaticActorName actorIndex actorName
        ]


viewActorSvg : Html Msg
viewActorSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ circle [ cx "25", cy "25", r "25" ] []
        ]


viewStaticActorName : Int -> String -> Html Msg
viewStaticActorName actorIndex actorName =
    div
        [ style "min-height" "16px"
        , class "p-1"
        ]
        [ text <| actorName ]


viewExternalSystemList : Array String -> Html Msg
viewExternalSystemList systems =
    div
        [ class "m-4"
        , class "p-4"
        , style "width" "300px"
        ]
        --(List.append
        (Array.toIndexedList systems
            |> List.map
                (\( index, system ) ->
                    viewExternalSystem index system
                )
        )


viewExternalSystem : Int -> String -> Html Msg
viewExternalSystem actorIndex actorName =
    div
        [ class "m-4"
        , class "p-4"
        , style "width" "200px"
        , draggable "true"
        ]
        [ viewExternalSystemSvg
        , viewStaticExternalSystemName actorIndex actorName
        ]


viewExternalSystemSvg : Html Msg
viewExternalSystemSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ rect [ x "0", y "0", width "50", height "50" ] []
        ]


viewStaticExternalSystemName : Int -> String -> Html Msg
viewStaticExternalSystemName actorIndex actorName =
    div
        [ style "min-height" "16px"
        , class "p-1"
        ]
        [ text <| actorName ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed ( msg, True )

            else
                Decode.fail "not ENTER"
    in
    -- preventDefaultOn not to trigger onBlur, and alwaysPreventDefault to always prevent onBlur
    preventDefaultOn "keydown" (keyCode |> Decode.andThen isEnter)


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    stopPropagationOn "dragstart" <| Decode.succeed ( msg, True )


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" <| Decode.succeed msg


onDragEnter : Msg -> Attribute Msg
onDragEnter msg =
    on "dragenter" <| Decode.succeed msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { systemContext =
            SystemContextModel.initialize
                ""
                ""
                ""
                [ "actor-1", "actor-2", "actor-3", "actor-4" ]
                [ "system-1", "system-2", "system-3", "system-4", "system-5" ]
                [ "actor-5", "actor-6", "actor-7", "actor-8" ]
                [ "system-6", "system-7" ]
      , control = NoControl
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type Msg
    = -- Actor messages
      PushActor ActorIndex
    | UpdateActorName ActorIndex String
    | ShowActorDropDown ActorIndex
    | OpenActorInput ActorIndex
    | FocusActorInput (Result Dom.Error ())
    | DragStartActor FromIndex
    | DragEnterActor FromIndex ToIndex
      -- Requirement messages
    | PushRequirement ActorIndex RequirementIndex
    | UpdateRequirementContent ActorIndex RequirementIndex String
    | ShowRequirementDropDown ActorIndex RequirementIndex
    | OpenRequirementInput ActorIndex RequirementIndex
    | FocusRequirementInput (Result Dom.Error ())
    | RemoveRequirement ActorIndex RequirementIndex
    | DragStartRequirement ActorIndex FromIndex
    | DragEnterRequirement ActorIndex FromIndex ToIndex
      -- Requirement messages
    | LeaveControl


type alias FromIndex =
    Int


type alias ToIndex =
    Int


type alias ActorIndex =
    Int


type alias RequirementIndex =
    Int


type alias Model =
    { systemContext : SystemContextModel
    , control : ModelControl
    }


type alias ActorRequirementArray =
    Array ( String, Array String )


type alias ActorArray =
    Array String


type alias RequirementArray =
    Array String



-- control of UI


{-| ModelControl: only one of them is possible at a time
-}
type ModelControl
    = ActorEditState ActorEditState Int
    | ActorDragged Int
    | RequirementControl RequirementControl ( Int, Int )
    | NoControl


type RequirementControl
    = RequirementDropDown
    | RequirementInput
    | RequirementDragged


type ActorEditState
    = ActorDropDown
    | ActorInput


{-| getActorDragged, getActorEditState, getRequirementControl, getRequirementDragged:
Utility functions to convert ModelControl to fin-grained Maybe.
Only one of these functions may return Just at a time.
-}
getActorDragged : ModelControl -> Maybe Int
getActorDragged control =
    case control of
        RequirementControl _ _ ->
            Nothing

        ActorEditState _ _ ->
            Nothing

        ActorDragged index ->
            Just index

        NoControl ->
            Nothing


getActorEditState : ModelControl -> Maybe ( ActorEditState, Int )
getActorEditState control =
    case control of
        RequirementControl _ _ ->
            Nothing

        ActorEditState actorEditState selectIndex ->
            Just ( actorEditState, selectIndex )

        ActorDragged _ ->
            Nothing

        NoControl ->
            Nothing


getRequirementControl : ModelControl -> Maybe ( RequirementControl, ( Int, Int ) )
getRequirementControl control =
    case control of
        RequirementControl requirementControl selectIndexTuple ->
            Just ( requirementControl, selectIndexTuple )

        ActorEditState _ _ ->
            Nothing

        ActorDragged _ ->
            Nothing

        NoControl ->
            Nothing
