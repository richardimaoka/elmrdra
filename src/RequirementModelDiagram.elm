module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Dom as Dom
import Data.Actor as Actor exposing (create)
import Data.Requirement as Requirement exposing (create)
import Data.RequirementModel as RequirementModel exposing (RequirementModel, getActorRequirements, initialize)
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
    let
        actorRequirements =
            RequirementModel.getActorRequirements model.requirementModel
    in
    div []
        [ viewActorRequirementPanel actorRequirements model.control
        ]


viewActorRequirementPanel : ActorRequirementArray -> ModelControl -> Html Msg
viewActorRequirementPanel actorRequirements control =
    div [ style "max-width" "600px" ]
        (List.append
            (Array.toIndexedList actorRequirements
                |> List.map
                    (\( actorIndex, ( actorName, requirements ) ) ->
                        viewEachActorRequirement actorIndex actorName requirements control
                    )
            )
            [ buttonAddActorRequirement <| Array.length actorRequirements ]
        )


buttonAddActorRequirement : Int -> Html Msg
buttonAddActorRequirement newActorIndex =
    button
        [ class "bg-gray-400"
        , class "p-1"
        , onClick <| PushActor newActorIndex
        ]
        [ text "add actor" ]


viewEachActorRequirement : Int -> String -> RequirementArray -> ModelControl -> Html Msg
viewEachActorRequirement actorIndex actorName requirements control =
    let
        attributes =
            case getActorDragged control of
                Nothing ->
                    [ style "opacity" "1.0" ]

                Just selectIndex ->
                    if actorIndex == selectIndex then
                        [ style "opacity" "0.5" ]

                    else
                        [ style "opacity" "1.0"
                        , onDragEnter <| DragEnterActor selectIndex actorIndex
                        ]

        maybeActorEditState =
            getActorEditState control

        maybeRequirementControl =
            getRequirementControl control
    in
    div
        attributes
        [ viewActor actorIndex maybeActorEditState actorName
        , viewRequirementList actorIndex maybeRequirementControl requirements
        ]


viewActor : Int -> Maybe ( ActorEditState, Int ) -> String -> Html Msg
viewActor actorIndex maybeControl actorName =
    div
        [ class "m-4"
        , class "p-4"
        , style "max-width" "200px"
        , draggable "true"
        , onDragStart <| DragStartActor actorIndex
        , onDragEnd LeaveControl
        ]
        [ viewActorSvg
        , case maybeControl of
            Nothing ->
                viewStaticActorName actorIndex actorName

            Just ( actorEditState, selectIndex ) ->
                if actorIndex == selectIndex then
                    case actorEditState of
                        ActorDropDown ->
                            viewActorDropdown actorIndex actorName

                        ActorInput ->
                            viewActorNameInput actorIndex actorName

                else
                    viewStaticActorName actorIndex actorName
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
        , onClick <| ShowActorDropDown actorIndex
        , onDoubleClick <| OpenActorInput actorIndex
        ]
        [ text <| actorName ]


viewActorNameInput : Int -> String -> Html Msg
viewActorNameInput actorIndex actorName =
    input
        [ id <| actorInputTagId actorIndex
        , style "max-width" "160px"
        , class "border-2"
        , class "p-1"
        , value <| actorName
        , onBlur LeaveControl
        , onEnter LeaveControl
        , onInput <| UpdateActorName actorIndex
        ]
        []


actorInputTagId : Int -> String
actorInputTagId actorIndex =
    "input-actor-" ++ String.fromInt actorIndex


viewActorDropdown : Int -> String -> Html Msg
viewActorDropdown actorIndex actorName =
    div []
        [ viewStaticActorName actorIndex actorName
        , div [ onClick <| OpenActorInput actorIndex ] [ text "rename" ]
        , div [] [ text "delete" ]
        ]


viewRequirementList : Int -> Maybe ( RequirementControl, ( Int, Int ) ) -> RequirementArray -> Html Msg
viewRequirementList actorIndex maybeControl requirements =
    div
        [ class "m-4"
        , class "p-4"
        , style "max-width" "400px"
        ]
        (List.append
            (Array.toIndexedList requirements
                |> List.map
                    (\( requirementIndex, requirementContent ) ->
                        viewRequirement ( actorIndex, requirementIndex ) maybeControl requirementContent
                    )
            )
            [ buttonAddRequirement actorIndex (Array.length requirements) ]
        )


buttonAddRequirement : Int -> Int -> Html Msg
buttonAddRequirement actorIndex newRequirementIndex =
    button
        [ class "bg-gray-400"
        , class "p-1"
        , onClick <| PushRequirement actorIndex newRequirementIndex
        ]
        [ text "add requirement" ]


viewRequirement : ( Int, Int ) -> Maybe ( RequirementControl, ( Int, Int ) ) -> String -> Html Msg
viewRequirement ( actorIndex, requirementIndex ) maybeControl requirementContent =
    case maybeControl of
        Nothing ->
            viewStaticRequirementContent actorIndex requirementIndex "1.0" requirementContent

        Just ( requirementControl, ( selectActorIndex, selectRequirementIndex ) ) ->
            if actorIndex == selectActorIndex && requirementIndex == selectRequirementIndex then
                case requirementControl of
                    RequirementDropDown ->
                        viewRequirementDropdown actorIndex requirementIndex requirementContent

                    RequirementInput ->
                        viewRequirementContentInput actorIndex requirementIndex requirementContent

                    RequirementDragged ->
                        viewStaticRequirementContent actorIndex requirementIndex "0.5" requirementContent

            else
                viewStaticRequirementContent actorIndex requirementIndex "1.0" requirementContent


viewStaticRequirementContent : Int -> Int -> String -> String -> Html Msg
viewStaticRequirementContent actorIndex requirementIndex opacity requirementContent =
    div
        [ style "min-height" "24px"
        , style "opacity" opacity
        , class "m-2"
        , class "p-1"
        , draggable "true"
        , onClick <| ShowRequirementDropDown actorIndex requirementIndex
        , onDoubleClick <| OpenRequirementInput actorIndex requirementIndex

        --, onDragEnter <| DragEnterRequirement actorIndex selectIndex requirementIndex
        ]
        [ text <| requirementContent ]


viewRequirementContentInput : Int -> Int -> String -> Html Msg
viewRequirementContentInput actorIndex requirementIndex requirementContent =
    input
        [ id <| requirementInputTagId actorIndex requirementIndex
        , style "max-width" "160px"
        , style "min-height" "24px"
        , class "border-2"
        , class "p-1"
        , value <| requirementContent
        , onBlur LeaveControl
        , onEnter LeaveControl
        , onInput <| UpdateRequirementContent actorIndex requirementIndex
        ]
        []


requirementInputTagId : Int -> Int -> String
requirementInputTagId actorIndex requirementIndex =
    "input-requirement-" ++ String.fromInt actorIndex ++ "-" ++ String.fromInt requirementIndex


viewRequirementDropdown : Int -> Int -> String -> Html Msg
viewRequirementDropdown actorIndex requirementIndex requirementContent =
    div []
        [ viewStaticRequirementContent actorIndex requirementIndex "1.0" requirementContent
        , div [ onClick <| OpenRequirementInput actorIndex requirementIndex ] [ text "edit" ]
        , div [ onClick <| RemoveRequirement actorIndex requirementIndex ] [ text "remove" ]
        ]


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



-- viewPlaceHolderActorPanel : ActorList -> Html Msg
-- viewPlaceHolderRequirementPanel : RequirementList -> Html Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { requirementModel =
            RequirementModel.initialize
                (Dict.fromList
                    [ ( "actor1", [ "req1-1", "req1-2" ] )
                    , ( "actor2", [ "req2-1", "req2-2" ] )
                    , ( "actor3", [ "req3-1", "req3-2", "req3-3" ] )
                    , ( "actor4", [] )
                    ]
                )
                [ "aaa" ]
                [ "bbb" ]
      , control = NoControl
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- handle Actor messages
        PushActor newActorIndex ->
            ( { model
                | requirementModel = RequirementModel.pushActor (Actor.create "") model.requirementModel
                , control = ActorEditState ActorInput newActorIndex
              }
            , Task.attempt FocusActorInput (Dom.focus <| actorInputTagId newActorIndex)
            )

        UpdateActorName actorIndex newName ->
            ( { model | requirementModel = RequirementModel.renameActor actorIndex newName model.requirementModel }
            , Cmd.none
            )

        ShowActorDropDown actorIndex ->
            ( { model | control = ActorEditState ActorDropDown actorIndex }
            , Cmd.none
            )

        OpenActorInput actorIndex ->
            ( { model | control = ActorEditState ActorInput actorIndex }
            , Task.attempt FocusActorInput (Dom.focus <| actorInputTagId actorIndex)
            )

        FocusActorInput _ ->
            ( model, Cmd.none )

        DragStartActor actorIndex ->
            ( { model | control = ActorDragged actorIndex }
            , Cmd.none
            )

        DragEnterActor fromActorIndex toActorIndex ->
            ( { model
                | requirementModel = RequirementModel.sortActor fromActorIndex toActorIndex model.requirementModel
                , control = ActorDragged toActorIndex
              }
            , Cmd.none
            )

        -- handle Requirement messages
        PushRequirement actorIndex newRequirementIndex ->
            ( { model
                | requirementModel = RequirementModel.pushRequirement actorIndex (Requirement.create "") model.requirementModel
                , control = RequirementControl RequirementInput ( actorIndex, newRequirementIndex )
              }
            , Task.attempt FocusRequirementInput (Dom.focus <| requirementInputTagId actorIndex newRequirementIndex)
            )

        UpdateRequirementContent actorIndex requirementIndex newContent ->
            ( { model
                | requirementModel = RequirementModel.updateRequirementContent ( actorIndex, requirementIndex ) newContent model.requirementModel
              }
            , Cmd.none
            )

        ShowRequirementDropDown actorIndex requirementIndex ->
            ( { model | control = RequirementControl RequirementDropDown ( actorIndex, requirementIndex ) }
            , Cmd.none
            )

        OpenRequirementInput actorIndex requirementIndex ->
            ( { model | control = RequirementControl RequirementInput ( actorIndex, requirementIndex ) }
            , Task.attempt FocusRequirementInput (Dom.focus <| requirementInputTagId actorIndex requirementIndex)
            )

        FocusRequirementInput _ ->
            ( model, Cmd.none )

        RemoveRequirement actorIndex requirementIndex ->
            ( { model
                | requirementModel = RequirementModel.removeRequirement ( actorIndex, requirementIndex ) model.requirementModel
                , control = NoControl
              }
            , Cmd.none
            )

        DragStartRequirement actorIndex requirementIndex ->
            ( { model | control = RequirementControl RequirementDragged ( actorIndex, requirementIndex ) }
            , Cmd.none
            )

        DragEnterRequirement actorIndex fromRequirementIndex toRequirementIndex ->
            ( { model
                | requirementModel = RequirementModel.sortRequirement actorIndex fromRequirementIndex toRequirementIndex model.requirementModel
                , control = RequirementControl RequirementDragged ( actorIndex, toRequirementIndex )
              }
            , Cmd.none
            )

        --common messages
        LeaveControl ->
            ( { model | control = NoControl }
            , Cmd.none
            )


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
    { requirementModel : RequirementModel
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
