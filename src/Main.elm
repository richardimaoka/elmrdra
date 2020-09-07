module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Dom as Dom
import Data.Actor as Actor exposing (create)
import Data.SystemContextModel as SystemContextModel
    exposing
        ( SystemContextModel
        , dropInActor
        , dropOffActor
        , getActors
        , getExplanation
        , getExternalSystems
        , getPurpose
        , getSystemName
        , getUnboundActors
        , getUnboundExternalSystems
        , initialize
        , pushActor
        , pushUnboundActor
        , removeActor
        , removeUnboundActor
        , renameActor
        , renameUnboundActor
        , sortActor
        , sortUnboundActor
        )
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
        [ viewActorList model.control (getActors model.systemContext)
        , viewExternalSystemList <| getExternalSystems model.systemContext
        ]



---------------------------------
-- actor views
---------------------------------


viewActorList : ModelControl -> Array String -> Html Msg
viewActorList control actors =
    div
        [ class "m-4"
        , class "p-4"
        , style "width" "300px"
        ]
        (List.append
            (Array.toIndexedList actors
                |> List.map
                    (\( index, actor ) ->
                        viewActor control index actor
                    )
            )
            [ buttonAddActor (Array.length actors) ]
        )


buttonAddActor : Int -> Html Msg
buttonAddActor newActorIndex =
    button
        [ class "bg-gray-400"
        , class "p-1"
        , onClick <| PushActor newActorIndex
        ]
        [ text "add actor" ]


viewActor : ModelControl -> Int -> String -> Html Msg
viewActor control actorIndex actorName =
    let
        maybeDragged =
            getActorDragged control

        maybeEdit =
            getActorEdit control

        defaultAttributes =
            [ class "m-4"
            , class "p-4"
            , style "width" "200px"
            , draggable "true"
            , onDragStart <| DragStartActor actorIndex
            , onDragEnd LeaveControl
            ]

        attributes =
            case maybeDragged of
                Just (ActorDragged selectIndex) ->
                    if selectIndex /= actorIndex then
                        (onDragEnter <| SortActor selectIndex actorIndex) :: defaultAttributes

                    else
                        defaultAttributes

                Just (UnboundActorDragged selectIndex) ->
                    (onDragEnter <| DropInActor selectIndex actorIndex) :: defaultAttributes

                Nothing ->
                    defaultAttributes
    in
    div
        attributes
        [ viewActorSvg maybeDragged actorIndex
        , case maybeEdit of
            Nothing ->
                viewStaticActorName actorIndex actorName

            Just (ActorInput selectIndex) ->
                if actorIndex == selectIndex then
                    viewActorNameInput actorIndex actorName

                else
                    viewStaticActorName actorIndex actorName

            Just (ActorDropDown selectIndex) ->
                if actorIndex == selectIndex then
                    viewActorDropdown actorIndex actorName

                else
                    viewStaticActorName actorIndex actorName
        ]


viewActorSvg : Maybe ActorDragged -> Int -> Html Msg
viewActorSvg maybeDragged actorIndex =
    let
        defaultAttributes =
            [ width "50", height "50", viewBox "0 0 50 50" ]

        attributes =
            case maybeDragged of
                Just (ActorDragged selectIndex) ->
                    if actorIndex == selectIndex then
                        style "opacity" "0.5" :: defaultAttributes

                    else
                        defaultAttributes

                Just (UnboundActorDragged _) ->
                    defaultAttributes

                Nothing ->
                    defaultAttributes
    in
    svg
        attributes
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


viewActorDragged : Int -> String -> Html Msg
viewActorDragged actorIndex actorName =
    div
        [ style "opacity" "0.5" ]
        [ viewStaticActorName actorIndex actorName ]


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
        , div [ onClick <| RemoveActor actorIndex ] [ text "delete" ]
        ]



---------------------------------
--- unbound actor views
---------------------------------


viewUnboundActorList : ModelControl -> Array String -> Html Msg
viewUnboundActorList control actors =
    div
        [ class "m-4"
        , class "p-4"
        , style "width" "300px"
        ]
        (List.append
            (Array.toIndexedList actors
                |> List.map
                    (\( index, actor ) ->
                        viewUnboundActor control index actor
                    )
            )
            [ buttonAddUnboundActor (Array.length actors) ]
        )


buttonAddUnboundActor : Int -> Html Msg
buttonAddUnboundActor newActorIndex =
    button
        [ class "bg-gray-400"
        , class "p-1"
        , onClick <| PushUnboundActor newActorIndex
        ]
        [ text "add actor" ]


viewUnboundActor : ModelControl -> Int -> String -> Html Msg
viewUnboundActor control actorIndex actorName =
    let
        maybeDragged =
            getActorDragged control

        maybeEdit =
            getUnboundActorEdit control

        defaultAttributes =
            [ class "m-4"
            , class "p-4"
            , style "width" "200px"
            , draggable "true"
            , onDragStart <| DragStartUnboundActor actorIndex
            , onDragEnd LeaveControl
            ]

        attributes =
            case maybeDragged of
                Just (UnboundActorDragged selectIndex) ->
                    if selectIndex /= actorIndex then
                        (onDragEnter <| SortUnboundActor selectIndex actorIndex) :: defaultAttributes

                    else
                        defaultAttributes

                Just (ActorDragged selectIndex) ->
                    (onDragEnter <| DropOffActor selectIndex actorIndex) :: defaultAttributes

                Nothing ->
                    defaultAttributes
    in
    div
        attributes
        [ viewUnboundActorSvg maybeDragged actorIndex
        , case maybeEdit of
            Nothing ->
                viewStaticUnboundActorName actorIndex actorName

            Just (UnboundActorDropDown selectIndex) ->
                if actorIndex == selectIndex then
                    viewUnboundActorDropdown actorIndex actorName

                else
                    viewStaticUnboundActorName actorIndex actorName

            Just (UnboundActorInput selectIndex) ->
                if actorIndex == selectIndex then
                    viewUnboundActorNameInput actorIndex actorName

                else
                    viewStaticUnboundActorName actorIndex actorName
        ]


viewUnboundActorSvg : Maybe ActorDragged -> Int -> Html Msg
viewUnboundActorSvg maybeDragged actorIndex =
    let
        defaultAttributes =
            [ width "50", height "50", viewBox "0 0 50 50" ]

        attributes =
            case maybeDragged of
                Just (UnboundActorDragged selectIndex) ->
                    if actorIndex == selectIndex then
                        style "opacity" "0.5" :: defaultAttributes

                    else
                        defaultAttributes

                Just (ActorDragged _) ->
                    defaultAttributes

                Nothing ->
                    defaultAttributes
    in
    svg
        attributes
        [ circle [ cx "25", cy "25", r "25" ] []
        ]


viewStaticUnboundActorName : Int -> String -> Html Msg
viewStaticUnboundActorName actorIndex actorName =
    div
        [ style "min-height" "16px"
        , class "p-1"
        , onClick <| ShowUnboundActorDropDown actorIndex
        , onDoubleClick <| OpenUnboundActorInput actorIndex
        ]
        [ text <| actorName ]


viewUnboundActorDragged : Int -> String -> Html Msg
viewUnboundActorDragged actorIndex actorName =
    div
        [ style "opacity" "0.5" ]
        [ viewStaticActorName actorIndex actorName ]


viewUnboundActorNameInput : Int -> String -> Html Msg
viewUnboundActorNameInput actorIndex actorName =
    input
        [ id <| unboundActorInputTagId actorIndex
        , style "max-width" "160px"
        , class "border-2"
        , class "p-1"
        , value <| actorName
        , onBlur LeaveControl
        , onEnter LeaveControl
        , onInput <| UpdateUnboundActorName actorIndex
        ]
        []


unboundActorInputTagId : Int -> String
unboundActorInputTagId actorIndex =
    "input-unbound-actor-" ++ String.fromInt actorIndex


viewUnboundActorDropdown : Int -> String -> Html Msg
viewUnboundActorDropdown actorIndex actorName =
    div []
        [ viewStaticUnboundActorName actorIndex actorName
        , div [ onClick <| OpenUnboundActorInput actorIndex ] [ text "rename" ]
        , div [ onClick <| RemoveUnboundActor actorIndex ] [ text "delete" ]
        ]



---------------------------------
-- external system views
---------------------------------


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


viewUnboundPanel : Model -> Html Msg
viewUnboundPanel model =
    div
        [ style "max-width" "600px"
        , class "p-1"
        , class "flex"
        ]
        [ viewUnboundActorList model.control (getUnboundActors model.systemContext)
        , viewExternalSystemList <| getUnboundExternalSystems model.systemContext
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
    case msg of
        -- handle Actor messages
        PushActor newActorIndex ->
            ( { model
                | systemContext = pushActor (Actor.create "") model.systemContext
                , control = ControlActorEdit <| ActorInput newActorIndex
              }
            , Task.attempt FocusActorInput (Dom.focus <| actorInputTagId newActorIndex)
            )

        UpdateActorName actorIndex newName ->
            ( { model | systemContext = renameActor actorIndex newName model.systemContext }
            , Cmd.none
            )

        ShowActorDropDown actorIndex ->
            ( { model | control = ControlActorEdit <| ActorDropDown actorIndex }
            , Cmd.none
            )

        OpenActorInput actorIndex ->
            ( { model | control = ControlActorEdit (ActorInput actorIndex) }
            , Task.attempt FocusActorInput (Dom.focus <| actorInputTagId actorIndex)
            )

        RemoveActor actorIndex ->
            ( { model
                | systemContext = removeActor actorIndex model.systemContext
                , control = NoControl
              }
            , Cmd.none
            )

        DragStartActor actorIndex ->
            ( { model | control = ControlActorDragged <| ActorDragged actorIndex }
            , Cmd.none
            )

        SortActor fromIndex toIndex ->
            ( { model
                | systemContext = sortActor fromIndex toIndex model.systemContext
                , control = ControlActorDragged <| ActorDragged toIndex
              }
            , Cmd.none
            )

        DropInActor unboundIndex actorIndex ->
            ( { model
                | systemContext = dropInActor unboundIndex actorIndex model.systemContext
                , control = ControlActorDragged <| ActorDragged actorIndex
              }
            , Cmd.none
            )

        FocusActorInput _ ->
            ( model, Cmd.none )

        -- handle unbound actor messages
        PushUnboundActor newActorIndex ->
            ( { model
                | systemContext = pushUnboundActor (Actor.create "") model.systemContext
                , control = ControlUnboundActorEdit <| UnboundActorInput newActorIndex
              }
            , Task.attempt FocusUnboundActorInput (Dom.focus <| unboundActorInputTagId newActorIndex)
            )

        UpdateUnboundActorName actorIndex newName ->
            ( { model | systemContext = renameUnboundActor actorIndex newName model.systemContext }
            , Cmd.none
            )

        ShowUnboundActorDropDown actorIndex ->
            ( { model | control = ControlUnboundActorEdit <| UnboundActorDropDown actorIndex }
            , Cmd.none
            )

        OpenUnboundActorInput actorIndex ->
            ( { model | control = ControlUnboundActorEdit <| UnboundActorInput actorIndex }
            , Task.attempt FocusUnboundActorInput (Dom.focus <| unboundActorInputTagId actorIndex)
            )

        RemoveUnboundActor actorIndex ->
            ( { model
                | systemContext = removeUnboundActor actorIndex model.systemContext
                , control = NoControl
              }
            , Cmd.none
            )

        DragStartUnboundActor actorIndex ->
            ( { model | control = ControlActorDragged <| UnboundActorDragged actorIndex }
            , Cmd.none
            )

        SortUnboundActor fromIndex toIndex ->
            ( { model
                | systemContext = sortUnboundActor fromIndex toIndex model.systemContext
                , control = ControlActorDragged <| UnboundActorDragged toIndex
              }
            , Cmd.none
            )

        DropOffActor actorIndex unboundIndex ->
            ( { model
                | systemContext = dropOffActor actorIndex unboundIndex model.systemContext
                , control = ControlActorDragged <| UnboundActorDragged unboundIndex
              }
            , Cmd.none
            )

        FocusUnboundActorInput _ ->
            ( model, Cmd.none )

        --common messages
        LeaveControl ->
            ( { model | control = NoControl }
            , Cmd.none
            )


type Msg
    = -- Actor messages
      PushActor Int
    | UpdateActorName Int String
    | ShowActorDropDown Int
    | OpenActorInput Int
    | FocusActorInput (Result Dom.Error ())
    | DragStartActor Int
    | SortActor Int Int
    | DropInActor Int Int
    | RemoveActor Int
      -- Unbound Actor messages
    | PushUnboundActor Int
    | UpdateUnboundActorName Int String
    | ShowUnboundActorDropDown Int
    | OpenUnboundActorInput Int
    | FocusUnboundActorInput (Result Dom.Error ())
    | DragStartUnboundActor Int
    | SortUnboundActor Int Int
    | DropOffActor Int Int
    | RemoveUnboundActor Int
      -- Requirement messages
    | LeaveControl


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
    = ControlActorEdit ActorEdit
    | ControlActorDragged ActorDragged
    | ExternalSystemEdit ExternalSystemEdit
    | ControlUnboundActorEdit UnboundActorEdit
    | ControlUnboundExternalSystemEdit UnboundExternalSystemEdit
    | NoControl


type UnboundExternalSystemEdit
    = UnboundExternalSystemDropDown ( Int, Int )
    | UnboundExternalSystemInput ( Int, Int )


type ActorEdit
    = ActorInput Int
    | ActorDropDown Int


type ActorDragged
    = ActorDragged Int
    | UnboundActorDragged Int


type ExternalSystemEdit
    = ExternalSystemDropDown ( Int, Int )
    | ExternalSystemInput ( Int, Int )


type UnboundActorEdit
    = UnboundActorDropDown Int
    | UnboundActorInput Int


getActorEdit : ModelControl -> Maybe ActorEdit
getActorEdit modelControl =
    case modelControl of
        ControlActorEdit edit ->
            Just edit

        _ ->
            Nothing


getActorDragged : ModelControl -> Maybe ActorDragged
getActorDragged modelControl =
    case modelControl of
        ControlActorDragged dragged ->
            Just dragged

        _ ->
            Nothing


getUnboundActorEdit : ModelControl -> Maybe UnboundActorEdit
getUnboundActorEdit modelControl =
    case modelControl of
        ControlUnboundActorEdit edit ->
            Just edit

        _ ->
            Nothing
