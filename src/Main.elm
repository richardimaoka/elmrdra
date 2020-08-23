module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Browser.Dom as Dom
import Data.RequirementModel as RequirementModel exposing (RequirementModel, getActorRequirements, initialize)
import Dict exposing (fromList)
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (keyCode, on, preventDefaultOn)
import Json.Decode as Decode
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width, x, y)


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
                        viewActorRequirementBox actorIndex actorName requirements control
                    )
            )
            [ buttonAddActorRequirement ]
        )


buttonAddActorRequirement : Html Msg
buttonAddActorRequirement =
    button
        [ class "bg-gray-400"
        , class "p-1"
        ]
        [ text "add actor" ]


viewActorRequirementBox : Int -> String -> RequirementArray -> ModelControl -> Html Msg
viewActorRequirementBox actorIndex actorName requirements control =
    let
        maybeActorControl =
            getActorControl control

        maybeRequirementControl =
            getRequirementControl control
    in
    div []
        [ viewActor actorIndex maybeActorControl actorName
        , viewRequirementList actorIndex maybeRequirementControl requirements
        ]


viewActor : Int -> Maybe ( ActorControl, Int ) -> String -> Html Msg
viewActor actorIndex maybeControl actorName =
    div
        [ class "m-4"
        , class "p-4"
        , style "max-width" "200px"
        ]
        [ viewActorSvg
        , case maybeControl of
            Nothing ->
                viewStaticActorName actorIndex actorName

            Just ( actorControl, selectIndex ) ->
                if actorIndex == selectIndex then
                    case actorControl of
                        ActorDropDown ->
                            viewStaticActorName actorIndex actorName

                        ActorInput ->
                            viewStaticActorName actorIndex actorName

                        ActorDragged ->
                            viewStaticActorName actorIndex actorName

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
    div [] [ text <| actorName ]


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
            [ buttonAddRequirement actorIndex ]
        )


buttonAddRequirement : Int -> Html Msg
buttonAddRequirement actorIndex =
    button
        [ class "bg-gray-400"
        , class "p-1"

        --        , onClick <| AddRequirement actorIndex
        ]
        [ text "add requirement" ]


viewRequirement : ( Int, Int ) -> Maybe ( RequirementControl, ( Int, Int ) ) -> String -> Html Msg
viewRequirement ( actorIndex, requirementIndex ) maybeControl requirementContent =
    div [ class "m-2" ]
        [ case maybeControl of
            Nothing ->
                text requirementContent

            Just ( actorControl, ( selectActorIndex, selectRequirementIndex ) ) ->
                if actorIndex == selectActorIndex && requirementIndex == selectRequirementIndex then
                    case actorControl of
                        RequirementDropDown ->
                            text requirementContent

                        RequirementInput ->
                            text requirementContent

                        RequirementDragged ->
                            text requirementContent

                else
                    text requirementContent
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"

        alwaysPreventDefault =
            \code -> ( code, True )
    in
    -- preventDefaultOn not to trigger onBlur, and alwaysPreventDefault to always prevent onBlur
    preventDefaultOn "keydown" (Decode.map alwaysPreventDefault (Decode.andThen isEnter keyCode))


onDragStart : Msg -> Attribute Msg
onDragStart msg =
    on "dragstart" <| Decode.succeed msg


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
                    [ ( "actor1", [ "req1", "req2" ] )
                    , ( "actor2", [ "req1", "req2" ] )
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
    ( model, Cmd.none )


type Msg
    = -- Actor messages
      PushActor
    | UpdateActorName Int String
    | ShowActorDropDown Int
    | OpenActorInput Int
    | DragStartActor Int Int
    | DragEndActor Int Int
    | DragEnterActor Int Int
    | FocusActorInput (Result Dom.Error ())
      -- Requirement messages
    | PushRequirement Int
    | UpdateRequirementContent Int Int String
    | ShowRequirementDropDown Int Int
    | OpenRequirementInput Int Int
    | DeleteRequirement Int Int
    | DragStartRequirement Int Int
    | DragEndRequirement Int Int
    | DragEnterRequirement Int Int
    | FocusRequirementInput (Result Dom.Error ())


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


type ModelControl
    = RequirementControl RequirementControl ( Int, Int )
    | ActorControl ActorControl Int
    | NoControl


getActorControl : ModelControl -> Maybe ( ActorControl, Int )
getActorControl control =
    case control of
        RequirementControl _ _ ->
            Nothing

        ActorControl actorControl selectIndex ->
            Just ( actorControl, selectIndex )

        NoControl ->
            Nothing


getRequirementControl : ModelControl -> Maybe ( RequirementControl, ( Int, Int ) )
getRequirementControl control =
    case control of
        RequirementControl requirementControl selectIndexTuple ->
            Just ( requirementControl, selectIndexTuple )

        ActorControl _ _ ->
            Nothing

        NoControl ->
            Nothing


type RequirementControl
    = RequirementDropDown
    | RequirementInput
    | RequirementDragged


type ActorControl
    = ActorDropDown
    | ActorInput
    | ActorDragged
