module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Data.RequirementModel as RequirementModel exposing (RequirementModel, getActorRequirements, initialize)
import Dict exposing (fromList)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
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
    div []
        [ viewActorRequirementPanel <| RequirementModel.getActorRequirements model
        ]


viewActorRequirementPanel : ActorRequirementArray -> Html Msg
viewActorRequirementPanel array =
    div [ style "max-width" "600px" ]
        (Array.toIndexedList
            array
            |> List.map
                (\( actorIndex, ( actorName, requirements ) ) ->
                    viewActorRequirementBox actorIndex actorName requirements
                )
        )


viewActorRequirementBox : Int -> String -> RequirementArray -> Html Msg
viewActorRequirementBox actorIndex actorName requirements =
    div []
        [ viewActor actorIndex actorName
        , viewRequirementList actorIndex requirements
        ]


viewActor : Int -> String -> Html Msg
viewActor actorIndex actorName =
    div
        [ class "m-4"
        , class "p-4"
        , style "max-width" "200px"
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
    div [] [ text <| actorName ]


viewRequirementList : Int -> RequirementArray -> Html Msg
viewRequirementList actorIndex requirements =
    div
        [ class "m-4"
        , class "p-4"
        , style "max-width" "400px"
        ]
        (Array.toIndexedList requirements
            |> List.map
                (\( requirementIndex, requirementContent ) -> viewRequirement ( actorIndex, requirementIndex ) requirementContent)
        )


viewRequirement : ( Int, Int ) -> String -> Html Msg
viewRequirement ( actorIndex, requirementIndex ) requirementContent =
    div [ class "m-2" ] [ text requirementContent ]



-- viewPlaceHolderActorPanel : ActorList -> Html Msg
-- viewPlaceHolderRequirementPanel : RequirementList -> Html Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( RequirementModel.initialize
        (Dict.fromList
            [ ( "actor1", [ "req1", "req2" ] )
            , ( "actor2", [ "req1", "req2" ] )
            ]
        )
        [ "aaa" ]
        [ "bbb" ]
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


type Msg
    = NoOp


type alias Model =
    RequirementModel


type alias ActorRequirementArray =
    Array ( String, Array String )


type alias ActorArray =
    Array String


type alias RequirementArray =
    Array String
