module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (keyCode, onBlur, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width)
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
    div []
        (Array.toIndexedList
            model.actorRequirements
            |> List.map
                (\( actorIndex, actorRequirement ) ->
                    div []
                        [ -- single requirement set
                          div []
                            [ div []
                                [ viewActorSvg
                                , viewActorName actorRequirement.actor
                                ]
                            , div [] <|
                                List.append
                                    (viewRequirementList 0 actorRequirement.requirements model.selected)
                                    [ buttonAddRequirement actorIndex ]
                            ]

                        -- button to add actor and requirement
                        , buttonAddActorRequirement
                        ]
                )
        )


buttonAddActorRequirement : Html Msg
buttonAddActorRequirement =
    button [] [ text "add" ]


viewActorSvg : Html Msg
viewActorSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ circle [ cx "25", cy "25", r "25" ] []
        ]


viewActorName : Actor -> Html Msg
viewActorName actor =
    div [] [ text <| actorName actor ]


viewActorNameInput : Actor -> Html Msg
viewActorNameInput actor =
    input
        [ id <| "input-" ++ actorId actor
        , value <| actorName actor
        ]
        []


viewRequirementList : Int -> Array Requirement -> Selection -> List (Html Msg)
viewRequirementList actorIndex array selection =
    Array.toIndexedList array
        |> List.map
            (\( requirementIndex, requirement ) ->
                case selection of
                    NotSelected ->
                        viewRequirement actorIndex requirementIndex requirement

                    Input selectActorIndex selectRequirementIndex ->
                        if actorIndex == selectActorIndex && requirementIndex == selectRequirementIndex then
                            viewRequirementInput actorIndex requirementIndex requirement

                        else
                            viewRequirement actorIndex requirementIndex requirement

                    DropDownActions selectActorIndex selectRequirementIndex ->
                        if actorIndex == selectActorIndex && requirementIndex == selectRequirementIndex then
                            viewRequirementDropdown actorIndex requirementIndex requirement

                        else
                            viewRequirement actorIndex requirementIndex requirement
            )


viewRequirement : Int -> Int -> Requirement -> Html Msg
viewRequirement actorIndex requirementIndex requirement =
    div []
        [ div [ onClick <| ShowRequirementSelection actorIndex requirementIndex ] [ text requirement.text ] ]


viewRequirementInput : Int -> Int -> Requirement -> Html Msg
viewRequirementInput actorIndex requirementIndex requirement =
    div []
        [ input
            [ id <| inputHtmlTagId actorIndex requirementIndex
            , value requirement.text
            , onBlur GoStatic
            , onEnter GoStatic
            , onInput <| UpdateRequirementText actorIndex requirementIndex requirement
            ]
            []
        ]


inputHtmlTagId : Int -> Int -> String
inputHtmlTagId actorIndex requirementIndex =
    "input-requirement-actor-" ++ String.fromInt actorIndex ++ "-requirement-" ++ String.fromInt requirementIndex


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


viewRequirementDropdown : Int -> Int -> Requirement -> Html Msg
viewRequirementDropdown actorIndex requirementIndex requirement =
    div []
        [ div [] [ text requirement.text ]
        , div [ onClick <| OpenRequirementInput actorIndex requirementIndex ] [ text "rename" ]
        , div [ onClick <| DeleteRequirement actorIndex requirementIndex ] [ text "DeleteRequirement" ]
        ]


buttonAddRequirement : Int -> Html Msg
buttonAddRequirement actorIndex =
    button [ onClick <| AddRequirement actorIndex ] [ text "add" ]


{-| Msg
-}
type Msg
    = AddRequirement Int
    | UpdateRequirementText Int Int Requirement String
    | ShowRequirementSelection Int Int
    | OpenRequirementInput Int Int
    | DeleteRequirement Int Int
    | GoStatic
    | Focus (Result Dom.Error ())


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRequirement actorIndex ->
            case Array.get actorIndex model.actorRequirements of
                Nothing ->
                    ( model, Cmd.none )

                Just acotRequirement ->
                    let
                        indexNewRequirement =
                            Array.length acotRequirement.requirements
                    in
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { acotRequirement | requirements = Array.push { id = "a", text = "" } acotRequirement.requirements }
                                model.actorRequirements
                        , selected = Input actorIndex indexNewRequirement
                      }
                    , Task.attempt Focus (Dom.focus <| inputHtmlTagId actorIndex indexNewRequirement)
                    )

        UpdateRequirementText actorIndex requirementIndex requirement newText ->
            case Array.get actorIndex model.actorRequirements of
                Nothing ->
                    ( model, Cmd.none )

                Just acotRequirement ->
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { acotRequirement | requirements = Array.set requirementIndex { requirement | text = newText } acotRequirement.requirements }
                                model.actorRequirements
                      }
                    , Cmd.none
                    )

        ShowRequirementSelection actorIndex requirementIndex ->
            ( { model | selected = DropDownActions actorIndex requirementIndex }
            , Cmd.none
            )

        OpenRequirementInput actorIndex requirementIndex ->
            ( { model | selected = Input actorIndex requirementIndex }
            , Task.attempt Focus (Dom.focus <| inputHtmlTagId actorIndex requirementIndex)
            )

        DeleteRequirement actorIndex requirementIndex ->
            case Array.get actorIndex model.actorRequirements of
                Nothing ->
                    ( model, Cmd.none )

                Just acotRequirement ->
                    let
                        arr1 =
                            Array.slice 0 requirementIndex acotRequirement.requirements

                        arr2 =
                            Array.slice (requirementIndex + 1) (Array.length acotRequirement.requirements) acotRequirement.requirements
                    in
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { acotRequirement | requirements = Array.append arr1 arr2 }
                                model.actorRequirements
                        , selected = NotSelected
                      }
                    , Cmd.none
                    )

        GoStatic ->
            ( { model | selected = NotSelected }, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actorRequirements = Array.fromList [ { actor = Actor { id = "", name = "actrrr" }, requirements = Array.fromList [] } ]
      , selected = NotSelected
      }
    , Cmd.none
    )


type alias Model =
    { actorRequirements : Array ActorRequirement
    , selected : Selection
    }


type Selection
    = NotSelected
    | Input Int Int
    | DropDownActions Int Int


type alias ActorRequirement =
    { actor : Actor
    , requirements : Array Requirement
    }


type alias Requirement =
    { id : String
    , text : String
    }


type Actor
    = Actor
        { id : String
        , name : String
        }


actorName : Actor -> String
actorName (Actor record) =
    record.name


actorId : Actor -> String
actorId (Actor record) =
    record.id
