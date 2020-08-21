module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (class, id, style, value)
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
                    div
                        [ class "m-4"
                        , class "p-4"
                        ]
                        [ -- single requirement set
                          div []
                            [ div
                                [ style "width" "200px"
                                , class "m-4"
                                ]
                                [ viewActorSvg
                                , viewActorName actorIndex actorRequirement.actor model.selected
                                ]
                            , div
                                [ style "width" "300px"
                                , class "m-4"
                                , class "p-2"
                                ]
                              <|
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
    button
        [ class "bg-gray-400"
        , class "p-1"
        , onClick <| AddActor
        ]
        [ text "add actor" ]


viewActorSvg : Html Msg
viewActorSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ circle [ cx "25", cy "25", r "25" ] []
        ]


viewActorName : Int -> Actor -> Selection -> Html Msg
viewActorName actorIndex actor selection =
    div []
        (case selection of
            ActorInput selectActorIndex ->
                if actorIndex == selectActorIndex then
                    [ viewActorNameInput actorIndex actor ]

                else
                    [ viewStaticActorName actorIndex actor ]

            ActorDropDownActions selectActorIndex ->
                if actorIndex == selectActorIndex then
                    [ viewActorDropdown actorIndex actor ]

                else
                    [ viewStaticActorName actorIndex actor ]

            _ ->
                [ viewStaticActorName actorIndex actor ]
        )


viewStaticActorName : Int -> Actor -> Html Msg
viewStaticActorName actorIndex actor =
    div [ onClick <| ShowActorSelection actorIndex ] [ text <| actorName actor ]


viewActorNameInput : Int -> Actor -> Html Msg
viewActorNameInput actorIndex actor =
    input
        [ id <| actorInputTagId actorIndex
        , value <| actorName actor
        , onBlur GoStatic
        , onEnter GoStatic
        , onInput <| UpdateActorName actorIndex actor
        ]
        []


viewActorDropdown : Int -> Actor -> Html Msg
viewActorDropdown actorIndex actor =
    div []
        [ div [] [ text <| actorName actor ]
        , div [ onClick <| OpenActorInput actorIndex ] [ text "rename" ]
        , div [] [ text "delete" ]
        ]


actorInputTagId : Int -> String
actorInputTagId actorIndex =
    "input-actor-" ++ String.fromInt actorIndex


viewRequirementList : Int -> Array Requirement -> Selection -> List (Html Msg)
viewRequirementList actorIndex array selection =
    Array.toIndexedList array
        |> List.map
            (\( requirementIndex, requirement ) ->
                case selection of
                    NotSelected ->
                        viewRequirement actorIndex requirementIndex requirement

                    RequirementInput selectActorIndex selectRequirementIndex ->
                        if actorIndex == selectActorIndex && requirementIndex == selectRequirementIndex then
                            viewRequirementInput actorIndex requirementIndex requirement

                        else
                            viewRequirement actorIndex requirementIndex requirement

                    RequirementDropDownActions selectActorIndex selectRequirementIndex ->
                        if actorIndex == selectActorIndex && requirementIndex == selectRequirementIndex then
                            viewRequirementDropdown actorIndex requirementIndex requirement

                        else
                            viewRequirement actorIndex requirementIndex requirement

                    _ ->
                        viewRequirement actorIndex requirementIndex requirement
            )


viewRequirement : Int -> Int -> Requirement -> Html Msg
viewRequirement actorIndex requirementIndex requirement =
    div [ class "m-2" ]
        [ div [ onClick <| ShowRequirementSelection actorIndex requirementIndex ] [ text requirement.text ] ]


viewRequirementInput : Int -> Int -> Requirement -> Html Msg
viewRequirementInput actorIndex requirementIndex requirement =
    div []
        [ input
            [ id <| requirementInputTagId actorIndex requirementIndex
            , value requirement.text
            , onBlur GoStatic
            , onEnter GoStatic
            , onInput <| UpdateRequirementText actorIndex requirementIndex requirement
            ]
            []
        ]


requirementInputTagId : Int -> Int -> String
requirementInputTagId actorIndex requirementIndex =
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
        , div [ onClick <| DeleteRequirement actorIndex requirementIndex ] [ text "delete" ]
        ]


buttonAddRequirement : Int -> Html Msg
buttonAddRequirement actorIndex =
    button
        [ class "bg-gray-400"
        , class "p-1"
        , onClick <| AddRequirement actorIndex
        ]
        [ text "add requirement" ]


{-| Msg
-}
type
    Msg
    -- Requirement messages
    = AddRequirement Int
    | UpdateRequirementText Int Int Requirement String
    | ShowRequirementSelection Int Int
    | OpenRequirementInput Int Int
    | DeleteRequirement Int Int
      -- Actor messages
    | AddActor
    | UpdateActorName Int Actor String
    | ShowActorSelection Int
    | OpenActorInput Int
      -- common messages
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

                Just actorRequirement ->
                    let
                        indexNewRequirement =
                            Array.length actorRequirement.requirements
                    in
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { actorRequirement | requirements = Array.push { id = "a", text = "" } actorRequirement.requirements }
                                model.actorRequirements
                        , selected = RequirementInput actorIndex indexNewRequirement
                      }
                    , Task.attempt Focus (Dom.focus <| requirementInputTagId actorIndex indexNewRequirement)
                    )

        UpdateRequirementText actorIndex requirementIndex requirement newText ->
            case Array.get actorIndex model.actorRequirements of
                Nothing ->
                    ( model, Cmd.none )

                Just actorRequirement ->
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { actorRequirement | requirements = Array.set requirementIndex { requirement | text = newText } actorRequirement.requirements }
                                model.actorRequirements
                      }
                    , Cmd.none
                    )

        ShowRequirementSelection actorIndex requirementIndex ->
            ( { model | selected = RequirementDropDownActions actorIndex requirementIndex }
            , Cmd.none
            )

        OpenRequirementInput actorIndex requirementIndex ->
            ( { model | selected = RequirementInput actorIndex requirementIndex }
            , Task.attempt Focus (Dom.focus <| requirementInputTagId actorIndex requirementIndex)
            )

        UpdateActorName actorIndex (Actor actor) newName ->
            case Array.get actorIndex model.actorRequirements of
                Nothing ->
                    ( model, Cmd.none )

                Just actorRequirement ->
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { actorRequirement | actor = Actor { actor | name = newName } }
                                model.actorRequirements
                      }
                    , Cmd.none
                    )

        ShowActorSelection actorIndex ->
            ( { model | selected = ActorDropDownActions actorIndex }
            , Cmd.none
            )

        OpenActorInput actorIndex ->
            ( { model | selected = ActorInput actorIndex }
            , Task.attempt Focus (Dom.focus <| actorInputTagId actorIndex)
            )

        DeleteRequirement actorIndex requirementIndex ->
            case Array.get actorIndex model.actorRequirements of
                Nothing ->
                    ( model, Cmd.none )

                Just actorRequirement ->
                    let
                        arr1 =
                            Array.slice 0 requirementIndex actorRequirement.requirements

                        arr2 =
                            Array.slice (requirementIndex + 1) (Array.length actorRequirement.requirements) actorRequirement.requirements
                    in
                    ( { model
                        | actorRequirements =
                            Array.set
                                actorIndex
                                { actorRequirement | requirements = Array.append arr1 arr2 }
                                model.actorRequirements
                        , selected = NotSelected
                      }
                    , Cmd.none
                    )

        GoStatic ->
            ( { model | selected = NotSelected }, Cmd.none )

        AddActor ->
            let
                indexNewActor =
                    Array.length model.actorRequirements
            in
            ( { model
                | actorRequirements = Array.push { actor = Actor { id = "a", name = "" }, requirements = Array.empty } model.actorRequirements
                , selected = ActorInput indexNewActor
              }
            , Task.attempt Focus (Dom.focus <| actorInputTagId indexNewActor)
            )

        Focus _ ->
            ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actorRequirements =
            Array.fromList
                [ { actor = Actor { id = "", name = "actrrr" }, requirements = Array.fromList [ { id = "", text = "some requirement" }, { id = "", text = "some requirement" } ] }
                , { actor = Actor { id = "", name = "actrrr" }, requirements = Array.fromList [ { id = "", text = "some requirement" }, { id = "", text = "some requirement" }, { id = "", text = "some requirement" } ] }
                , { actor = Actor { id = "", name = "actrrr" }, requirements = Array.fromList [] }
                ]
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
    | RequirementInput Int Int
    | RequirementDropDownActions Int Int
    | ActorInput Int
    | ActorDropDownActions Int


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
