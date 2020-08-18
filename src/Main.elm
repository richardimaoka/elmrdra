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
        [ -- single requirement set
          div []
            [ div []
                [ viewActorSvg
                , viewActorName model.actor
                ]
            , div [] <|
                List.append
                    (viewRequirementList model.requirements model.selected)
                    [ buttonAddRequirement ]
            ]

        -- button to add actor and requirement
        , buttonAddActorRequirement
        ]


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


viewRequirementList : Array Requirement -> Selection -> List (Html Msg)
viewRequirementList array selection =
    Array.toIndexedList array
        |> List.map
            (\( index, requirement ) ->
                case selection of
                    NotSelected ->
                        viewRequirement ( index, requirement )

                    Input selectIndex ->
                        if selectIndex == index then
                            viewRequirementInput ( index, requirement )

                        else
                            viewRequirement ( index, requirement )

                    DropDownActions selectIndex ->
                        if selectIndex == index then
                            viewRequirementDropdown ( index, requirement )

                        else
                            viewRequirement ( index, requirement )
            )


viewRequirement : ( Int, Requirement ) -> Html Msg
viewRequirement ( index, requirement ) =
    div []
        [ div [ onClick <| ShowSelection index ] [ text requirement.text ] ]


viewRequirementInput : ( Int, Requirement ) -> Html Msg
viewRequirementInput ( index, requirement ) =
    div []
        [ input
            [ id <| inputHtmlTagId index
            , value requirement.text
            , onBlur GoStatic
            , onEnter GoStatic
            , onInput <| UpdateText ( index, requirement )
            ]
            []
        ]


inputHtmlTagId : Int -> String
inputHtmlTagId index =
    "input-requirement-" ++ String.fromInt index


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


viewRequirementDropdown : ( Int, Requirement ) -> Html Msg
viewRequirementDropdown ( index, requirement ) =
    div []
        [ div [] [ text requirement.text ]
        , div [ onClick <| OpenInput index ] [ text "rename" ]
        , div [ onClick <| Delete index ] [ text "delete" ]
        ]


buttonAddRequirement : Html Msg
buttonAddRequirement =
    button [ onClick AddRequirement ] [ text "add" ]


{-| Msg
-}
type Msg
    = AddRequirement
    | UpdateText ( Int, Requirement ) String
    | ShowSelection Int
    | OpenInput Int
    | Delete Int
    | GoStatic
    | Focus (Result Dom.Error ())


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRequirement ->
            let
                indexNewRequirement =
                    Array.length model.requirements
            in
            ( { model
                | requirements = Array.push { id = "a", text = "" } model.requirements
                , selected = Input indexNewRequirement
              }
            , Task.attempt Focus (Dom.focus <| inputHtmlTagId indexNewRequirement)
            )

        UpdateText ( index, requirement ) newText ->
            ( { model
                | requirements = Array.set index { requirement | text = newText } model.requirements
              }
            , Cmd.none
            )

        ShowSelection index ->
            ( { model | selected = DropDownActions index }
            , Cmd.none
            )

        OpenInput index ->
            ( { model | selected = Input index }
            , Task.attempt Focus (Dom.focus <| inputHtmlTagId index)
            )

        Delete index ->
            let
                arr1 =
                    Array.slice 0 index model.requirements

                arr2 =
                    Array.slice (index + 1) (Array.length model.requirements) model.requirements
            in
            ( { model
                | requirements = Array.append arr1 arr2
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
    ( { actor = Actor { id = "", name = "actrrr" }
      , requirements = Array.fromList []
      , selected = NotSelected
      }
    , Cmd.none
    )


type alias Model =
    { actor : Actor
    , requirements : Array Requirement
    , selected : Selection
    }


type alias Requirement =
    { id : String
    , text : String
    }


type Selection
    = NotSelected
    | Input Int
    | DropDownActions Int


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
