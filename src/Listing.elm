module Listing exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, button, div, text, textarea)
import Html.Attributes exposing (class, id, value)
import Html.Events exposing (keyCode, onBlur, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
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
    div [ class "max-w-xs" ]
        (List.append
            (List.map requirementView model.requirements)
            [ case model.input of
                Just requirementInput ->
                    requirementInputView requirementInput.inputText

                Nothing ->
                    addButton
            ]
        )


addButton : Html Msg
addButton =
    button [ onClick OpenTextArea ] [ text "add" ]


requirementInputView : String -> Html Msg
requirementInputView requirementText =
    div []
        [ textarea
            [ id "requirement-input"
            , value requirementText
            , onInput Input
            , onBlur CancelTextArea
            , onEnter FinishTextArea
            ]
            []
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
    preventDefaultOn "keydown" (Decode.map alwaysPreventDefault (Decode.andThen isEnter keyCode))


requirementView : String -> Html Msg
requirementView requirementText =
    div []
        [ div [] [ text requirementText ]
        ]


{-| Msg
-}
type Msg
    = OpenTextArea
    | Input String
    | CancelTextArea
    | FinishTextArea
    | Focus (Result Dom.Error ())


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenTextArea ->
            ( { model | input = Just { name = "nnnname", inputText = "" } }
            , Task.attempt Focus (Dom.focus "requirement-input")
            )

        Input requirementText ->
            ( { model | input = Just { name = "nnnname", inputText = requirementText } }, Cmd.none )

        CancelTextArea ->
            ( { model | input = Nothing }, Cmd.none )

        FinishTextArea ->
            case model.input of
                Just requirementInput ->
                    ( { input = Nothing, requirements = List.append model.requirements [ requirementInput.inputText ] }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Focus _ ->
            ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = Nothing
      , requirements = []
      }
    , Cmd.none
    )


type alias Model =
    { input : Maybe RequirementInput
    , requirements : List String
    }


type alias RequirementInput =
    { name : String
    , inputText : String
    }
