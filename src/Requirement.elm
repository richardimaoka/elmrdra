module Requirement exposing (view)

import Html exposing (Html, div, input, text)
import Html.Events exposing (onInput)


view : Model -> (Msg -> msg) -> Html msg
view model toMsg =
    div [] [ viewRequirementText model toMsg ]


viewRequirementText : String -> (Msg -> msg) -> Html msg
viewRequirementText requirementText toMsg =
    div []
        [ div [] [ text requirementText ]
        , input [ onInput (UpdateRequirementText >> toMsg) ] []
        ]


{-| Msg
-}
type Msg
    = UpdateRequirementText String


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRequirementText newText ->
            ( newText, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s"
    , Cmd.none
    )


type alias Model =
    String
