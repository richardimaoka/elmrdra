module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
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
        [ viewPurpose model.purpose
        , viewExplanation model.explanation
        , div [] (Array.toIndexedList model.actors |> List.map (\( index, actor ) -> viewActor index actor))
        , div [] (Array.toIndexedList model.externalSystems |> List.map (\( index, externalSystem ) -> viewExternalSystem index externalSystem))
        ]


viewPurpose : String -> Html Msg
viewPurpose purpose =
    let
        classes =
            [ class "p-1"
            , class "h-16"
            ]
    in
    if String.isEmpty purpose then
        div
            (style "opacity" "0.5" :: classes)
            [ text "please click here to fill in the purpose" ]

    else
        div classes [ text purpose ]


viewExplanation : String -> Html Msg
viewExplanation explanation =
    let
        classes =
            [ class "p-1"
            , class "h-20"
            ]
    in
    if String.isEmpty explanation then
        div
            (style "opacity" "0.5" :: classes)
            [ text "please click here to fill in the explanation" ]

    else
        div classes [ text explanation ]


viewActor : Int -> Actor -> Html Msg
viewActor index actor =
    div []
        [ viewActorSvg
        , viewStaticActorName index actor
        ]


viewActorSvg : Html Msg
viewActorSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ circle [ cx "25", cy "25", r "25" ] []
        ]


viewStaticActorName : Int -> Actor -> Html Msg
viewStaticActorName _ actor =
    div [] [ text <| actor ]


viewExternalSystem : Int -> ExternalSystem -> Html Msg
viewExternalSystem index externalSystem =
    div []
        [ viewExternalSystemSvg
        , viewStaticExternalSystemName index externalSystem
        ]


viewExternalSystemSvg : Html Msg
viewExternalSystemSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ rect [ x "0", y "0", width "50", height "50" ] []
        ]


viewStaticExternalSystemName : Int -> ExternalSystem -> Html Msg
viewStaticExternalSystemName _ externalSystem =
    div [] [ text <| externalSystem ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { purpose = ""
      , explanation = ""
      , actors = Array.fromList [ "hi", "ya", "all" ]
      , externalSystems = Array.fromList [ "hi-system", "ya-system", "all-system", "splutta" ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


type alias Model =
    { purpose : String
    , explanation : String
    , actors : Array Actor
    , externalSystems : Array ExternalSystem
    }


type Msg
    = NoOp


type alias Actor =
    String


type alias ExternalSystem =
    String
