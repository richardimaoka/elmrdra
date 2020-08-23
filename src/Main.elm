module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Data.RequirementModel as RequirementModel exposing (RequirementModel, initialize)
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
        [ text "balloonn" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( RequirementModel.initialize
        (Dict.fromList [ ( "actor1", [ "req1", "req2" ] ) ])
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
