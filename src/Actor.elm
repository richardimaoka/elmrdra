module Actor exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, height, r, style, transform, viewBox, width)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


view : Model -> Html Msg
view model =
    div []
        [ svg [ width "100", height "100", viewBox "0 0 100 100" ]
            [ circle [ cx "50", cy "50", r "50" ] []
            ]
        , div [] [ input [ value model, onInput Input ] [] ]
        ]


{-| Msg
-}
type Msg
    = Input String


{-| Model
-}
update : Msg -> Model -> Model
update msg _ =
    case msg of
        Input inputText ->
            inputText


init : Model
init =
    "hel"


type alias Model =
    String
