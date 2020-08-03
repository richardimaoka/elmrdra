module TextArea exposing (main)

import Browser
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


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
        [ div [] [ textarea [ value model, onInput Input ] [] ]
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
            "inputText"


init : Model
init =
    "hel"


type alias Model =
    String
