module Hello exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Events.Extra.Mouse as Mouse


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


view : Model -> Html Msg
view model =
    div
        [ Mouse.onDown
            (\event ->
                StartDrag
                    { clientX = Tuple.first event.clientPos
                    , clientY = Tuple.second event.clientPos
                    }
            )
        , Mouse.onMove
            (\event ->
                Move
                    { clientX = Tuple.first event.clientPos
                    , clientY = Tuple.second event.clientPos
                    }
            )
        , Mouse.onUp (\_ -> Lift)
        , Mouse.onLeave (\_ -> Lift)
        ]
        [ text "click here" ]


{-| Msg
-}
type Msg
    = StartDrag ClientPosition
    | Move ClientPosition
    | Lift


{-| Model
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        StartDrag clientPosition ->
            DragStarted clientPosition

        Move currentPosition ->
            case model of
                Init ->
                    Init

                DragStarted startPosition ->
                    BeingDragged
                        { startPosition = startPosition, currentPosition = currentPosition }

                BeingDragged state ->
                    BeingDragged { startPosition = state.startPosition, currentPosition = currentPosition }

        Lift ->
            Init


init : Model
init =
    Init


type Model
    = Init
    | DragStarted ClientPosition
    | BeingDragged DraggedState


type alias DraggedState =
    { startPosition : ClientPosition
    , currentPosition : ClientPosition
    }


type alias ClientPosition =
    { clientX : Float
    , clientY : Float
    }


type alias OffsetPosition =
    { offsetX : Float
    , offsetY : Float
    }
