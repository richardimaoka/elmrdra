module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, div, input, text)
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
    case model of
        Editing actor ->
            div
                []
                [ viewActorSvg
                , div [] [ viewActorNameInput actor ]
                ]

        Fixed maybeActor ->
            case maybeActor of
                Nothing ->
                    div [] []

                Just actor ->
                    div [ onClick StartEditing ]
                        [ viewActorSvg
                        , div [] [ text <| actorName actor ]
                        ]


viewActorSvg : Html Msg
viewActorSvg =
    svg [ width "50", height "50", viewBox "0 0 50 50" ]
        [ circle [ cx "25", cy "25", r "25" ] []
        ]


viewActorNameInput : Actor -> Html Msg
viewActorNameInput actor =
    input
        [ id <| "input-" ++ actorId actor
        , value <| actorName actor
        , onInput EditName
        , onBlur FinishEditingName
        , onEnter FinishEditingName
        ]
        []


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


{-| Msg
-}
type Msg
    = EditName String
    | FinishEditingName
    | StartEditing
    | Focus (Result Dom.Error ())


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( StartEditing, Fixed (Just actor) ) ->
            ( Editing actor, Task.attempt Focus (Dom.focus "input-actor1") )

        ( EditName inputText, Editing (Actor record) ) ->
            ( Editing <| Actor { record | name = inputText }, Cmd.none )

        ( FinishEditingName, Editing actor ) ->
            if String.isEmpty <| actorName actor then
                ( Fixed Nothing, Cmd.none )

            else
                ( Fixed <| Just actor, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Editing <| Actor { id = "actor1", name = "hel" }
    , Task.attempt Focus (Dom.focus "input-actor1")
    )


type Model
    = Editing Actor
    | Fixed (Maybe Actor)


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
