port module Hello exposing (main, svgMatrixSubscription)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (id)
import Html.Events.Extra.Mouse as Mouse
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, transform, viewBox, width, x, y)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    svg
        [ id "rect"
        , height "400px"
        , width "400px"
        , viewBox "0 0 30 20"
        ]
        [ rect [ x "0", y "0", width "30", height "20", fill "#fafafa" ] []
        , rect
            [ id "rect"
            , Mouse.onDown (.clientPos >> DetectDragStart)
            , Mouse.onMove
                (\event ->
                    if isDraggableState model then
                        ContinueDragging event.clientPos

                    else
                        NoOp
                )
            , Mouse.onUp (\_ -> EndDragging)
            , Mouse.onLeave (\_ -> EndDragging)
            , x "0"
            , y "0"
            , width "8"
            , height "10"
            , fill "#007bff"
            , transform (transformAttribute model)
            ]
            []
        , rect [ x "18", y "5", width "8", height "10", fill "#888" ] []
        ]


transformAttribute : Model -> String
transformAttribute model =
    case model of
        BeingDragged state ->
            let
                x =
                    Tuple.first state.svgTransform

                y =
                    Tuple.second state.svgTransform
            in
            "translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"

        _ ->
            ""


{-| Msg
-}
type Msg
    = NoOp
    | ReceiveSvgMatrix SvgMatrix
    | DetectDragStart ( Float, Float )
    | ContinueDragging ( Float, Float )
    | EndDragging


subscriptions : Model -> Sub Msg
subscriptions _ =
    svgMatrixSubscription ReceiveSvgMatrix


port svgMatrixSubscription : (SvgMatrix -> msg) -> Sub msg


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceiveSvgMatrix svgMatrix ->
            case model of
                Init ->
                    ( SvgMatrixReceived svgMatrix, Cmd.none )

                DragPositionDetected pos ->
                    ( DragStarted svgMatrix pos, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DetectDragStart pos ->
            case model of
                Init ->
                    ( DragPositionDetected pos, Cmd.none )

                SvgMatrixReceived matrix ->
                    ( DragStarted matrix pos, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ContinueDragging currentPos ->
            case model of
                DragStarted svgMatrix startPos ->
                    ( BeingDragged
                        { svgMatrix = svgMatrix
                        , dragStartPos = startPos
                        , dragCurrentPos = currentPos
                        , svgTransform = ( 0.0, 0.0 )
                        }
                    , Cmd.none
                    )

                BeingDragged state ->
                    ( BeingDragged
                        { state
                            | dragCurrentPos = currentPos
                            , svgTransform = mult state.svgMatrix state.dragStartPos currentPos
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EndDragging ->
            case model of
                BeingDragged state ->
                    ( DragEnded state.svgTransform, Cmd.none )

                _ ->
                    ( Init, Cmd.none )


mult : SvgMatrix -> ClientPosition -> ClientPosition -> SvgTransform
mult svgMatrix dragStartPos dragCurrentPos =
    let
        dxClient =
            Tuple.first dragCurrentPos - Tuple.first dragStartPos

        dyClient =
            Tuple.second dragCurrentPos - Tuple.second dragStartPos

        dxSvg =
            svgMatrix.a * dxClient + svgMatrix.c * dyClient + svgMatrix.e

        dySvg =
            svgMatrix.b * dxClient + svgMatrix.d * dyClient + svgMatrix.f
    in
    ( dxSvg, dySvg )


type Model
    = Init
    | SvgMatrixReceived SvgMatrix
    | DragPositionDetected ClientPosition
    | DragStarted SvgMatrix ClientPosition
    | BeingDragged
        { svgMatrix : SvgMatrix
        , dragStartPos : ClientPosition
        , dragCurrentPos : ClientPosition
        , svgTransform : SvgTransform
        }
    | DragEnded SvgTransform


isDraggableState : Model -> Bool
isDraggableState model =
    case model of
        DragStarted _ _ ->
            True

        BeingDragged _ ->
            True

        _ ->
            False


type alias ClientPosition =
    ( Float, Float )


type alias SvgTransform =
    ( Float, Float )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init, Cmd.none )



-- | DragStarted SvgMatrix
-- | BeingDragged SvgMatrix


type alias SvgMatrix =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    , f : Float
    }
