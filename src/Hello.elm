port module Hello exposing (main, requestSvgMatrix, svgMatrixSubscription)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (id, style)
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
    case model of
        State _ svgOuter ->
            svgOuterView svgOuter



-- , rect
--     [ id "rect"
--     , Mouse.onDown (.clientPos >> DetectDragStart)
--     , Mouse.onMove
--         (\event ->
--             if isDraggableState model then
--                 ContinueDragging event.clientPos
--             else
--                 NoOp
--         )
--     , Mouse.onUp (\_ -> EndDragging)
--     , Mouse.onLeave (\_ -> EndDragging)
--     , x "0"
--     , y "0"
--     , width "8"
--     , height "10"
--     , fill "#007bff"
--     , transform (transformAttribute model)
--     ]


svgOuterView : SvgOuter -> Html Msg
svgOuterView svgOuter =
    svg
        [ id "svg-outer"
        , height (String.fromFloat svgOuter.height)
        , width (String.fromFloat svgOuter.width)
        , viewBox (viewBoxString svgOuter.viewBox)
        , style "border-style" "solid"
        ]
        (List.map
            svgRectView
            svgOuter.children
        )


svgRectView : SvgRectangle -> Html Msg
svgRectView svgRect =
    rect
        [ id svgRect.id
        , width (String.fromFloat svgRect.width)
        , height (String.fromFloat svgRect.height)
        , transform (transformString svgRect.transform)
        , fill svgRect.fillColor
        ]
        []


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


port requestSvgMatrix : String -> Cmd msg


{-| Model
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveSvgMatrix svgMatrix ->
            case model of
                State dragState svgOuter ->
                    ( State dragState { svgOuter | matrix = svgMatrix }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- case msg of
--     NoOp ->
--         ( model, Cmd.none )
--     ReceiveSvgMatrix svgMatrix ->
--         case model of
--             Init ->
--                 ( SvgMatrixReceived svgMatrix, Cmd.none )
--             DragPositionDetected pos ->
--                 ( DragStarted svgMatrix pos, Cmd.none )
--             _ ->
--                 ( model, Cmd.none )
--     DetectDragStart pos ->
--         case model of
--             Init ->
--                 ( DragPositionDetected pos, Cmd.none )
--             SvgMatrixReceived matrix ->
--                 ( DragStarted matrix pos, Cmd.none )
--             _ ->
--                 ( model, Cmd.none )
--     ContinueDragging currentPos ->
--         case model of
--             DragStarted svgMatrix startPos ->
--                 ( BeingDragged
--                     { svgMatrix = svgMatrix
--                     , dragStartPos = startPos
--                     , dragCurrentPos = currentPos
--                     , svgTransform = ( 0.0, 0.0 )
--                     }
--                 , Cmd.none
--                 )
--             BeingDragged state ->
--                 ( BeingDragged
--                     { state
--                         | dragCurrentPos = currentPos
--                         , svgTransform = mult state.svgMatrix state.dragStartPos currentPos
--                     }
--                 , Cmd.none
--                 )
--             _ ->
--                 ( model, Cmd.none )
--     EndDragging ->
--         case model of
--             BeingDragged state ->
--                 ( DragEnded state.svgTransform, Cmd.none )
--             _ ->
--                 ( Init, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( State UnDragged
        { width = 600
        , height = 500
        , viewBox = { minX = 0, minY = 0, width = 30, height = 20 }
        , matrix = { a = 0, b = 0, c = 0, d = 0, e = 0, f = 0 }
        , children =
            [ { id = "dhu", width = 30, height = 20, transform = ( 0, 0 ), fillColor = "#98e612" }
            , { id = "bbb", width = 8, height = 10, transform = ( 4, 5 ), fillColor = "#007bff" }
            , { id = "bbb", width = 8, height = 10, transform = ( 18, 5 ), fillColor = "#888" }
            ]
        }
    , requestSvgMatrix "svg-outer"
    )


type Model
    = State DragState SvgOuter


type DragState
    = UnDragged
    | Dragged
        { startPos : ClientPosition
        , currentPos : ClientPosition
        }


type alias ClientPosition =
    ( Float, Float )


type alias UserPosition =
    ( Float, Float )


type alias SvgTransform =
    ( Float, Float )


transformString : SvgTransform -> String
transformString svgTransform =
    let
        x =
            String.fromFloat (Tuple.first svgTransform)

        y =
            String.fromFloat (Tuple.second svgTransform)
    in
    "translate (" ++ x ++ " " ++ y ++ ")"


type alias SvgMatrix =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    , f : Float
    }


type alias SvgOuter =
    { width : Float
    , height : Float
    , viewBox : SvgViewBox
    , matrix : SvgMatrix
    , children : List SvgRectangle
    }


mult : SvgMatrix -> ClientPosition -> UserPosition
mult inverse clientPos =
    let
        clientX =
            Tuple.first clientPos

        clientY =
            Tuple.second clientPos

        userX =
            inverse.a * clientX + inverse.c * clientY + inverse.e

        userY =
            inverse.b * clientX + inverse.d * clientY + inverse.f
    in
    ( userX, userY )


type alias SvgRectangle =
    { id : String
    , height : Float
    , width : Float
    , transform : SvgTransform
    , fillColor : String
    }


type alias SvgViewBox =
    { minX : Float
    , minY : Float
    , width : Float
    , height : Float
    }


viewBoxString : SvgViewBox -> String
viewBoxString viewBox =
    let
        minX =
            String.fromFloat viewBox.minX

        minY =
            String.fromFloat viewBox.minY

        width =
            String.fromFloat viewBox.width

        height =
            String.fromFloat viewBox.height
    in
    minX ++ " " ++ minY ++ " " ++ width ++ " " ++ height
