port module Hello exposing (main, requestSvgMatrix, svgMatrixSubscription)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Mouse as Mouse
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, transform, viewBox, width, x, y)
import Tuple exposing (first, second)


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
    svgOuterView model.svgOuter


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
        , Mouse.onDown (\event -> StartDrag svgRect.id event.clientPos)
        , Mouse.onMove (.clientPos >> KeepDragging)
        , Mouse.onLeave (\_ -> UnDrag)
        , Mouse.onUp (\_ -> UnDrag)
        ]
        []


{-| Msg
-}
type Msg
    = ReceiveSvgMatrix SvgMatrix
    | UnDrag
    | StartDrag String ClientPosition
    | KeepDragging ClientPosition


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
            ( updateReceiveSvgMatrix model svgMatrix, Cmd.none )

        StartDrag id clientPos ->
            ( updateStartDrag model id clientPos, Cmd.none )

        KeepDragging clientPos ->
            ( updateKeepDragging model clientPos, Cmd.none )

        UnDrag ->
            ( updateUnDrag model, Cmd.none )


updateStartDrag : Model -> String -> ClientPosition -> Model
updateStartDrag model id clientPos =
    { model | drag = Dragged { id = id, startPos = clientPos, currentPos = clientPos } }


updateKeepDragging : Model -> ClientPosition -> Model
updateKeepDragging model clientPos =
    case model.drag of
        Dragged draggable ->
            let
                svgOuter =
                    model.svgOuter

                updatedDraggable =
                    { draggable | currentPos = clientPos }

                updatedChildren =
                    List.map
                        (\elem ->
                            if elem.id == draggable.id then
                                { elem | transform = calcTransform updatedDraggable svgOuter.matrix }

                            else
                                elem
                        )
                        svgOuter.children
            in
            { drag = Dragged updatedDraggable
            , svgOuter = { svgOuter | children = updatedChildren }
            }

        UnDragged ->
            model


updateReceiveSvgMatrix : Model -> SvgMatrix -> Model
updateReceiveSvgMatrix model svgMatrix =
    let
        svgOuter =
            model.svgOuter
    in
    { model | svgOuter = { svgOuter | matrix = svgMatrix } }


updateUnDrag : Model -> Model
updateUnDrag model =
    { model | drag = UnDragged }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { drag = UnDragged
      , svgOuter =
            { width = 600
            , height = 500
            , viewBox = { minX = 0, minY = 0, width = 30, height = 20 }
            , matrix = { a = 0, b = 0, c = 0, d = 0, e = 0, f = 0 }
            , children =
                [ { id = "big-rect", width = 30, height = 20, transform = ( 0, 0 ), fillColor = "#98e612" }
                , { id = "left-rect", width = 8, height = 10, transform = ( 4, 5 ), fillColor = "#007bff" }
                , { id = "right-rect", width = 8, height = 10, transform = ( 18, 5 ), fillColor = "#888" }
                ]
            }
      }
    , requestSvgMatrix "svg-outer"
    )


type alias Model =
    { drag : DragState
    , svgOuter : SvgOuter
    }


type DragState
    = UnDragged
    | Dragged Draggable


type alias Draggable =
    { id : String
    , startPos : ClientPosition
    , currentPos : ClientPosition
    }


type alias ClientPosition =
    ( Float, Float )


type alias UserPosition =
    ( Float, Float )


type alias SvgTransform =
    ( Float, Float )


calcTransform : Draggable -> SvgMatrix -> SvgTransform
calcTransform draggable svgMatrix =
    let
        userStartPos =
            mult svgMatrix draggable.startPos

        userCurrentPos =
            mult svgMatrix draggable.currentPos
    in
    ( first userCurrentPos - first userStartPos, second userCurrentPos - second userStartPos )


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
