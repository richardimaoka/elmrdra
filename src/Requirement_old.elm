module Requirement exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, r, style, transform, viewBox)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


view : Model -> Html Msg
view model =
    div [ class "max-w-screen-lg mx-auto bg-gray-100" ]
        [ h1 [ class "text-4xl" ] [ text "RDRA tool" ]
        , div [ class "grid grid-cols-1 row-gap-8" ]
            (List.append
                (List.map
                    viewRequirementModel
                    model.requirementModels
                )
                [ viewExtraButton ]
            )
        ]


viewRequirementModel : RequirementModel -> Html Msg
viewRequirementModel requirementModel =
    case requirementModel of
        RequirementModel record ->
            div [ class "grid grid-cols-12 grid-rows-1" ]
                [ viewActor record.actor
                , viewRequirementList record.requirements
                ]


viewActor : Actor -> Html Msg
viewActor actor =
    case actor of
        Actor record ->
            div [ class "col-start-1 col-end-5 p-4 bg-blue-300" ]
                [ div [] [ viewActorSvg, viewActorName record.name ] ]


viewActorSvg : Html Msg
viewActorSvg =
    div [ class "w-16 h-16 mt-4 mb-2 mx-auto h-auto" ]
        [ svg [ viewBox "0 0 213.83 233.11" ]
            [ path
                [ d "M408,274.51c-15.73,19.92-74.31,29.8-99.69,29.8s-84.54-9.88-100.27-29.8c-10-12.63,0-64.38,0-64.38a28.43,28.43,0,0,1,28.43-28.44h143.1A28.43,28.43,0,0,1,408,210.13S418,261.88,408,274.51Z"
                , transform "translate(-201.08 -73.69)"
                , style "fill:#201f21;stroke:#fff;stroke-miterlimit:10;stroke-width:5px"
                ]
                []
            , circle
                [ cx "106.92"
                , cy "63.81"
                , r "61.31"
                , style "fill:#201f21;stroke:#fff;stroke-miterlimit:10;stroke-width:5px"
                ]
                []
            ]
        ]


viewActorName : String -> Html Msg
viewActorName name =
    div [ class "w-auto h-auto mt-2 mb-4 mx-4 p-1 text-center" ]
        [ text name ]


viewRequirementList : List Requirement -> Html Msg
viewRequirementList requirements =
    div [ class "col-start-5 col-end-13 bg-blue-300" ]
        (List.append
            (List.map
                viewRequirement
                requirements
            )
            [ buttonAddRequirement ]
        )


viewRequirement : Requirement -> Html Msg
viewRequirement requirement =
    case requirement of
        Requirement str ->
            div [ class "p-2 m-4 h-auto bg-gray-100" ]
                [ text str ]


buttonAddRequirement : Html Msg
buttonAddRequirement =
    button [ class "block p-2 mx-4 mt-8 mb-4 h-auto bg-blue-800 text-gray-100" ] [ text "要求を追加" ]


viewExtraButton : Html Msg
viewExtraButton =
    div [ class "bg-blue-300" ]
        [ button [ class "block p-2 mx-4 mt-8 mb-4 h-auto bg-blue-800 text-gray-100" ]
            [ text "アクターと要求を追加" ]
        ]


update : Msg -> Model -> Model
update _ _ =
    init


init : Model
init =
    { requirementModels =
        [ RequirementModel
            { actor = Actor { id = "executive", name = "経営者" }
            , requirements =
                [ Requirement "全体の進捗を適宜報告して欲しい"
                ]
            }
        , RequirementModel
            { actor = Actor { id = "manager", name = "現場マネジメント" }
            , requirements =
                [ Requirement "全体の進捗をひと目で知りたい"
                , Requirement "素早く進捗を追いたい"
                , Requirement "他のステークホルダーへの報告を手早く済ませたい"
                , Requirement "不整合・衝突を検出したい"
                , Requirement "コミュニケーションを促進したい"
                ]
            }
        , RequirementModel
            { actor = Actor { id = "creator", name = "制作担当者" }
            , requirements =
                [ Requirement "自分の割当作業の位置づけを知りたい"
                , Requirement "他者とのコミュニケーションをスムーズにしたい"
                , Requirement "調査結果を全体計画にフィードバックしたい"
                , Requirement "素早く整合性チェックしたい"
                ]
            }
        ]
    , unmappedRequirements = []
    , unmappedActors = []
    }


type alias Model =
    { requirementModels : List RequirementModel
    , unmappedRequirements : List Requirement
    , unmappedActors : List Actor
    }


type Requirement
    = Requirement String


type Actor
    = Actor { id : String, name : String }


type RequirementModel
    = RequirementModel { actor : Actor, requirements : List Requirement }


type ExternalSystem
    = ExternalSystem { name : String }


type alias Msg =
    Int
