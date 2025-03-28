module Main exposing (main, viewNeed)

import Browser
import Functions exposing (..)
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import Utils exposing (..)
import World exposing (..)


stepActor : List Location -> Actor -> Actor
stepActor locations actor =
    let
        decayed =
            decayAndUpdateNeeds actor

        possible =
            availableActions locations decayed
    in
    case maximumBy (actionScore decayed.needs) possible of
        Just best ->
            performAction best decayed

        Nothing ->
            decayed


type alias Model =
    { actors : List Actor, locations : List Location }


initialModel : Model
initialModel =
    { actors = [ ava ], locations = [ forestGlade, mountainBase ] }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | actors = List.map (stepActor model.locations) model.actors }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        (button [ onClick Tick ] [ text "Simulate Step" ]
            :: List.map (\a -> viewActor model a) model.actors
        )


viewActor : Model -> Actor -> Html Msg
viewActor model actor =
    div [ class "actor-container" ]
        [ div [ class "section-column" ]
            [ Html.h2 [] [ text ("Actor: " ++ actor.name) ]
            , Html.h3 [] [ text "At Location" ]
            , viewLocation <| getActorLocation actor model.locations
            , Html.h3 [] [ text "Needs" ]
            , ul [] (List.map viewNeed actor.needs)
            , Html.h3 [] [ text "Stuff" ]
            , ul [] (List.map viewStuff actor.stuff)
            ]
        , div [ class "section-column" ]
            [ Html.h3 [] [ text "Decision" ]
            , viewDecisionMaking model actor
            , Html.h3 [] [ text "Action History" ]
            , ul [] (List.map (\entry -> li [] [ text entry ]) (List.reverse actor.history))
            ]
        , div [ class "section-column" ]
            [ Html.h3 [] [ text "All Locations" ]
            , ul [] (List.map viewLocation model.locations)
            ]
        ]


viewDecisionMaking : Model -> Actor -> Html msg
viewDecisionMaking model actor =
    let
        possibleActions =
            availableActions model.locations actor

        bestAction =
            maximumBy (actionScore actor.needs) possibleActions
    in
    case bestAction of
        Just action ->
            div []
                [ Html.p [] [ text ("Wants to: " ++ action.name) ]
                , Html.p [] [ text ("Score: " ++ String.fromInt (actionScore actor.needs action)) ]

                --, Html.p [] [ text ("At Location: " ++ location.name ++ " — ") ]
                , Html.p [] [ text "Motivations:" ]
                , ul []
                    (List.map
                        (\m ->
                            let
                                urgency =
                                    case List.filter (\n -> n.desire == m) actor.needs of
                                        n :: _ ->
                                            n.urgency

                                        [] ->
                                            0
                            in
                            li [] [ text (desireToString m ++ " (urgency: " ++ String.fromInt urgency ++ ")") ]
                        )
                        action.positiveMotivations
                    )
                , Html.h4 [] [ text "All Options:" ]
                , ul []
                    (List.map
                        (\a ->
                            let
                                s =
                                    actionScore actor.needs a
                            in
                            li [] [ text (a.name ++ " — Score: " ++ String.fromInt s) ]
                        )
                        possibleActions
                    )
                ]

        Nothing ->
            Html.p [] [ text "No useful action to take." ]


viewNeed : Need -> Html msg
viewNeed n =
    li [] [ text (desireToString n.desire ++ " — Urgency: " ++ String.fromInt n.urgency) ]


viewStuff : Stuff -> Html msg
viewStuff s =
    let
        satString =
            s.satisfies
                |> List.map (\sat -> desireToString sat.desire ++ "(" ++ String.fromInt sat.amount ++ ")")
                |> String.join ", "

        dur =
            if s.permanent then
                " ♾️ Permanent"

            else
                " — Durability: " ++ String.fromInt s.durability
    in
    li [] [ text (s.name ++ " (Satisfies: " ++ satString ++ ")" ++ dur) ]


viewLocation : Location -> Html msg
viewLocation loc =
    div []
        [ Html.h4 [] [ text ("📍 " ++ loc.name ++ " (Size: " ++ String.fromInt loc.size ++ ")") ]
        , Html.ul [] (List.map viewTerrainFeature loc.terrainFeatures)
        ]


viewTerrainFeature : TerrainFeature -> Html msg
viewTerrainFeature tf =
    li []
        [ text ("🧭 " ++ tf.name ++ " ( Size: " ++ String.fromInt tf.size ++ ")")
        , Html.ul [] (List.map (\a -> li [] [ text a.name ]) tf.actions)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view =
            \a ->
                { title = "Colony"
                , body = [ view a ]
                }
        }
