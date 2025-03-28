module Main exposing (main, viewNeed)

import Browser
import Functions exposing (..)
import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import Utils exposing (..)
import World exposing (..)


stepActor : List Location -> Actor -> ( Actor, List Location )
stepActor locations actor =
    let
        possible =
            availableActions locations actor

        ( updatedActor, updatedLocations ) =
            case maximumBy (actionScore actor.needs) possible of
                Just best ->
                    performAction best locations actor

                Nothing ->
                    ( actor, locations )
    in
    ( decayAndUpdateNeeds updatedActor, updatedLocations )


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
            let
                ( updatedActors, finalLocations ) =
                    List.foldl
                        (\actor ( actorsAcc, locs ) ->
                            let
                                ( updatedActor, updatedLocs ) =
                                    stepActor locs actor
                            in
                            ( updatedActor :: actorsAcc, updatedLocs )
                        )
                        ( [], model.locations )
                        model.actors
            in
            ( { model
                | actors = List.reverse updatedActors
                , locations = finalLocations
              }
            , Cmd.none
            )


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
            , ul [] (List.map (\entry -> li [] [ text entry ]) actor.history)
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
                , Html.p [] [ text "Motivations:" ]
                , ul []
                    (List.map
                        (\m ->
                            let
                                urgency =
                                    case List.filter (\n -> n.desire == m.desire) actor.needs of
                                        n :: _ ->
                                            n.urgency

                                        [] ->
                                            0
                            in
                            li [] [ text (desireToString m.desire ++ " (urgency: " ++ String.fromInt urgency ++ ")") ]
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
        dur =
            if s.permanent then
                " ♾️ Permanent"

            else
                " — Durability: " ++ String.fromInt s.durability
    in
    -- li [] [ text (s.name ++ " (Satisfies: " ++ satString ++ ")" ++ dur) ]
    li [] [ text (s.name ++ " (Satisfies: )" ++ dur) ]


viewLocation : Location -> Html msg
viewLocation loc =
    div []
        [ Html.h4 [] [ text ("📍 " ++ loc.name) ]
        , Html.ul [] (List.map viewTerrainFeature loc.terrain)
        ]


viewTerrainFeature : Terrain -> Html msg
viewTerrainFeature terrain =
    li []
        [ text ("🧭 " ++ terrain.name)
        , Html.ul [] (List.map (\a -> li [] [ text a.name ]) terrain.actions)
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
