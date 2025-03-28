module Functions exposing (..)

import Types exposing (..)
import Utils exposing (..)


desireToString : Desire -> String
desireToString desire =
    case desire of
        Hunger ->
            "Hunger"

        Shelter ->
            "Shelter"

        Socialize ->
            "Socialize"


giveStuffToActor : List Stuff -> Actor -> Actor
giveStuffToActor newStuff actor =
    { actor | stuff = actor.stuff ++ newStuff }


decayStuff : Stuff -> Maybe Stuff
decayStuff s =
    if s.permanent then
        Just s

    else if s.durability <= 1 then
        Nothing

    else
        Just { s | durability = s.durability - 1 }


consumeStuff : List (Stuff -> Bool) -> List Stuff -> List Stuff
consumeStuff matchers stuffList =
    let
        step matcher ( remaining, consumed ) =
            case List.partition matcher remaining of
                ( toConsume :: rest, others ) ->
                    ( others ++ rest, toConsume :: consumed )

                ( [], _ ) ->
                    ( remaining, consumed )

        ( result, _ ) =
            List.foldl step ( stuffList, [] ) matchers
    in
    result


fulfillNeeds : Action -> Actor -> Actor
fulfillNeeds action actor =
    let
        applySatisfaction : Need -> Need
        applySatisfaction need =
            case List.filter (\s -> s.desire == need.desire) action.satisfies of
                [] ->
                    need

                sats ->
                    let
                        totalSatisfaction =
                            List.sum (List.map .amount sats)

                        newUrgency =
                            max 0 (need.urgency - totalSatisfaction)
                    in
                    { need | urgency = newUrgency }
    in
    { actor | needs = List.map applySatisfaction actor.needs }


performAction : Action -> List Location -> Actor -> ( Actor, List Location )
performAction action locations actor =
    let
        ( actorAfterAction, updatedLocations ) =
            case action.actionType of
                NoOp ->
                    ( actor, locations )

                Consume name ->
                    let
                        stuff =
                            actor.stuff
                                |> List.filter (\a -> a.name /= name)
                    in
                    ( { actor | stuff = stuff }, locations )

                Obtain stuff ->
                    ( { actor | stuff = actor.stuff ++ [ stuff ] }, locations )

                Build feature ->
                    let
                        location =
                            getActorLocation actor locations

                        updatedLocation =
                            { location | terrain = location.terrain ++ [ feature ] }
                    in
                    ( actor, replaceById .id location.id updatedLocation locations )

        actorHistoryLogged =
            { actorAfterAction | history = action.name :: actorAfterAction.history }

        updatedActor =
            fulfillNeeds action actorHistoryLogged
    in
    ( updatedActor, updatedLocations )


actionScore : List Need -> Action -> Int
actionScore needs action =
    let
        scoreMotivation m =
            case List.filter (\n -> n.desire == m.desire) needs of
                n :: _ ->
                    n.urgency * m.strength

                [] ->
                    0
    in
    List.sum (List.map scoreMotivation action.positiveMotivations)
        - List.sum (List.map scoreMotivation action.negativeMotivations)


availableActions : List Location -> Actor -> List Action
availableActions locations actor =
    let
        actorLocation =
            getLocationById actor.locationId locations

        locationActions =
            List.concatMap .actions actorLocation.terrain

        stuffActions =
            List.concatMap .actions actor.stuff
    in
    locationActions ++ stuffActions


bestAction : Actor -> List Location -> Maybe Action
bestAction actor locations =
    maximumBy (actionScore actor.needs) (availableActions locations actor)


decayAndUpdateNeeds : Actor -> Actor
decayAndUpdateNeeds actor =
    -- let
    --     decayedStuff =
    --         List.filterMap decayStuff actor.stuff
    --     updatedNeeds =
    --         List.map
    --             (\need ->
    --                 if isDesireFulfilled need.desire decayedStuff then
    --                     need
    --                 else
    --                     { need | urgency = need.urgency + 1 }
    --             )
    --             actor.needs
    -- in
    -- { actor | stuff = decayedStuff, needs = updatedNeeds }
    let
        updatedNeeds =
            List.map
                (\need ->
                    { need | urgency = need.urgency + 1 }
                )
                actor.needs
    in
    { actor | needs = updatedNeeds }



-- FIND LOCATION BY ID


getLocationById : Int -> List Location -> Location
getLocationById id locations =
    List.head (List.filter (\l -> l.id == id) locations)
        |> Maybe.withDefault (Location id "Unknown" [])


getActorLocation : Actor -> List Location -> Location
getActorLocation actor locations =
    getLocationById actor.locationId locations
