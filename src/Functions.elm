module Functions exposing (..)

import Types exposing (..)


isDesireFulfilled : Desire -> List Stuff -> Bool
isDesireFulfilled desire stuffList =
    List.any (\s -> List.any (\sat -> sat.desire == desire) s.satisfies) stuffList


giveStuffToActor : List Stuff -> Actor -> Actor
giveStuffToActor newStuff actor =
    { actor | stuff = actor.stuff ++ newStuff }


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


performAction : Action -> Actor -> Actor
performAction action actor =
    let
        withoutConsumed =
            { actor | stuff = consumeStuff action.consumes actor.stuff }

        -- updatedActor =
        --     giveStuffToActor action.gives withoutConsumed
        --         |> fulfillNeeds action
        updatedActor =
            fulfillNeeds action actor
    in
    { updatedActor | history = action.name :: actor.history }


actionScore : List Need -> Action -> Int
actionScore needs action =
    let
        scoreMotivation m =
            case List.filter (\n -> n.desire == m) needs of
                n :: _ ->
                    n.urgency

                [] ->
                    0
    in
    List.sum (List.map scoreMotivation action.positiveMotivations)
        - List.sum (List.map scoreMotivation action.negativeMotivations)


availableActions : List Location -> Actor -> List Action
availableActions locations actor =
    actor.actions ++ List.concatMap .actions (getLocationById actor.locationId locations).terrainFeatures


decayAndUpdateNeeds : Actor -> Actor
decayAndUpdateNeeds actor =
    let
        decayedStuff =
            List.filterMap decayStuff actor.stuff

        updatedNeeds =
            List.map
                (\need ->
                    if isDesireFulfilled need.desire decayedStuff then
                        need

                    else
                        { need | urgency = need.urgency + 1 }
                )
                actor.needs
    in
    { actor | stuff = decayedStuff, needs = updatedNeeds }



-- FIND LOCATION BY ID


getLocationById : Int -> List Location -> Location
getLocationById id locations =
    List.head (List.filter (\l -> l.id == id) locations)
        |> Maybe.withDefault (Location id "Unknown" [] 0)


getActorLocation : Actor -> List Location -> Location
getActorLocation actor locations =
    getLocationById actor.locationId locations
