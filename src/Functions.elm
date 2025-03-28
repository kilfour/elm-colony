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
        fulfillNeed : Satisfaction -> List Need
        fulfillNeed satisfaction =
            actor.needs
                |> List.map
                    (\a ->
                        if a.desire == satisfaction.desire then
                            { a | urgency = a.urgency - satisfaction.amount }

                        else
                            a
                    )
    in
    { actor | needs = action.satisfies |> List.map fulfillNeed |> List.concat }


performAction : Action -> Actor -> Actor
performAction action actor =
    let
        withoutConsumed =
            { actor | stuff = consumeStuff action.consumes actor.stuff }

        updatedActor =
            giveStuffToActor action.gives withoutConsumed
                |> fulfillNeeds action
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


availableActions : Actor -> List Action
availableActions actor =
    actor.actions ++ List.concatMap .actions actor.location.terrainFeatures


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
