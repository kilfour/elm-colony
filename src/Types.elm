module Types exposing (..)

-- DESIRES / NEEDS


type Desire
    = Hunger
    | Shelter
    | Socialize


desireToString : Desire -> String
desireToString desire =
    case desire of
        Hunger ->
            "Hunger"

        Shelter ->
            "Shelter"

        Socialize ->
            "Socialize"


type alias Need =
    { desire : Desire
    , urgency : Int
    }



-- SATISFACTION


type alias Satisfaction =
    { desire : Desire
    , amount : Int
    }



-- MOTIVATION


type alias Motivation =
    Desire



-- STUFF


type alias Stuff =
    { name : String
    , satisfies : List Satisfaction
    , durability : Int
    , permanent : Bool
    }


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


type alias Action =
    { name : String
    , positiveMotivations : List Motivation
    , negativeMotivations : List Motivation
    , gives : List Stuff
    , consumes : List (Stuff -> Bool)
    , satisfies : List Satisfaction
    }


type alias Actor =
    { name : String
    , needs : List Need
    , stuff : List Stuff
    , location : Location
    , actions : List Action
    , history : List String
    }



-- LOCATION


type alias Location =
    { name : String
    , terrainFeatures : List TerrainFeature
    , size : Int
    }



-- TERRAIN FEATURE


type alias TerrainFeature =
    { name : String
    , actions : List Action
    , size : Int
    }
