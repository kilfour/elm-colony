module Types exposing (..)


type Desire
    = Hunger
    | Shelter
    | Socialize


type alias Need =
    { desire : Desire
    , urgency : Int
    }


type alias Satisfaction =
    { desire : Desire
    , amount : Int
    }


type alias Motivation =
    { desire : Desire
    , strength : Int
    }


type alias Stuff =
    { name : String
    , durability : Int
    , permanent : Bool
    , actions : List Action
    }


type ActionType
    = NoOp
    | Obtain Stuff
    | Consume String
    | Build Terrain


type alias Action =
    { name : String
    , actionType : ActionType
    , positiveMotivations : List Motivation
    , negativeMotivations : List Motivation
    , satisfies : List Satisfaction
    }


type alias Actor =
    { name : String
    , locationId : Int
    , needs : List Need
    , stuff : List Stuff
    , history : List String
    }


type alias Location =
    { id : Int
    , name : String
    , terrain : List Terrain
    }


type alias Terrain =
    { name : String
    , actions : List Action
    }
