module World exposing (..)

import Types exposing (..)


getApple : Action
getApple =
    { name = "Get Apple"
    , actionType = Obtain apple
    , positiveMotivations = [ { desire = Hunger, strength = 1 } ]
    , negativeMotivations = []
    , satisfies = []
    }


apple : Stuff
apple =
    { name = "Apple"
    , durability = 2
    , permanent = False
    , actions = [ eatApple ]
    }


eatApple : Action
eatApple =
    { name = "Eat Apple"
    , actionType = Consume "Apple"
    , positiveMotivations = [ { desire = Hunger, strength = 10 } ]
    , negativeMotivations = []
    , satisfies = [ { desire = Hunger, amount = 5 } ]
    }


starGaze : Action
starGaze =
    { name = "Stargaze"
    , actionType = NoOp
    , positiveMotivations = [ { desire = Socialize, strength = 1 } ]
    , negativeMotivations = []
    , satisfies = [ { desire = Socialize, amount = 3 } ]
    }


buildShelter : Action
buildShelter =
    { name = "Build Shelter"
    , actionType = Build shelter
    , positiveMotivations = [ { desire = Shelter, strength = 1 } ]
    , negativeMotivations = []
    , satisfies = []
    }


shelter : Terrain
shelter =
    { name = "Shelter"
    , actions = [ sleepInShelter ]
    }


sleepInShelter : Action
sleepInShelter =
    { name = "Sleep in Shelter"
    , actionType = NoOp
    , positiveMotivations = [ { desire = Shelter, strength = 5 } ]
    , negativeMotivations = []
    , satisfies = [ { desire = Shelter, amount = 5 } ]
    }


clearing : Terrain
clearing =
    { name = "Clearing"
    , actions = [ getApple, buildShelter ]
    }


forestGlade : Location
forestGlade =
    { id = 1
    , name = "Forest Glade"
    , terrain = [ clearing ]
    }


mountainBase : Location
mountainBase =
    { id = 2
    , name = "Mountain Base"
    , terrain = [ rockyOutcrop ]
    }


rockyOutcrop : Terrain
rockyOutcrop =
    { name = "Rocky Outcrop"
    , actions = [ starGaze ]
    }


ava : Actor
ava =
    { name = "Ava"
    , locationId = 1
    , needs =
        [ { desire = Hunger, urgency = 2 }
        , { desire = Shelter, urgency = 1 }
        , { desire = Socialize, urgency = 3 }
        ]
    , stuff = []
    , history = []
    }
