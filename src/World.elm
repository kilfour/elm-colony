module World exposing (..)

import Types exposing (..)


eatApple : Action
eatApple =
    { name = "Eat Apple"
    , positiveMotivations = [ Hunger ]
    , negativeMotivations = []
    , gives = []
    , consumes = [ \s -> s.name == "Apple" ]
    , satisfies = []
    }


apple : Stuff
apple =
    { name = "Apple"
    , satisfies = [ { desire = Hunger, amount = 5 } ]
    , durability = 2
    , permanent = False
    }


getApple : Action
getApple =
    { name = "Get Apple"
    , positiveMotivations = [ Hunger ]
    , negativeMotivations = []
    , gives = [ apple ]
    , consumes = []
    , satisfies = []
    }


starGaze : Action
starGaze =
    { name = "Stargaze"
    , positiveMotivations = [ Socialize ]
    , negativeMotivations = []
    , gives = []
    , consumes = []
    , satisfies = [ { desire = Socialize, amount = 3 } ]
    }


buildShelter : Action
buildShelter =
    { name = "Build Shelter"
    , positiveMotivations = [ Shelter ]
    , negativeMotivations = []
    , gives = []
    , consumes = []
    , satisfies = []
    }


clearing : TerrainFeature
clearing =
    { name = "Clearing"
    , actions = [ getApple, buildShelter ]
    , size = 5
    }


forestGlade : Location
forestGlade =
    { id = 1
    , name = "Forest Glade"
    , terrainFeatures = [ clearing ]
    , size = 10
    }


mountainBase : Location
mountainBase =
    { id = 2
    , name = "Mountain Base"
    , terrainFeatures = [ rockyOutcrop ]
    , size = 7
    }


rockyOutcrop : TerrainFeature
rockyOutcrop =
    { name = "Rocky Outcrop"
    , actions = [ starGaze ]
    , size = 3
    }


ava : Actor
ava =
    { name = "Ava"
    , locationId = 1
    , needs =
        [ { desire = Hunger, urgency = 1 }
        , { desire = Shelter, urgency = 2 }
        , { desire = Socialize, urgency = 3 }
        ]
    , stuff = []
    , actions = []
    , history = []
    }
