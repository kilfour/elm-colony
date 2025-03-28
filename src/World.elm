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
    , actions = [ getApple, starGaze, buildShelter ]
    , size = 5
    }


forestGlade : Location
forestGlade =
    { name = "Forest Glade"
    , terrainFeatures = [ clearing ]
    , size = 10
    }


ava : Actor
ava =
    { name = "Ava"
    , needs = [ { desire = Hunger, urgency = 1 }, { desire = Shelter, urgency = 2 }, { desire = Socialize, urgency = 3 } ]
    , stuff = []
    , location = forestGlade
    , actions = []
    , history = []
    }
