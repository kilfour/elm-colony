module Utils exposing (..)


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just
                (List.foldl
                    (\item currentMax ->
                        if f item > f currentMax then
                            item

                        else
                            currentMax
                    )
                    x
                    xs
                )


replaceById : (a -> Int) -> Int -> a -> List a -> List a
replaceById getId targetId newValue list =
    List.map
        (\item ->
            if getId item == targetId then
                newValue

            else
                item
        )
        list
