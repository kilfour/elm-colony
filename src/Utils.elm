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
