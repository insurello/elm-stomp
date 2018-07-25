module Stomp.Internal.Batch
    exposing
        ( Batch
        , none
        , singleton
        , map
        , batch
        , foldl
        , cmd
        )


type Batch a
    = Items (List a)


none : Batch a
none =
    Items []


singleton : a -> Batch a
singleton item =
    Items [ item ]


map : (a -> b) -> Batch a -> Batch b
map func batch =
    case batch of
        Items items ->
            Items (List.map func items)


batch : List (Batch a) -> Batch a
batch items =
    items
        |> List.map (\(Items items) -> items)
        |> List.concat
        |> Items


foldl : (a -> b -> b) -> b -> Batch a -> b
foldl func acc batch =
    case batch of
        Items items ->
            List.foldl func acc items


cmd : (a -> Cmd b) -> Batch a -> Cmd b
cmd func batch =
    Cmd.batch <|
        foldl (\a acc -> acc ++ [ func a ]) [] batch
