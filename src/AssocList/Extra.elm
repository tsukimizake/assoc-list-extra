module AssocList.Extra exposing
    ( groupBy, filterGroupBy, fromListBy, fromListDedupe, fromListDedupeBy, frequencies
    , removeWhen, removeMany, keepOnly, insertDedupe, mapKeys, filterMap, invert
    , any, find
    )

{-| Convenience functions for working with [`AssocList.Dict`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest/)

A copy of [`Dict.Extra`](https://package.elm-lang.org/packages/elm-community/dict-extra/latest/)
that works on `AssocList.Dict` instead of the `Dict` implementation from `elm/core`.


# List operations

@docs groupBy, filterGroupBy, fromListBy, fromListDedupe, fromListDedupeBy, frequencies


# Manipulation

@docs removeWhen, removeMany, keepOnly, insertDedupe, mapKeys, filterMap, invert


# Utilities

@docs any, find

-}

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)


{-| Takes a key-fn and a list.
Creates a `Dict` which maps the key to a list of matching elements.

    import AssocList as Dict

    groupBy String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, [ "tree", "leaf" ] ), ( 5, [ "apple" ] ) ]

-}
groupBy : (v -> k) -> List v -> Dict k (List v)
groupBy keyfn list =
    List.foldr
        (\x acc ->
            Dict.update (keyfn x) (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc
        )
        Dict.empty
        list


{-| Takes a key-fn and a list.
Creates a `Dict` which maps the key to a list of matching elements, skipping elements
where key-fn returns `Nothing`

    import AssocList as Dict

    filterGroupBy (String.uncons >> Maybe.map Tuple.first) [ "tree" , "", "tweet", "apple" , "leaf", "" ]
    --> Dict.fromList [ ( 't', [ "tree", "tweet" ] ), ( 'a', [ "apple" ] ), ( 'l', [ "leaf" ] ) ]

    filterGroupBy
        .car
        [ { name = "Mary"
          , car = Just "Ford"
          }
        , { name = "Jack"
          , car = Nothing
          }
        , { name = "Jill"
          , car = Just "Tesla"
          }
        , { name = "John"
          , car = Just "Tesla"
          }
        ]
    --> Dict.fromList
    --> [ ( "Ford"
    -->   , [ { name = "Mary" , car = Just "Ford" } ]
    -->   )
    --> , ( "Tesla"
    -->   , [ { name = "Jill" , car = Just "Tesla" }
    -->     , { name = "John" , car = Just "Tesla" }
    -->     ]
    -->   )
    --> ]

-}
filterGroupBy : (v -> Maybe k) -> List v -> Dict k (List v)
filterGroupBy keyfn list =
    List.foldr
        (\x acc ->
            case keyfn x of
                Just key ->
                    Dict.update key (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc

                Nothing ->
                    acc
        )
        Dict.empty
        list


{-| Create a dictionary from a list of values, by passing a function that can get a key from any such value.
If the function does not return unique keys, earlier values are discarded.

    import AssocList as Dict

    fromListBy String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, "leaf" ), ( 5, "apple" ) ]

-}
fromListBy : (v -> k) -> List v -> Dict k v
fromListBy keyfn xs =
    List.foldl
        (\x acc -> Dict.insert (keyfn x) x acc)
        Dict.empty
        xs


{-| Like `Dict.fromList`, but you provide a way to deal with
duplicate keys. Create a dictionary from a list of pairs of keys and
values, providing a function that is used to combine multiple values
paired with the same key.

    import AssocList as Dict

    fromListDedupe
        (\a b -> a ++ " " ++ b)
        [ ( "class", "menu" ), ( "width", "100%" ), ( "class", "big" ) ]
    --> Dict.fromList [ ( "class", "menu big" ), ( "width", "100%" ) ]

-}
fromListDedupe : (v -> v -> v) -> List ( k, v ) -> Dict k v
fromListDedupe combine xs =
    List.foldl
        (\( key, value ) acc -> insertDedupe combine key value acc)
        Dict.empty
        xs


{-| `fromListBy` and `fromListDedupe` rolled into one.

    import AssocList as Dict

    fromListDedupeBy (\first second -> first) String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, "tree" ), ( 5, "apple" ) ]

-}
fromListDedupeBy : (v -> v -> v) -> (v -> k) -> List v -> Dict k v
fromListDedupeBy combine keyfn xs =
    List.foldl
        (\x acc -> insertDedupe combine (keyfn x) x acc)
        Dict.empty
        xs


{-| Count the number of occurences for each of the elements in the list.

    import AssocList as Dict

    frequencies [ "A", "B", "C", "B", "C", "B" ]
    --> Dict.fromList [ ( "A", 1 ), ( "B", 3 ), ( "C", 2 ) ]

-}
frequencies : List k -> Dict k Int
frequencies list =
    list
        |> List.foldl
            (\el counter ->
                Dict.get el counter
                    |> Maybe.withDefault 0
                    |> (\count -> count + 1)
                    |> (\count -> Dict.insert el count counter)
            )
            Dict.empty


{-| Remove elements which satisfies the predicate.

    import AssocList as Dict

    Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ]
        |> removeWhen (\_ value -> value == 1 )
    --> Dict.fromList [ ( "Jack", 2 ) ]

-}
removeWhen : (k -> v -> Bool) -> Dict k v -> Dict k v
removeWhen pred dict =
    Dict.filter (\k v -> not (pred k v)) dict


{-| Remove a key-value pair if its key appears in the set.

    import AssocList as Dict
    import AssocSet as Set

    Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ]
        |> removeMany (Set.fromList [ "Mary", "Jill" ])
    --> Dict.fromList [ ( "Jack", 2 ) ]

-}
removeMany : Set k -> Dict k v -> Dict k v
removeMany set dict =
    Set.foldl Dict.remove dict set


{-| Insert an element at the given key, providing a combining
function that used in the case that there is already an
element at that key. The combining function is called with
original element and the new element as arguments and
returns the element to be inserted.

    import AssocList as Dict

    Dict.fromList [ ( "expenses", 38.25 ), ( "assets", 100.85 ) ]
        |> insertDedupe (+) "expenses" 2.50
        |> insertDedupe (+) "liabilities" -2.50
    --> Dict.fromList [ ( "expenses", 40.75 ), ( "assets", 100.85 ), ( "liabilities", -2.50 ) ]

-}
insertDedupe : (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertDedupe combine key value dict =
    let
        with mbValue =
            case mbValue of
                Just oldValue ->
                    Just <| combine oldValue value

                Nothing ->
                    Just value
    in
    Dict.update key with dict


{-| Keep a key-value pair if its key appears in the set.

    import AssocList as Dict
    import AssocSet as Set

    Dict.fromList [ ( "Mary", 1 ), ( "Jack", 2 ), ( "Jill", 1 ) ]
        |> keepOnly (Set.fromList [ "Jack", "Jill" ])
    --> Dict.fromList [ ( "Jack", 2 ), ( "Jill", 1 ) ]

-}
keepOnly : Set k -> Dict k v -> Dict k v
keepOnly set dict =
    Set.foldl
        (\k acc ->
            Maybe.withDefault acc <| Maybe.map (\v -> Dict.insert k v acc) (Dict.get k dict)
        )
        Dict.empty
        set


{-| Apply a function to all keys in a dictionary.

    import AssocList as Dict

    Dict.fromList [ ( 5, "Jack" ), ( 10, "Jill" ) ]
        |> mapKeys (\x -> x + 1)
    --> Dict.fromList [ ( 6, "Jack" ), ( 11, "Jill" ) ]

    Dict.fromList [ ( 5, "Jack" ), ( 10, "Jill" ) ]
        |> mapKeys String.fromInt
    --> Dict.fromList [ ( "5", "Jack" ), ( "10", "Jill" ) ]

-}
mapKeys : (k1 -> k2) -> Dict k1 v -> Dict k2 v
mapKeys keyMapper dict =
    Dict.foldl
        (\k v acc ->
            Dict.insert (keyMapper k) v acc
        )
        Dict.empty
        dict


{-| Apply a function that may or may not succeed to all entries in a dictionary,
but only keep the successes.

    import AssocList as Dict

    let
        isTeen n a =
            if 13 <= n && n <= 19 then
                Just <| String.toUpper a
            else
                Nothing
    in
    Dict.fromList [ ( 5, "Jack" ), ( 15, "Jill" ), ( 20, "Jones" ) ]
        |> filterMap isTeen
    --> Dict.fromList [ ( 15, "JILL" ) ]

-}
filterMap : (k -> v1 -> Maybe v2) -> Dict k v1 -> Dict k v2
filterMap f dict =
    Dict.foldl
        (\k v acc ->
            case f k v of
                Just newVal ->
                    Dict.insert k newVal acc

                Nothing ->
                    acc
        )
        Dict.empty
        dict


{-| Inverts the keys and values of an array.

    import AssocList as Dict

    Dict.fromList [ ("key", "value")  ]
        |> invert
    --> Dict.fromList [ ( "value", "key" ) ]

-}
invert : Dict a b -> Dict b a
invert dict =
    Dict.foldl
        (\k v acc ->
            Dict.insert v k acc
        )
        Dict.empty
        dict


{-| Determine if any key/value pair satisfies some test.

    import AssocList as Dict

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> any (\_ value -> value == "Jill")
    --> True

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> any (\key _ -> key == 5)
    --> False

-}
any : (k -> v -> Bool) -> Dict k v -> Bool
any predicate dict =
    Dict.foldl
        (\k v acc ->
            if acc then
                acc

            else if predicate k v then
                True

            else
                False
        )
        False
        dict


{-| Find the first key/value pair that matches a predicate.

    import AssocList as Dict

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> find (\_ value -> value == "Jill")
    --> Just ( 7, "Jill" )

    Dict.fromList [ ( 9, "Jill" ), ( 7, "Jill" ) ]
        |> find (\key _ -> key == 5)
    --> Nothing

-}
find : (k -> v -> Bool) -> Dict k v -> Maybe ( k, v )
find predicate dict =
    Dict.foldl
        (\k v acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if predicate k v then
                        Just ( k, v )

                    else
                        Nothing
        )
        Nothing
        dict
