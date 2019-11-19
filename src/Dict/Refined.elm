module Dict.Refined exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A Dict over any keys with a mapping to comparable.


# Data structure

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import Dict


type Dict comparable k v
    = Dict
        { dict : Dict.Dict comparable ( k, v )
        , toKey : k -> comparable
        }


{-| Create an empty dictionary by suppling function used for comparing keys.
-}
empty : (k -> comparable) -> Dict comparable k v
empty toKey =
    Dict
        { dict = Dict.empty
        , toKey = toKey
        }


{-| Create a dictionary with one key-value pair.
-}
singleton : (k -> comparable) -> k -> v -> Dict comparable k v
singleton toKey k v =
    empty toKey
        |> insert k v


{-| Insert a key-value pair into a dict. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict comparable k v -> Dict comparable k v
insert k v (Dict inner) =
    Dict { inner | dict = Dict.insert (inner.toKey k) ( k, v ) inner.dict }


{-| Update the value of a dict for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> Dict comparable k v -> Dict comparable k v
update k f (Dict inner) =
    let
        updateDict =
            Maybe.map (\b -> ( k, b )) << f << Maybe.map Tuple.second
    in
    Dict { inner | dict = Dict.update (inner.toKey k) updateDict inner.dict }


{-| Remove a key-value pair from a dict. If the key is not found,
no changes are made.
-}
remove : k -> Dict comparable k v -> Dict comparable k v
remove k (Dict inner) =
    Dict { inner | dict = Dict.remove (inner.toKey k) inner.dict }


{-| Determine if a dict is empty.

    `isEmpty empty == True`

-}
isEmpty : Dict comparable k v -> Bool
isEmpty (Dict { dict }) =
    Dict.isEmpty dict


{-| Determine if a whole key is in a dict.
-}
member : k -> Dict comparable k v -> Bool
member k (Dict { dict, toKey }) =
    Dict.member (toKey k) dict


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
get : k -> Dict comparable k v -> Maybe v
get k (Dict { dict, toKey }) =
    Dict.get (toKey k) dict
        |> Maybe.map Tuple.second


{-| Determine the number of key-value pairs in the dict.
-}
size : Dict comparable k v -> Int
size (Dict { dict }) =
    Dict.size dict


{-| Get all of the keys in a dict, sorted from lowest to highest.
-}
keys : Dict comparable k v -> List k
keys =
    List.map Tuple.first << toList


{-| Get all of the values in a dict, in the order of their keys.
-}
values : Dict comparable k v -> List v
values =
    List.map Tuple.second << toList


{-| Convert a dict into an association list of key-value pairs, sorted by keys.
-}
toList : Dict comparable k v -> List ( k, v )
toList (Dict { dict }) =
    Dict.values dict


{-| Convert an association list into a dict.
-}
fromList : (k -> comparable) -> List ( k, v ) -> Dict comparable k v
fromList toKey xs =
    Dict
        { toKey = toKey
        , dict = Dict.fromList <| List.map (\( k, v ) -> ( toKey k, ( k, v ) )) xs
        }


{-| Apply a function to all values in a dict.
-}
map : (k -> a -> b) -> Dict comparable k a -> Dict comparable k b
map f (Dict { dict, toKey }) =
    Dict
        { dict = Dict.map (\_ ( k, v ) -> ( k, f k v )) dict
        , toKey = toKey
        }


{-| Fold over the key-value pairs in a dict from lowest key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldl f acc (Dict { dict }) =
    Dict.foldl (\_ ( k, v ) -> f k v) acc dict


{-| Fold over the key-value pairs in a dict from highest key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldr f acc (Dict { dict }) =
    Dict.foldr (\_ ( k, v ) -> f k v) acc dict


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> v -> Bool) -> Dict comparable k v -> Dict comparable k v
filter f (Dict inner) =
    Dict { inner | dict = Dict.filter (\_ ( k, v ) -> f k v) inner.dict }


{-| Partition a dict according to some test. The first dict contains all
key-value pairs which passed the test, and the second contains the pairs that
did not.
-}
partition : (k -> v -> Bool) -> Dict comparable k v -> ( Dict comparable k v, Dict comparable k v )
partition f (Dict inner) =
    let
        ( left, right ) =
            Dict.partition (\_ ( k, v ) -> f k v) inner.dict
    in
    ( Dict { inner | dict = left }
    , Dict { inner | dict = right }
    )


{-| Combine two dicts. If there is a collision, preference is given to the first
dict.
-}
union : Dict comparable k v -> Dict comparable k v -> Dict comparable k v
union (Dict inner) (Dict { dict }) =
    Dict { inner | dict = Dict.union inner.dict dict }


{-| Keep a key-value pair when its key appears in the second dict. Preference is
given to values in the first dictionary.
-}
intersect : Dict comparable k v -> Dict comparable k v -> Dict comparable k v
intersect (Dict inner) (Dict { dict }) =
    Dict { inner | dict = Dict.intersect inner.dict dict }


{-| Keep a key-value pair when its key does not appear in the second dict.
-}
diff : Dict comparable k v -> Dict comparable k v -> Dict comparable k v
diff (Dict inner) (Dict { dict }) =
    Dict { inner | dict = Dict.diff inner.dict dict }


{-| The most general way of combining two dicts. You provide three accumulators
for when a given key appears:

1.  Only in the left dict.
2.  In both dicts.
3.  Only in the right dict.

You then traverse all the keys from lowest to highest, building up whatever you
want.

-}
merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> Dict comparable k a
    -> Dict comparable k b
    -> result
    -> result
merge f g h (Dict inner) (Dict { dict }) =
    let
        l fc _ ( k, v ) =
            fc k v
    in
    Dict.merge (l f) (\_ ( k, a ) ( _, b ) -> g k a b) (l h) inner.dict dict
