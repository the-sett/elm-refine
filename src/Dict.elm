module Dict exposing
    ( empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A Dict over refinement type keys.


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

import Dict exposing (Dict)


type alias Dict k v =
    Dict.Dict k v


{-| Create a trie with one key-value pair.
-}
singleton : String -> a -> Dict a
singleton key val =
    Dict.singleton (String.toList key) val


{-| Insert a key-value pair into a trie. Replaces value when there is
a collision.
-}
insert : String -> a -> Dict a -> Dict a
insert key val trie =
    Dict.insert (String.toList key) val trie


{-| Update the value of a trie for a specific key with a given function.
-}
update : String -> (Maybe a -> Maybe a) -> Dict a -> Dict a
update key fn trie =
    Dict.update (String.toList key) fn trie


{-| Remove a key-value pair from a trie. If the key is not found,
no changes are made.
-}
remove : String -> Dict a -> Dict a
remove key trie =
    Dict.remove (String.toList key) trie


{-| Determine if a trie is empty.

    `isEmpty empty == True`

-}
isEmpty : Dict a -> Bool
isEmpty trie =
    Dict.isEmpty trie


{-| Determine if a whole key is in a trie.
-}
member : String -> Dict a -> Bool
member key trie =
    Dict.member (String.toList key) trie


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
get : String -> Dict a -> Maybe a
get key trie =
    Dict.get (String.toList key) trie


{-| Determine the number of key-value pairs in the trie.
-}
size : Dict a -> Int
size trie =
    Dict.size trie


{-| Get all of the keys in a trie, sorted from lowest to highest.
-}
keys : Dict a -> List String
keys trie =
    Dict.keys trie
        |> List.map String.fromList


{-| Get all of the values in a trie, in the order of their keys.
-}
values : Dict a -> List a
values trie =
    Dict.values trie


{-| Convert a trie into an association list of key-value pairs, sorted by keys.
-}
toList : Dict a -> List ( String, a )
toList trie =
    Dict.toList trie
        |> List.map (Tuple.mapFirst String.fromList)


{-| Convert an association list into a trie.
-}
fromList : List ( String, a ) -> Dict a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


{-| Apply a function to all values in a trie.
-}
map : (String -> a -> b) -> Dict a -> Dict b
map fn trie =
    Dict.map (\chars -> fn <| String.fromList chars) trie


{-| Fold over the key-value pairs in a trie from lowest key to highest key.
-}
foldl : (String -> a -> b -> b) -> b -> Dict a -> b
foldl fn accum trie =
    Dict.foldl (\chars -> fn <| String.fromList chars) accum trie


{-| Fold over the key-value pairs in a trie from highest key to lowest key.

Due to the way shorter keys are nearer the top of the trie this fold function
has to hold more pending nodes in memory in order to fold in order from the
highest key to the lowest key. For this reason it is less efficient than `foldl`
and `foldl` should be preferred unless the ordering is important.

-}
foldr : (String -> a -> b -> b) -> b -> Dict a -> b
foldr fn accum trie =
    Dict.foldr (\chars -> fn <| String.fromList chars) accum trie


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (String -> a -> Bool) -> Dict a -> Dict a
filter isGood trie =
    Dict.filter (\chars -> isGood <| String.fromList chars) trie


{-| Partition a trie according to some test. The first trie contains all
key-value pairs which passed the test, and the second contains the pairs that
did not.
-}
partition : (String -> a -> Bool) -> Dict a -> ( Dict a, Dict a )
partition isGood trie =
    Dict.partition (\chars -> isGood <| String.fromList chars) trie


{-| Combine two tries. If there is a collision, preference is given to the first
trie.
-}
union : Dict a -> Dict a -> Dict a
union t1 t2 =
    Dict.union t1 t2


{-| Keep a key-value pair when its key appears in the second trie. Preference is
given to values in the first dictionary.
-}
intersect : Dict a -> Dict a -> Dict a
intersect t1 t2 =
    Dict.intersect t1 t2


{-| Keep a key-value pair when its key does not appear in the second trie.
-}
diff : Dict a -> Dict b -> Dict a
diff t1 t2 =
    Dict.diff t1 t2


{-| The most general way of combining two tries. You provide three accumulators
for when a given key appears:

1.  Only in the left trie.
2.  In both tries.
3.  Only in the right trie.

You then traverse all the keys from lowest to highest, building up whatever you
want.

-}
merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Dict a
    -> Dict b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    Dict.merge
        (\chars -> leftStep <| String.fromList chars)
        (\chars -> bothStep <| String.fromList chars)
        (\chars -> rightStep <| String.fromList chars)
        leftDict
        rightDict
        initialResult
