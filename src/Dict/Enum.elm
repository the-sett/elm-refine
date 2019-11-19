module Dict.Enum exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A Dict over any keys with a mapping to Strings.


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

import Dict.Refined


{-| Dictionary over any keys with a function to map those keys to strings.
-}
type alias Dict k v =
    Dict.Refined.Dict String k v


{-| Create an empty dictionary by suppling function used for comparing keys.
-}
empty : (k -> String) -> Dict k v
empty toKey =
    Dict.Refined.empty toKey


{-| Create a dictionary with one key-value pair.
-}
singleton : (k -> String) -> k -> v -> Dict k v
singleton toKey k v =
    Dict.Refined.singleton toKey k v


{-| Insert a key-value pair into a dict. Replaces value when there is
a collision.
-}
insert : k -> v -> Dict k v -> Dict k v
insert k v dict =
    Dict.Refined.insert k v dict


{-| Update the value of a dict for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update k f dict =
    Dict.Refined.update k f dict


{-| Remove a key-value pair from a dict. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove k dict =
    Dict.Refined.remove k dict


{-| Determine if a dict is empty.

    `isEmpty empty == True`

-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    Dict.Refined.isEmpty dict


{-| Determine if a whole key is in a dict.
-}
member : k -> Dict k v -> Bool
member k dict =
    Dict.Refined.member k dict


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
get : k -> Dict k v -> Maybe v
get k dict =
    Dict.Refined.get k dict


{-| Determine the number of key-value pairs in the dict.
-}
size : Dict k v -> Int
size dict =
    Dict.Refined.size dict


{-| Get all of the keys in a dict, sorted from lowest to highest.
-}
keys : Dict k v -> List k
keys dict =
    Dict.Refined.keys dict


{-| Get all of the values in a dict, in the order of their keys.
-}
values : Dict k v -> List v
values dict =
    Dict.Refined.values dict


{-| Convert a dict into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    Dict.Refined.toList dict


{-| Convert an association list into a dict.
-}
fromList : (k -> String) -> List ( k, v ) -> Dict k v
fromList toKey xs =
    Dict.Refined.fromList toKey xs


{-| Apply a function to all values in a dict.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map f dict =
    Dict.Refined.map f dict


{-| Fold over the key-value pairs in a dict from lowest key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl f acc dict =
    Dict.Refined.foldl f acc dict


{-| Fold over the key-value pairs in a dict from highest key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr f acc dict =
    Dict.Refined.foldr f acc dict


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter f dict =
    Dict.Refined.filter f dict


{-| Partition a dict according to some test. The first dict contains all
key-value pairs which passed the test, and the second contains the pairs that
did not.
-}
partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition f dict =
    Dict.Refined.partition f dict


{-| Combine two dicts. If there is a collision, preference is given to the first
dict.
-}
union : Dict k v -> Dict k v -> Dict k v
union dictA dictB =
    Dict.Refined.union dictA dictB


{-| Keep a key-value pair when its key appears in the second dict. Preference is
given to values in the first dictionary.
-}
intersect : Dict k v -> Dict k v -> Dict k v
intersect dictA dictB =
    Dict.Refined.intersect dictA dictB


{-| Keep a key-value pair when its key does not appear in the second dict.
-}
diff : Dict k v -> Dict k v -> Dict k v
diff dictA dictB =
    Dict.Refined.diff dictA dictB


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
    -> Dict k a
    -> Dict k b
    -> result
    -> result
merge f g h dictA dictB =
    Dict.Refined.merge f g h dictA dictB
