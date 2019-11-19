module Enum exposing
    ( Enum
    , define, build
    , decoder, encoder, toString, emptyDict, singletonDict
    )

{-| Enum provides support for various different ways of defining an enum in Elm.


# Definition of Enums and functions to create them.

@docs Enum
@docs define, build


# Helper functions for working with Enums.

@docs decoder, encoder, toString, emptyDict, singletonDict

-}

import Dict.Enum
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


{-| An enum is a list of possible values and a function for turning an instance
of one into a string.
-}
type Enum a
    = Enum (List a) (a -> String)


{-| Creates an enum definition from a list of possible values and a definition opf
the `toString` function.
-}
define : List a -> (a -> String) -> Enum a
define vals toStringFn =
    Enum vals toStringFn


{-| Looks up an instance of an enum from its string representation.
-}
build : Enum a -> String -> Maybe a
build (Enum vals toStringFn) val =
    vals
        |> List.filter ((==) val << toStringFn)
        |> List.head


{-| Turns an instance of an enum into a string.
-}
toString : Enum a -> a -> String
toString (Enum _ toStringFn) val =
    toStringFn val


{-| JSON Decoder for an enum
-}
decoder : Enum a -> Decoder a
decoder enum =
    Decode.string
        |> Decode.andThen
            (\val ->
                case build enum val of
                    Just value ->
                        Decode.succeed value

                    Nothing ->
                        Decode.fail <| "Could not decode value to enum: " ++ val
            )


{-| JSON Encoder for an enum.
-}
encoder : Enum a -> a -> Value
encoder enum val =
    toString enum val
        |> Encode.string


{-| Creates an empty dict with an `Enum` key.
-}
emptyDict : Enum k -> Dict.Enum.Dict k v
emptyDict (Enum _ toStringFn) =
    Dict.Enum.empty toStringFn


{-| Creates a dict with a single entry with an `Enum` key.
-}
singletonDict : Enum k -> k -> v -> Dict.Enum.Dict k v
singletonDict (Enum _ toStringFn) =
    Dict.Enum.singleton toStringFn



{- Sets -}
-- emptySet : Enum k -> Set k
--
--
-- singletonSet : Enum k -> k -> Set k
