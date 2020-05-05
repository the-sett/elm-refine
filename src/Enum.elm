module Enum exposing
    ( Enum
    , define, build
    , decoder, encoder, toString
    , emptyDict, singletonDict, dictDecoder, dictEncoder, stringDict
    )

{-| Enum provides support for various different ways of defining an enum in Elm.


# Definition of Enums and functions to create them.

@docs Enum
@docs define, build


# Helper functions for working with Enums.

@docs decoder, encoder, toString


# Dicts over enum keys.

@docs emptyDict, singletonDict, dictDecoder, dictEncoder, stringDict

-}

import Dict
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


{-| Creates a decoder for dictionaries with enum values as keys.
-}
dictDecoder : Enum k -> Decoder v -> Decoder (Dict.Enum.Dict k v)
dictDecoder enum valDecoder =
    let
        toDict : List ( String, v ) -> Result String (Dict.Enum.Dict k v)
        toDict keyValuePairs =
            List.foldl
                (\( fieldName, v ) accum ->
                    case ( build enum fieldName, accum ) of
                        ( Just k, Ok dict ) ->
                            Dict.Enum.insert k v dict |> Ok

                        ( Nothing, _ ) ->
                            "Field with name '"
                                ++ fieldName
                                ++ "' is not a member of the enum."
                                |> Err

                        ( _, Err _ ) ->
                            accum
                )
                (emptyDict enum |> Ok)
                keyValuePairs
    in
    Decode.keyValuePairs valDecoder
        |> Decode.andThen
            (\kvps ->
                case toDict kvps of
                    Ok dict ->
                        Decode.succeed dict

                    Err val ->
                        Decode.fail val
            )


{-| JSON Encoder for an enum.
-}
encoder : Enum a -> a -> Value
encoder enum val =
    toString enum val
        |> Encode.string


{-| Creates an encoder for dictionaries with enum values as keys.
-}
dictEncoder : Enum k -> (v -> Value) -> Dict.Enum.Dict k v -> Value
dictEncoder enum valEncoder dict =
    Dict.Enum.foldl (\k v accum -> ( toString enum k, valEncoder v ) :: accum) [] dict
        |> Encode.object


{-| Turns a Dict with enum keys, into a normal Dict with the enum keys as strings.
-}
stringDict : Enum k -> (v -> a) -> Dict.Enum.Dict k v -> Dict.Dict String a
stringDict (Enum _ toStringFn) valEncoder dict =
    Dict.Enum.foldl (\k v accum -> Dict.insert (toStringFn k) (valEncoder v) accum) Dict.empty dict


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
