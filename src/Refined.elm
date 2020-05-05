module Refined exposing
    ( Refined
    , define, build, errorToString
    , decoder, encoder, unbox
    , emptyDict, singletonDict, dictDecoder, dictEncoder, unboxedDict
    , IntError, intErrorToString, gt, gte, lt, lte
    , StringError, stringErrorToString, minLength, maxLength, regexMatch
    )

{-| Refined provides support for common ways of creating refined types in Elm.
A refined type is a basic type like Int or String, that has a constructor which
ensures that it can only take on certain values. The basic type is wrapped in a
custom type which is made opaque - so that only instances of it with the allowable
values can ever be created.


# Definition of Refined types and functions to create them.

@docs Refined
@docs define, build, errorToString


# Helper functions for working with refined types.

@docs decoder, encoder, unbox


# Dicts over refined keys.

@docs emptyDict, singletonDict, dictDecoder, dictEncoder, unboxedDict


# Functions for building refined integers.

@docs IntError, intErrorToString, gt, gte, lt, lte


# Functions for building refined strings.

@docs StringError, stringErrorToString, minLength, maxLength, regexMatch

-}

import Dict
import Dict.Refined
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Regex


{-| Refined types.

`i` is the underlying basic type.
`a` is the refined custom type that will be created
from it.
`e` is the type or errors that the constructor can return when invalid inputs
are given.

-}
type Refined i a e
    = Refined (i -> Result e a) (Decoder i) (i -> Value) (e -> String) (a -> i)


{-| Creates a refined type from the input guard function, decoder on the underlying basic type,
encoder on the underlying basic type, the error to string function, and the unboxing function
that extracts the underlying basic type.
-}
define : (i -> Result e a) -> Decoder i -> (i -> Value) -> (e -> String) -> (a -> i) -> Refined i a e
define guardFn dec enc errorToStringFn unboxFn =
    Refined guardFn dec enc errorToStringFn unboxFn


{-| Builds an instance of a refined type from its input type.
-}
build : Refined i a e -> i -> Result e a
build (Refined buildFn _ _ _ _) val =
    buildFn val


{-| Unboxes an instance of a refined type.
-}
unbox : Refined i a e -> a -> i
unbox (Refined _ _ _ _ unboxFn) val =
    unboxFn val


{-| Prints the error messages resulting from failing to create an instance of a refined type.
-}
errorToString : Refined i a e -> e -> String
errorToString (Refined _ _ _ errorToStringFn _) err =
    errorToStringFn err


{-| JSON decoder for a refined type.
-}
decoder : Refined i a e -> Decoder a
decoder (Refined buildFn decoderI _ errorToStringFn _) =
    decoderI
        |> Decode.andThen
            (\val ->
                case buildFn val of
                    Ok value ->
                        Decode.succeed value

                    Err err ->
                        errorToStringFn err
                            |> Decode.fail
            )


{-| Creates a decoder for dictionaries with refined values as keys.
-}
dictDecoder : Refined comparable k e -> Decoder v -> Decoder (Dict.Refined.Dict comparable k v)
dictDecoder refined valDecoder =
    let
        toDict : List ( String, v ) -> Result String (Dict.Refined.Dict comparable k v)
        toDict keyValuePairs =
            List.foldl
                (\( fieldName, v ) accum ->
                    let
                        fieldNameToRefinedRes =
                            fromString refined fieldName

                        --|> Result.andThen (build refined)
                    in
                    case ( fieldNameToRefinedRes, accum ) of
                        ( Ok k, Ok dict ) ->
                            Dict.Refined.insert k v dict |> Ok

                        ( Err err, _ ) ->
                            "Field name is not a memeber of the refined type. "
                                ++ Decode.errorToString err
                                |> Err

                        ( _, Err _ ) ->
                            accum
                )
                (emptyDict refined |> Ok)
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


{-| JSON encoder for a refined type.
-}
encoder : Refined i a e -> a -> Value
encoder (Refined _ _ encoderI _ unboxFn) val =
    unboxFn val
        |> encoderI


{-| Creates an encoder for dictionaries with refined values as keys.
-}
dictEncoder : Refined comparable k e -> (v -> Value) -> Dict.Refined.Dict comparable k v -> Value
dictEncoder refined valEncoder dict =
    Dict.Refined.foldl (\k v accum -> ( toString refined k, valEncoder v ) :: accum) [] dict
        |> Encode.object


{-| Turns a Dict with refined keys, into a normal Dict with the refined keys unboxed to their underlying type.
-}
unboxedDict : Refined comparable k e -> (v -> a) -> Dict.Refined.Dict comparable k v -> Dict.Dict comparable a
unboxedDict (Refined _ _ _ _ unboxFn) valEncoder dict =
    Dict.Refined.foldl (\k v accum -> Dict.insert (unboxFn k) (valEncoder v) accum) Dict.empty dict


toString refined val =
    let
        jsonAsString =
            encoder refined val |> Encode.encode 0
    in
    if String.startsWith "\"" jsonAsString then
        jsonAsString
            |> String.dropLeft 1
            |> String.dropRight 1

    else
        jsonAsString


fromString refined val =
    Decode.decodeString (decoder refined) val


{-| Creates an empty dict with a `Refined` key.
-}
emptyDict : Refined comparable k e -> Dict.Refined.Dict comparable k v
emptyDict (Refined _ _ _ _ unboxFn) =
    Dict.Refined.empty unboxFn


{-| Creates a dict with a single entry with a `Refined` key.
-}
singletonDict : Refined comparable k e -> k -> v -> Dict.Refined.Dict comparable k v
singletonDict (Refined _ _ _ _ unboxFn) =
    Dict.Refined.singleton unboxFn



{- Sets -}
-- emptySet : Refined comparable k e -> Set comparable k
--
--
-- singletonSet : Refined comparable k e -> k -> Set comparable k
-- Helper guard functions for numbers.


{-| Describes the possible errors that can occur when creating a refined integer.
-}
type IntError
    = BelowRange
    | AboveRange


{-| Translates integer errors to descriptive strings.
-}
intErrorToString : IntError -> String
intErrorToString err =
    case err of
        BelowRange ->
            "Too low."

        AboveRange ->
            "Too high."


{-| Guard function for creating an integer that must be greater than a given value.
-}
gt : Int -> Int -> Result IntError Int
gt bound val =
    if val <= bound then
        Err BelowRange

    else
        Ok val


{-| Guard function for creating an integer that must be greater than or equal to
a given value.
-}
gte : Int -> Int -> Result IntError Int
gte bound val =
    if val < bound then
        Err BelowRange

    else
        Ok val


{-| Guard function for creating an integer that must be less than a given value.
-}
lt : Int -> Int -> Result IntError Int
lt bound val =
    if val >= bound then
        Err AboveRange

    else
        Ok val


{-| Guard function for creating an integer that must be less than or equal to
a given value.
-}
lte : Int -> Int -> Result IntError Int
lte bound val =
    if val > bound then
        Err AboveRange

    else
        Ok val



-- Helper guard functions for strings.


{-| Describes the possible errors that can occur when creating a refined string.
-}
type StringError
    = TooShort
    | TooLong
    | NotMatchingRegex


{-| Translates string errors to descriptive strings.
-}
stringErrorToString : StringError -> String
stringErrorToString err =
    case err of
        TooShort ->
            "Too short."

        TooLong ->
            "Too long."

        NotMatchingRegex ->
            "Not matching regex."


{-| Guard function for creating a string that must have a given minimum length.
-}
minLength : Int -> String -> Result StringError String
minLength bound val =
    if String.length val < bound then
        Err TooShort

    else
        Ok val


{-| Guard function for creating a string that must have a given maximum length.
-}
maxLength : Int -> String -> Result StringError String
maxLength bound val =
    if String.length val > bound then
        Err TooLong

    else
        Ok val


{-| Guard function for creating a string that must match a given regular expression.
-}
regexMatch : String -> String -> Result StringError String
regexMatch pattern val =
    let
        regex =
            Regex.fromString pattern
                |> Maybe.withDefault Regex.never
    in
    if Regex.contains regex val then
        Ok val

    else
        Err NotMatchingRegex
