module Refined exposing
    ( Refined
    , define, build
    , decoder, encoder, emptyDict, singletonDict
    , IntError, intErrorToString, gt, lt
    , StringError, stringErrorToString, minLength, maxLength, regexMatch
    )

{-| Refined provides support for common ways of creating refined types in Elm.
A refined type is a basic type like Int or String, that has a constructor which
ensures that it can only take on certain values. The basic type is wrapped in a
custom type which is made opaque - so that only instances of it with the allowable
values can ever be created.


# Definition of Refined types and functions to create them.

@docs Refined
@docs define, build


# Helper functions for working with refined types.

@docs decoder, encoder, emptyDict, singletonDict


# Functions for building refined integers.

@docs IntError, intErrorToString, gt, lt


# Functions for building refined strings.

@docs StringError, stringErrorToString, minLength, maxLength, regexMatch

-}

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


{-| JSON encoder for a refined type.
-}
encoder : Refined i a e -> a -> Value
encoder (Refined _ _ encoderI _ unboxFn) val =
    unboxFn val
        |> encoderI


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
    if bound <= val then
        Err BelowRange

    else
        Ok val


{-| Guard function for creating an integer that must be less than a given value.
-}
lt : Int -> Int -> Result IntError Int
lt bound val =
    if bound >= val then
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
