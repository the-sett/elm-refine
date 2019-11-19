module Refined exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Regex


type Refined i a e
    = Refined (i -> Result e a) (Decoder i) (i -> Value) (e -> String) (a -> i)


make : (i -> Result e a) -> Decoder i -> (i -> Value) -> (e -> String) -> (a -> i) -> Refined i a e
make guardFn dec enc errorToStringFn unboxFn =
    Refined guardFn dec enc errorToStringFn unboxFn


build : Refined i a e -> i -> Result e a
build (Refined buildFn _ _ _ _) val =
    buildFn val


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


encoder : Refined i a e -> a -> Value
encoder (Refined _ _ encoderI _ unboxFn) val =
    unboxFn val
        |> encoderI



{- Dicts -}
-- emptyDict : Refined comparable k e -> Dict comparable k v
--
--
-- singletonDict : Refined comparable k e -> k -> v -> Dict comparable k v
{- Sets -}
-- emptySet : Refined comparable k e -> Set comparable k
--
--
-- singletonSet : Refined comparable k e -> k -> Set comparable k
-- Helper guard functions for numbers.


type IntError
    = BelowRange
    | AboveRange


intErrorToString : IntError -> String
intErrorToString err =
    case err of
        BelowRange ->
            "Too low."

        AboveRange ->
            "Too high."


gt : Int -> Int -> Result IntError Int
gt bound val =
    if bound <= val then
        Err BelowRange

    else
        Ok val


lt : Int -> Int -> Result IntError Int
lt bound val =
    if bound >= val then
        Err AboveRange

    else
        Ok val



-- Helper guard functions for strings.


type StringError
    = TooShort
    | TooLong
    | NotMatchingRegex


stringErrorToString : StringError -> String
stringErrorToString err =
    case err of
        TooShort ->
            "Too short."

        TooLong ->
            "Too long."

        NotMatchingRegex ->
            "Not matching regex."


minLength : Int -> String -> Result StringError String
minLength bound val =
    if String.length val < bound then
        Err TooShort

    else
        Ok val


maxLength : Int -> String -> Result StringError String
maxLength bound val =
    if String.length val > bound then
        Err TooLong

    else
        Ok val


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
