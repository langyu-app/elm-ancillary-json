module Json.Decode.Ancillary exposing
    ( nonemptyString
    , validate, mapMaybe, mapResult
    , lookup, lookupComparable, lookupWith, mapByField
    )

{-| The `Json.Decode.Ancillary` module provides additional, convenient Json
decoding functionality not found in `Json.Decode` or `Json.Decode.Extra`.


# Validating Decoders

Decode simple types from JSON with additional restrictions.

@docs nonemptyString


# Custom Validating Decoders

Create custom decoders that validate additional characteristics of the decoded
values.

@docs validate, mapMaybe, mapResult


# Custom-type Decoders

It's not uncommon to want to import JSON representations of custom types, in
which custom types are typically represented by strings, e.g.

    {
        "animals": ["dog", "cat", "cat"]
    }

    ->

    animals : List Animal
    animals =
        [ Dog, Cat, Cat ]

or

    {
        "animals": [{
            "dog": "Spot"
        }, {
            "cat": "Bastet"
        }, {
            "dog": "Rover"
        }]
    }

    ->

    animals : List Animal
    animals =
        [ Dog "Spot"
        , Cat "Bastet"
        , Dog "Rover"
        ]

These functions can help handle such JSON, though they may have applications
beyond mere custom type handling.

@docs lookup, lookupComparable, lookupWith, mapByField

-}

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeX



-- * Validating Decoders


{-| Decode a JSON `String` to Elm but fail if the string is empty.

    decodeString nonemptyString "true"              == Err ...
    decodeString nonemptyString "42"                == Err ...
    decodeString nonemptyString "3.14"              == Err ...
    decodeString nonemptyString "\"hello\""         == Ok "hello"
    decodeString nonemptyString "\" \""             == Ok " "
    decodeString nonemptyString "\"\""              == Err ...
    decodeString nonemptyString "{ \"hello\": 42 }" == Err ...

-}
nonemptyString : Decoder String
nonemptyString =
    validate (not << String.isEmpty) "Expected a nonempty string!" Decode.string



-- * Custom Validating Decoders


{-| Given a predicate to validate a decoded value and an error message for
invalid values, decode a value from JSON and fail if it does not evaluate to
`True`.

Examples:

    nonemptyString : Decoder String
    nonemptyString =
        Decode.string
            |> validate (not << String.isEmpty) "Expected a nonempty string!"

-}
validate : (a -> Bool) -> String -> Decoder a -> Decoder a
validate predicate error =
    Decode.andThen
        (\a ->
            if predicate a then
                Decode.succeed a

            else
                Decode.fail error
        )


{-| Given a function to map a value `a` into `Maybe b` and an error message for
invalid values, decode an `a` value from JSON and convert it into a `b` or fail.

    firstChar : Decoder Char
    firstChar =
        Decode.string
            |> mapMaybe (Maybe.map Tuple.first << String.uncons) "Expected a nonempty string!"

-}
mapMaybe : (a -> Maybe b) -> String -> Decoder a -> Decoder b
mapMaybe toMaybe error =
    Decode.map toMaybe
        >> Decode.andThen (DecodeX.fromMaybe error)


{-| Given a function to map a value `a` into `Result String b`, decode an `a`
value from JSON and convert it into a `b` or fail with the error message.

    customParser : Parser SomeType
    customParser =
        ...

    customDecoder : Decoder SomeType
    customDecoder =
        Decode.string
            |> mapResult (Result.mapError Parser.deadEndsToString << Parser.run customParser )

-}
mapResult : (a -> Result String b) -> Decoder a -> Decoder b
mapResult toResult =
    Decode.map toResult
        >> Decode.andThen DecodeX.fromResult



-- * Custom-type Decoders


{-| Given an association dictionary between strings and a type, decode a string
from JSON into the associated value or fail if none match.

    animalDecoder : Decoder Animal
    animalDecoder =
        lookup
            (Dict.fromList
                [ ( "dog", Dog )
                , ( "cat", Cat )
                , ( "bird", Bird )
                ]
            )

    decodeString animalDecoder "true"      == Err ...
    decodeString animalDecoder "42"        == Err ...
    decodeString animalDecoder "3.14"      == Err ...
    decodeString animalDecoder "\"hello\"" == Err ...
    decodeString animalDecoder "\"dog\""   == Ok Dog
    decodeString animalDecoder "\"bird\""  == Ok Bird

-}
lookup : Dict String b -> Decoder b
lookup assocs =
    mapMaybe (flip Dict.get assocs) "No matching value found!" Decode.string


{-| Given an association dictionary between two types, decode the first type
from JSON into the associated value of the second type or fail if none match.

Used `lookup` if your `comparable` type is `String`, the more common use-case.

    asciiDecoder : Decoder Char
    asciiDecoder =
        lookupComparable
            (Dict.fromList
                [
                , ...
                , ( 65, 'A' )
                , ( 66, 'B' )
                , ( 67, 'C' )
                , ...
            )
            Decode.int

    decodeString asciiDecoder "true"      == Err ...
    decodeString asciiDecoder "66"        == Ok 'B'
    decodeString asciiDecoder "99999"     == Err ...
    decodeString asciiDecoder "3.14"      == Err ...
    decodeString asciiDecoder "\"hello\"" == Err ...

-}
lookupComparable : Dict comparable a -> Decoder comparable -> Decoder a
lookupComparable assocs =
    mapMaybe (flip Dict.get assocs) "No matching value found!"


{-| Less-efficient alternative (except perhaps with short lists) to
`lookupComparable` for non-comparable values. Given an association list between
two types, decode the first type from JSON into the associated value of the
second type or fail if none match.

You should probably use `lookup` or `lookupComparable` if possible, unless your
list is very short.

    asciiDecoder : Decoder Char
    asciiDecoder =
        lookupWith
            [
            , ...
            , ( 65, 'A' )
            , ( 66, 'B' )
            , ( 67, 'C' )
            , ...
            ]
            Decode.int

    decodeString asciiDecoder "true"      == Err ...
    decodeString asciiDecoder "66"        == Ok 'B'
    decodeString asciiDecoder "99999"     == Err ...
    decodeString asciiDecoder "3.14"      == Err ...
    decodeString asciiDecoder "\"hello\"" == Err ...

-}
lookupWith : List ( a, b ) -> Decoder a -> Decoder b
lookupWith assocs d =
    List.map (\( s, b ) -> DecodeX.when d ((==) s) (Decode.succeed b)) assocs
        |> Decode.oneOf


{-| Given an association list of fields (as strings) and functions mapping one
type to another, and a decoder of the first type, map the contents of a
matching field with the associated map.

This is a little complicated sounding, so consider the following example:

    type Species
        = Dog
        | Cat
        | Bird

    type alias Animal =
        { species : Species
        , age : Int
        }

    animalDecoder : Decoder Animal
    animalDecoder =
        mapByField
            [ ( "dog", Animal Dog )
            , ( "cat", Animal Cat )
            , ( "bird", Animal Bird )
            ]
            Decode.int

    decodeString animalDecoder "true"                 == Err ...
    decodeString animalDecoder "42"                   == Err ...
    decodeString animalDecoder "{\"dog\": 6}"         == Ok ( Dog 42 )
    decodeString animalDecoder "{\"bird\": 10}"       == Ok ( Bird 10 )
    decodeString animalDecoder "\"dog\""              == Err ...
    decodeString animalDecoder "{\"dog\": \"hello\"}" == Err ...

-}
mapByField : List ( String, a -> b ) -> Decoder a -> Decoder b
mapByField ms d =
    List.map (\( s, f ) -> Decode.map f <| Decode.field s d) ms
        |> Decode.oneOf
