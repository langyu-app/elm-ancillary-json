module TestJsonDecode exposing (suite)

import Dict
import Expect
import Fuzz
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Ancillary
    exposing
        ( lookup
        , lookupComparable
        , lookupWith
        , mapByField
        , mapMaybe
        , mapResult
        , nonemptyString
        , validate
        )
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser)
import Test exposing (Test, concat, describe, fuzz)
import TestExtra as TestX exposing (DecoderExpectation(..))


{-| Test suite for `Json.Decode.Ancillary`.
-}
suite : Test
suite =
    concat
        [ describe "Validating Decoders"
            [ TestX.describeDecoder "nonemptyString"
                nonemptyString
                identity
                [ ( "true", FailsToDecode )
                , ( "42", FailsToDecode )
                , ( "3.14", FailsToDecode )
                , ( "\"hello\"", DecodesTo "hello" )
                , ( "\" \"", DecodesTo " " )
                , ( "\"\"", FailsToDecode )
                , ( "{ \"hello\": 42 }", FailsToDecode )
                ]
            ]
        , describe "Custom Validating Decoders"
            [ describe "validate"
                [ fuzz Fuzz.int
                    "test even ints"
                    (\i ->
                        let
                            predicate : Int -> Bool
                            predicate =
                                (==) 0 << modBy 2

                            result : Result Decode.Error Int
                            result =
                                Decode.decodeValue decoder <| Encode.int i

                            decoder : Decoder Int
                            decoder =
                                validate predicate "Invalid!" Decode.int
                        in
                        if predicate i then
                            Expect.equal result <| Ok i

                        else
                            Expect.err result
                    )
                , fuzz Fuzz.string
                    "test nonempty strings"
                    (\s ->
                        let
                            result : Result Decode.Error String
                            result =
                                Decode.decodeValue decoder <| Encode.string s

                            decoder : Decoder String
                            decoder =
                                validate predicate "Invalid!" Decode.string

                            predicate : String -> Bool
                            predicate =
                                not << String.isEmpty
                        in
                        if String.isEmpty s then
                            Expect.err result

                        else
                            Expect.equal result <| Ok s
                    )
                ]
            , describe "mapMaybe"
                [ fuzz
                    (Fuzz.oneOf [ Fuzz.string, Fuzz.map String.fromInt Fuzz.int ])
                    "string to int"
                    (\s ->
                        let
                            result : Result Decode.Error Int
                            result =
                                Decode.decodeValue decoder <| Encode.string s

                            decoder : Decoder Int
                            decoder =
                                mapMaybe String.toInt "Expected a string-encoded integer!" Decode.string
                        in
                        case String.toInt s of
                            Just i ->
                                Expect.equal result <| Ok i

                            Nothing ->
                                Expect.err result
                    )
                , fuzz
                    (Fuzz.list Fuzz.string)
                    "list head"
                    (\ss ->
                        let
                            result : Result Decode.Error String
                            result =
                                Decode.decodeValue decoder <| Encode.list Encode.string ss

                            decoder : Decoder String
                            decoder =
                                mapMaybe List.head "Expected a nonempty list!" (Decode.list Decode.string)
                        in
                        if List.isEmpty ss then
                            Expect.err result

                        else
                            Expect.ok result
                    )
                ]
            , describe "mapResult"
                [ fuzz
                    (Fuzz.oneOf [ Fuzz.string, Fuzz.map String.fromInt Fuzz.int ])
                    "string to int"
                    (\s ->
                        let
                            result : Result Decode.Error Int
                            result =
                                Decode.decodeValue decoder <| Encode.string s

                            decoder : Decoder Int
                            decoder =
                                mapResult
                                    (Result.mapError (always "dead ends haven't been implemented") << Parser.run intParser)
                                    Decode.string

                            intParser : Parser Int
                            intParser =
                                Parser.oneOf
                                    [ Parser.succeed negate
                                        |. Parser.symbol "-"
                                        |= Parser.int
                                    , Parser.int
                                    ]
                                    |. Parser.end
                        in
                        case String.toInt s of
                            Just i ->
                                Expect.equal result <| Ok i

                            Nothing ->
                                Expect.err result
                    )
                ]
            ]
        , describe "Custom-type Decoders"
            [ describe "lookup"
                [ TestX.describeDecoder
                    "charDecoder"
                    (lookup
                        (Dict.fromList
                            [ ( "A", 'A' )
                            , ( "B", 'B' )
                            , ( "C", 'C' )
                            ]
                        )
                    )
                    String.fromChar
                    [ ( "true", FailsToDecode )
                    , ( "42", FailsToDecode )
                    , ( "3.14", FailsToDecode )
                    , ( "\"hello\"", FailsToDecode )
                    , ( "\"\"", FailsToDecode )
                    , ( "\"A\"", DecodesTo 'A' )
                    , ( "{ \"A\": 42 }", FailsToDecode )
                    ]
                ]
            , describe "lookupComparable"
                [ TestX.describeDecoder
                    "asciiDecoder"
                    (lookupComparable
                        (Dict.fromList
                            [ ( 65, 'A' )
                            , ( 66, 'B' )
                            , ( 67, 'C' )
                            ]
                        )
                        Decode.int
                    )
                    String.fromChar
                    [ ( "true", FailsToDecode )
                    , ( "42", FailsToDecode )
                    , ( "66", DecodesTo 'B' )
                    , ( "3.14", FailsToDecode )
                    , ( "\"hello\"", FailsToDecode )
                    , ( "\"\"", FailsToDecode )
                    , ( "{ \"hello\": 42 }", FailsToDecode )
                    ]
                ]
            , describe "lookupWith"
                [ TestX.describeDecoder
                    "asciiDecoder"
                    (lookupWith
                        [ ( 65, 'A' )
                        , ( 66, 'B' )
                        , ( 67, 'C' )
                        ]
                        Decode.int
                    )
                    String.fromChar
                    [ ( "true", FailsToDecode )
                    , ( "42", FailsToDecode )
                    , ( "66", DecodesTo 'B' )
                    , ( "3.14", FailsToDecode )
                    , ( "\"66\"", FailsToDecode )
                    , ( "\"\"", FailsToDecode )
                    , ( "{ \"66\": 42 }", FailsToDecode )
                    ]
                ]
            , describe "mapByField"
                [ TestX.describeDecoder
                    "animalDecoder"
                    (mapByField
                        [ ( "dog", \i -> { age = i, species = "Dog" } )
                        , ( "cat", \i -> { age = i, species = "Cat" } )
                        , ( "bird", \i -> { age = i, species = "Bird" } )
                        ]
                        Decode.int
                    )
                    (\{ age, species } -> species ++ String.fromInt age)
                    [ ( "true", FailsToDecode )
                    , ( "42", FailsToDecode )
                    , ( "{\"dog\": 6}", DecodesTo { age = 6, species = "Dog" } )
                    , ( "{\"bird\": 10}", DecodesTo { age = 10, species = "Bird" } )
                    , ( "3.14", FailsToDecode )
                    , ( "\"dog\"", FailsToDecode )
                    , ( "{\"dog\": \"hello\"}", FailsToDecode )
                    ]
                ]
            ]
        ]
