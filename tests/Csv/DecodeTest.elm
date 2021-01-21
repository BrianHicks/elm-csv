module Csv.DecodeTest exposing (..)

import Csv.Decode as Decode exposing (Decoder, Error(..), Headers(..))
import Expect
import Test exposing (..)


stringTest : Test
stringTest =
    describe "string"
        [ test "a blank string" <|
            \_ ->
                "\"\""
                    |> Decode.decodeCsv NoHeaders Decode.string
                    |> Expect.equal (Ok [ "" ])
        , test "a unquoted value" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders Decode.string
                    |> Expect.equal (Ok [ "a" ])
        , test "an integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoHeaders Decode.string
                    |> Expect.equal (Ok [ "1" ])
        ]


intTest : Test
intTest =
    describe "int"
        [ test "a valid integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.int)
                    |> Expect.equal (Ok [ 1 ])
        , test "an invalid integer" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.int)
                    |> Expect.equal
                        (Err
                            (DecodingError
                                { row = 0
                                , problem = Decode.ExpectedInt "a"
                                }
                            )
                        )
        ]


floatTest : Test
floatTest =
    describe "float"
        [ test "a float shaped like an integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.float)
                    |> Expect.equal (Ok [ 1.0 ])
        , test "a float shaped like a floating-point number" <|
            \_ ->
                "3.14"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.float)
                    |> Expect.equal (Ok [ 3.14 ])
        , test "an invalid float" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.float)
                    |> Expect.equal
                        (Err
                            (DecodingError
                                { row = 0
                                , problem = Decode.ExpectedFloat "a"
                                }
                            )
                        )
        ]


columnTest : Test
columnTest =
    describe "column"
        [ test "can get the only column" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.string)
                    |> Expect.ok
        , test "can get an arbitrary column" <|
            \_ ->
                "a,b,c"
                    |> Decode.decodeCsv NoHeaders (Decode.column 1 Decode.string)
                    |> Expect.equal (Ok [ "b" ])
        , test "issues an error if the column doesn't exist" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.column 1 Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedColumn 1 }))
        ]


fieldTest : Test
fieldTest =
    describe "field"
        [ test "does not work when no field names are provided or present" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        , test "does not work when the provided headers don't contain the name" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv (CustomHeaders []) (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        , test "retrieves the field from custom-provided fields" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv
                        (CustomHeaders [ "Name" ])
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal (Ok [ "a" ])
        , test "does not work when the first row doesn't contain the name" <|
            \_ ->
                "Blah\u{000D}\na"
                    |> Decode.decodeCsv (CustomHeaders []) (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        ]


mapTest : Test
mapTest =
    describe "map functions"
        [ test "can map a single value" <|
            \_ ->
                "5"
                    |> Decode.decodeCsv NoHeaders (Decode.column 0 Decode.int |> Decode.map (\i -> i * 2))
                    |> Expect.equal (Ok [ 10 ])
        , test "map2" <|
            \_ ->
                "1,Atlas"
                    |> Decode.decodeCsv NoHeaders
                        (Decode.map2 Tuple.pair
                            (Decode.column 0 Decode.int)
                            (Decode.column 1 Decode.string)
                        )
                    |> Expect.equal
                        (Ok [ ( 1, "Atlas" ) ])
        , test "map3" <|
            \_ ->
                "1,Atlas,Cat"
                    |> Decode.decodeCsv NoHeaders
                        (Decode.map3 (\id name species -> ( id, name, species ))
                            (Decode.column 0 Decode.int)
                            (Decode.column 1 Decode.string)
                            (Decode.column 2 Decode.string)
                        )
                    |> Expect.equal
                        (Ok [ ( 1, "Atlas", "Cat" ) ])
        ]


succeedTest : Test
succeedTest =
    describe "succeed"
        [ test "ignores the values you send it in favor of the value you provide" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.succeed ())
                    |> Expect.equal (Ok [ () ])
        , test "provides one value for each row" <|
            \_ ->
                "a\u{000D}\nb"
                    |> Decode.decodeCsv NoHeaders (Decode.succeed ())
                    |> Expect.equal (Ok [ (), () ])
        ]


failTest : Test
failTest =
    describe "fail"
        [ test "ignores the values you send it in favor of the value you provide" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoHeaders (Decode.fail "a nice description")
                    |> Expect.equal
                        (Err
                            (DecodingError
                                { problem = Decode.Failure "a nice description"
                                , row = 0
                                }
                            )
                        )
        , test "fails on the first row where it's attempted" <|
            \_ ->
                "a\u{000D}\nb"
                    |> Decode.decodeCsv NoHeaders (Decode.fail "a nice description")
                    |> Expect.equal
                        (Err
                            (DecodingError
                                { problem = Decode.Failure "a nice description"
                                , row = 0
                                }
                            )
                        )
        ]


andThenTest : Test
andThenTest =
    describe "andThen"
        [ describe "for validation" <|
            let
                positiveInteger : Decoder Int
                positiveInteger =
                    Decode.andThen
                        (\value ->
                            if value > 0 then
                                Decode.succeed value

                            else
                                Decode.fail "Only positive integers are allowed!"
                        )
                        Decode.int
            in
            [ test "allows positive integers" <|
                \_ ->
                    "1"
                        |> Decode.decodeCsv NoHeaders (Decode.column 0 positiveInteger)
                        |> Expect.equal (Ok [ 1 ])
            , test "disallows negative integers" <|
                \_ ->
                    "-1"
                        |> Decode.decodeCsv NoHeaders (Decode.column 0 positiveInteger)
                        |> Expect.equal
                            (Err
                                (DecodingError
                                    { problem = Decode.Failure "Only positive integers are allowed!"
                                    , row = 0
                                    }
                                )
                            )
            ]
        , describe "for fields depending on each other" <|
            let
                followThePointer : Decoder String
                followThePointer =
                    Decode.column 0 Decode.int
                        |> Decode.andThen (\column -> Decode.column column Decode.string)
            in
            [ test "get the second column" <|
                \_ ->
                    "1,a,b"
                        |> Decode.decodeCsv NoHeaders followThePointer
                        |> Expect.equal (Ok [ "a" ])
            , test "get the third column" <|
                \_ ->
                    "2,a,b"
                        |> Decode.decodeCsv NoHeaders followThePointer
                        |> Expect.equal (Ok [ "b" ])
            , test "has a reasonable error message for missing a column" <|
                \_ ->
                    "3,a,b"
                        |> Decode.decodeCsv NoHeaders followThePointer
                        |> Expect.equal
                            (Err
                                (DecodingError
                                    { problem = Decode.ExpectedColumn 3
                                    , row = 0
                                    }
                                )
                            )
            ]
        ]
