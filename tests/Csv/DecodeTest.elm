module Csv.DecodeTest exposing (..)

import Csv.Decode as Decode exposing (Decoder, Error(..), FieldNames(..))
import Expect
import Test exposing (..)


stringTest : Test
stringTest =
    describe "string"
        [ test "a blank string" <|
            \_ ->
                "\"\""
                    |> Decode.decodeCsv NoFieldNames Decode.string
                    |> Expect.equal (Ok [ "" ])
        , test "a unquoted value" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames Decode.string
                    |> Expect.equal (Ok [ "a" ])
        , test "an integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoFieldNames Decode.string
                    |> Expect.equal (Ok [ "1" ])
        ]


intTest : Test
intTest =
    describe "int"
        [ test "a valid integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.int)
                    |> Expect.equal (Ok [ 1 ])
        , test "an invalid integer" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.int)
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
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.float)
                    |> Expect.equal (Ok [ 1.0 ])
        , test "a float shaped like a floating-point number" <|
            \_ ->
                "3.14"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.float)
                    |> Expect.equal (Ok [ 3.14 ])
        , test "an invalid float" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.float)
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
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.string)
                    |> Expect.ok
        , test "can get an arbitrary column" <|
            \_ ->
                "a,b,c"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 1 Decode.string)
                    |> Expect.equal (Ok [ "b" ])
        , test "issues an error if the column doesn't exist" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 1 Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedColumn 1 }))
        ]


fieldTest : Test
fieldTest =
    describe "field"
        [ test "does not work when no field names are provided or present" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        , test "does not work when the provided headers don't contain the name" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv
                        (CustomFieldNames [])
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        , test "retrieves the field from custom-provided fields" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv
                        (CustomFieldNames [ "Name" ])
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal (Ok [ "a" ])
        , test "does not work when the first row doesn't contain the name" <|
            \_ ->
                "Blah\u{000D}\na"
                    |> Decode.decodeCsv
                        FromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        , test "fails when there is no first row" <|
            \_ ->
                ""
                    |> Decode.decodeCsv
                        FromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.NoFieldNamesOnFirstRow }))
        , test "fails when name is not present in the first row" <|
            \_ ->
                "Bad\u{000D}\nAtlas"
                    |> Decode.decodeCsv
                        FromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err (DecodingError { row = 0, problem = Decode.ExpectedField "Name" }))
        , test "uses the headers on the first row, if present" <|
            \_ ->
                "Name\u{000D}\nAtlas"
                    |> Decode.decodeCsv
                        FromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Ok [ "Atlas" ])
        ]


mapTest : Test
mapTest =
    describe "map functions"
        [ test "can map a single value" <|
            \_ ->
                "5"
                    |> Decode.decodeCsv NoFieldNames (Decode.column 0 Decode.int |> Decode.map (\i -> i * 2))
                    |> Expect.equal (Ok [ 10 ])
        , test "map2" <|
            \_ ->
                "1,Atlas"
                    |> Decode.decodeCsv NoFieldNames
                        (Decode.map2 Tuple.pair
                            (Decode.column 0 Decode.int)
                            (Decode.column 1 Decode.string)
                        )
                    |> Expect.equal
                        (Ok [ ( 1, "Atlas" ) ])
        , test "map3" <|
            \_ ->
                "1,Atlas,Cat"
                    |> Decode.decodeCsv NoFieldNames
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
                    |> Decode.decodeCsv NoFieldNames (Decode.succeed ())
                    |> Expect.equal (Ok [ () ])
        , test "provides one value for each row" <|
            \_ ->
                "a\u{000D}\nb"
                    |> Decode.decodeCsv NoFieldNames (Decode.succeed ())
                    |> Expect.equal (Ok [ (), () ])
        ]


failTest : Test
failTest =
    describe "fail"
        [ test "ignores the values you send it in favor of the value you provide" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames (Decode.fail "a nice description")
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
                    |> Decode.decodeCsv NoFieldNames (Decode.fail "a nice description")
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
                        |> Decode.decodeCsv NoFieldNames (Decode.column 0 positiveInteger)
                        |> Expect.equal (Ok [ 1 ])
            , test "disallows negative integers" <|
                \_ ->
                    "-1"
                        |> Decode.decodeCsv NoFieldNames (Decode.column 0 positiveInteger)
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
                        |> Decode.decodeCsv NoFieldNames followThePointer
                        |> Expect.equal (Ok [ "a" ])
            , test "get the third column" <|
                \_ ->
                    "2,a,b"
                        |> Decode.decodeCsv NoFieldNames followThePointer
                        |> Expect.equal (Ok [ "b" ])
            , test "has a reasonable error message for missing a column" <|
                \_ ->
                    "3,a,b"
                        |> Decode.decodeCsv NoFieldNames followThePointer
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
