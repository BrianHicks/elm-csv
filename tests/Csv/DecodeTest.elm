module Csv.DecodeTest exposing (..)

import Csv.Decode as Decode exposing (Column(..), Decoder, Error(..), FieldNames(..))
import Expect
import Hex
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
        , test "multiple columns" <|
            \_ ->
                "1,2"
                    |> Decode.decodeCsv NoFieldNames Decode.string
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.ExpectedOneColumn 2
                                    }
                                ]
                            )
                        )
        ]


intTest : Test
intTest =
    describe "int"
        [ test "a valid integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoFieldNames Decode.int
                    |> Expect.equal (Ok [ 1 ])
        , test "an integer with spaces around" <|
            \_ ->
                " 1 "
                    |> Decode.decodeCsv NoFieldNames Decode.int
                    |> Expect.equal (Ok [ 1 ])
        , test "an invalid integer" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames Decode.int
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.ExpectedInt "a"
                                    }
                                ]
                            )
                        )
        , test "multiple columns" <|
            \_ ->
                "1,2"
                    |> Decode.decodeCsv NoFieldNames Decode.int
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.ExpectedOneColumn 2
                                    }
                                ]
                            )
                        )
        ]


floatTest : Test
floatTest =
    describe "float"
        [ test "a float shaped like an integer" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoFieldNames Decode.float
                    |> Expect.equal (Ok [ 1.0 ])
        , test "a float shaped like a floating-point number" <|
            \_ ->
                "3.14"
                    |> Decode.decodeCsv NoFieldNames Decode.float
                    |> Expect.equal (Ok [ 3.14 ])
        , test "a float with spaces around" <|
            \_ ->
                " 3.14 "
                    |> Decode.decodeCsv NoFieldNames Decode.float
                    |> Expect.equal (Ok [ 3.14 ])
        , test "an invalid float" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames Decode.float
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.ExpectedFloat "a"
                                    }
                                ]
                            )
                        )
        , test "multiple columns" <|
            \_ ->
                "1,2"
                    |> Decode.decodeCsv NoFieldNames Decode.float
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.ExpectedOneColumn 2
                                    }
                                ]
                            )
                        )
        ]


blankTest : Test
blankTest =
    describe "blank"
        [ test "when the field is blank" <|
            \_ ->
                ""
                    |> Decode.decodeCsv NoFieldNames (Decode.blank Decode.int)
                    |> Expect.equal (Ok [])
        , test "when the field contains spaces" <|
            \_ ->
                "  "
                    |> Decode.decodeCsv NoFieldNames (Decode.blank Decode.int)
                    |> Expect.equal (Ok [ Nothing ])
        , test "when the field contains whitespace characters" <|
            \_ ->
                "\"\u{00A0}\t\n\""
                    |> Decode.decodeCsv NoFieldNames (Decode.blank Decode.int)
                    |> Expect.equal (Ok [ Nothing ])
        , test "when the field is non-blank but not valid for the decoder" <|
            \_ ->
                "banana"
                    |> Decode.decodeCsv NoFieldNames (Decode.blank Decode.int)
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.ExpectedInt "banana"
                                    }
                                ]
                            )
                        )
        , test "when the field is non-blank and valid for the decoder" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoFieldNames (Decode.blank Decode.int)
                    |> Expect.equal (Ok [ Just 1 ])
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
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = Column 1
                                    , problem = Decode.ColumnNotFound 1
                                    }
                                ]
                            )
                        )
        ]


fieldTest : Test
fieldTest =
    describe "field"
        [ test "fails when no field names are provided or present" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err
                            (DecodingErrors [ Decode.FieldNotProvided "Name" ])
                        )
        , test "fails when the provided headers don't contain the name" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv
                        (CustomFieldNames [])
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldNotProvided "Name"
                                ]
                            )
                        )
        , test "fails when the first row doesn't contain the name" <|
            \_ ->
                "Blah\u{000D}\na"
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldNotProvided "Name" ]
                            )
                        )
        , test "fails when there is no first row" <|
            \_ ->
                ""
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal (Err Decode.NoFieldNamesOnFirstRow)
        , test "fails when name is not present in the first row" <|
            \_ ->
                "Bad\u{000D}\nAtlas"
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldNotProvided "Name" ]
                            )
                        )
        , test "fails when the associated column is not present in the row" <|
            \_ ->
                "Name,Other\u{000D}\nAtlas"
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Other" Decode.string)
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 1
                                    , column = Field "Other" (Just 1)
                                    , problem = Decode.FieldNotFound "Other"
                                    }
                                ]
                            )
                        )
        , test "retrieves the field from custom-provided fields" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv
                        (CustomFieldNames [ "Name" ])
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal (Ok [ "a" ])
        , test "uses the headers on the first row, if present" <|
            \_ ->
                "Name\u{000D}\nAtlas"
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Ok [ "Atlas" ])
        , test "uses the headers on the first row, trimmed" <|
            \_ ->
                " Name \u{000D}\nAtlas"
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Name" Decode.string)
                    |> Expect.equal
                        (Ok [ "Atlas" ])
        , test "fails with the right line number after getting field names from the first row" <|
            \_ ->
                "Number\u{000D}\nnot a number"
                    |> Decode.decodeCsv
                        FieldNamesFromFirstRow
                        (Decode.field "Number" Decode.int)
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 1
                                    , column = Field "Number" (Just 0)
                                    , problem = Decode.ExpectedInt "not a number"
                                    }
                                ]
                            )
                        )
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


oneOfTest : Test
oneOfTest =
    describe "oneOf"
        [ test "decodes a value" <|
            \_ ->
                "1"
                    |> Decode.decodeCsv NoFieldNames (Decode.oneOf Decode.int [])
                    |> Expect.equal (Ok [ 1 ])
        , test "uses a fallback" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames
                        (Decode.oneOf
                            (Decode.map Just Decode.int)
                            [ Decode.succeed Nothing ]
                        )
                    |> Expect.equal (Ok [ Nothing ])
        , test "gives all the errors if all the decoders fail" <|
            \_ ->
                "a"
                    |> Decode.decodeCsv NoFieldNames
                        (Decode.oneOf
                            (Decode.fail "ONE")
                            [ Decode.fail "TWO"
                            , Decode.fail "THREE"
                            ]
                        )
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.OneOfDecodingError 0
                                    [ Decode.FieldDecodingError
                                        { row = 0
                                        , column = OnlyColumn
                                        , problem = Decode.Failure "ONE"
                                        }
                                    , Decode.FieldDecodingError
                                        { row = 0
                                        , column = OnlyColumn
                                        , problem = Decode.Failure "TWO"
                                        }
                                    , Decode.FieldDecodingError
                                        { row = 0
                                        , column = OnlyColumn
                                        , problem = Decode.Failure "THREE"
                                        }
                                    ]
                                ]
                            )
                        )
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
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.Failure "a nice description"
                                    }
                                ]
                            )
                        )
        , test "fails on every row where it's attempted" <|
            \_ ->
                "a\u{000D}\nb"
                    |> Decode.decodeCsv NoFieldNames (Decode.fail "a nice description")
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.Failure "a nice description"
                                    }
                                , Decode.FieldDecodingError
                                    { row = 1
                                    , column = OnlyColumn
                                    , problem = Decode.Failure "a nice description"
                                    }
                                ]
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
                        |> Decode.decodeCsv NoFieldNames positiveInteger
                        |> Expect.equal (Ok [ 1 ])
            , test "disallows negative integers" <|
                \_ ->
                    "-1"
                        |> Decode.decodeCsv NoFieldNames positiveInteger
                        |> Expect.equal
                            (Err
                                (DecodingErrors
                                    [ Decode.FieldDecodingError
                                        { row = 0
                                        , column = OnlyColumn
                                        , problem = Decode.Failure "Only positive integers are allowed!"
                                        }
                                    ]
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
                                (DecodingErrors
                                    [ Decode.FieldDecodingError
                                        { row = 0
                                        , column = Column 3
                                        , problem = Decode.ColumnNotFound 3
                                        }
                                    ]
                                )
                            )
            ]
        ]


fromResultTest : Test
fromResultTest =
    let
        hex : Decoder Int
        hex =
            Decode.string
                |> Decode.andThen (Decode.fromResult << Hex.fromString)
    in
    describe "fromResult"
        [ test "succeeds when the function returns Ok" <|
            \_ ->
                "ff"
                    |> Decode.decodeCsv NoFieldNames hex
                    |> Expect.equal (Ok [ 255 ])
        , test "fails when the function returns Err" <|
            \_ ->
                "banana"
                    |> Decode.decodeCsv NoFieldNames hex
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.Failure "\"banana\" is not a valid hexadecimal string because n is not a valid hexadecimal character."
                                    }
                                ]
                            )
                        )
        ]


fromMaybeTest : Test
fromMaybeTest =
    let
        myInt : Decoder Int
        myInt =
            Decode.string
                |> Decode.andThen (Decode.fromMaybe "Expected an int" << String.toInt)
    in
    describe "fromMaybe"
        [ test "succeeds when the function returns Just" <|
            \_ ->
                "123"
                    |> Decode.decodeCsv NoFieldNames myInt
                    |> Expect.equal (Ok [ 123 ])
        , test "fails when the function returns Nothing" <|
            \_ ->
                "banana"
                    |> Decode.decodeCsv NoFieldNames myInt
                    |> Expect.equal
                        (Err
                            (DecodingErrors
                                [ Decode.FieldDecodingError
                                    { row = 0
                                    , column = OnlyColumn
                                    , problem = Decode.Failure "Expected an int"
                                    }
                                ]
                            )
                        )
        ]


testErrorToString : Test
testErrorToString =
    describe "errorToString"
        [ test "groups simple errors" <|
            \() ->
                "a\na\na"
                    |> Decode.decodeCsv NoFieldNames Decode.int
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "There was a problem on rows 0–2, column 0 (the only column present): I could not parse an int from `a`.")
        , test "groups simple errors in more complex settings - errors are sorted by row occurence" <|
            \() ->
                "a\na\nb\na\na"
                    |> Decode.decodeCsv NoFieldNames Decode.int
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "I saw 3 problems while decoding this CSV:\n\nThere was a problem on rows 0 and 1, column 0 (the only column present): I could not parse an int from `a`.\n\nThere was a problem on row 2, column 0 (the only column present): I could not parse an int from `b`.\n\nThere was a problem on rows 3 and 4, column 0 (the only column present): I could not parse an int from `a`.")
        , test "works with map2" <|
            \() ->
                "foo,bar\na,2\na,b\na,c"
                    |> Decode.decodeCsv FieldNamesFromFirstRow (Decode.map2 Tuple.pair (Decode.field "foo" Decode.int) (Decode.field "bar" Decode.int))
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "I saw 3 problems while decoding this CSV:\n\nThere was a problem on rows 1–3, in the `foo` field (column 0): I could not parse an int from `a`.\n\nThere was a problem on row 2, in the `bar` field (column 1): I could not parse an int from `b`.\n\nThere was a problem on row 3, in the `bar` field (column 1): I could not parse an int from `c`.")
        , test "works with oneOf" <|
            \() ->
                "a\n1\n1.2"
                    |> Decode.decodeCsv NoFieldNames (Decode.oneOf (Decode.map toFloat Decode.int) [ Decode.float ])
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "There was a problem on row 0 - all of the following decoders failed, but at least one must succeed:\n  (1) column 0 (the only column present): I could not parse an int from `a`.\n  (2) column 0 (the only column present): I could not parse a float from `a`.")
        , test "works with nested oneOf" <|
            \() ->
                "a\n1\n1.2"
                    |> Decode.decodeCsv NoFieldNames (Decode.oneOf Decode.float [ Decode.oneOf (Decode.map toFloat Decode.int) [ Decode.float ] ])
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "There was a problem on row 0 - all of the following decoders failed, but at least one must succeed:\n  (1) column 0 (the only column present): I could not parse a float from `a`.\n  (2) column 0 (the only column present): I could not parse an int from `a`.\n  (3) column 0 (the only column present): I could not parse a float from `a`.")
        , test "works with complex decoder" <|
            \() ->
                "foo,bar\na,2\na,b\na,c"
                    |> Decode.decodeCsv FieldNamesFromFirstRow (Decode.map2 Tuple.pair (Decode.oneOf (Decode.field "foo" Decode.int) [ Decode.field "bar" (Decode.map round Decode.float) ]) (Decode.field "bar" Decode.int))
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "I saw 4 problems while decoding this CSV:\n\nThere was a problem on row 2, in the `bar` field (column 1): I could not parse an int from `b`.\n\nThere was a problem on row 2 - all of the following decoders failed, but at least one must succeed:\n  (1) in the `foo` field (column 0): I could not parse an int from `a`.\n  (2) in the `bar` field (column 1): I could not parse a float from `b`.\n\nThere was a problem on row 3, in the `bar` field (column 1): I could not parse an int from `c`.\n\nThere was a problem on row 3 - all of the following decoders failed, but at least one must succeed:\n  (1) in the `foo` field (column 0): I could not parse an int from `a`.\n  (2) in the `bar` field (column 1): I could not parse a float from `c`.")
        ]


availableFieldsTest : Test
availableFieldsTest =
    describe "availableFields"
        [ test "returns header row in order" <|
            \() ->
                "foo,bar\na,a\na,b\na,c"
                    |> Decode.decodeCsv FieldNamesFromFirstRow Decode.availableFields
                    |> Expect.equal (Ok [ [ "foo", "bar" ], [ "foo", "bar" ], [ "foo", "bar" ] ])
        , test "allows conditional decoding based on header row" <|
            \() ->
                "foo,bar\na,a\na,b\na,c"
                    |> Decode.decodeCsv FieldNamesFromFirstRow
                        (Decode.availableFields
                            |> Decode.andThen
                                (\headers ->
                                    if List.member "bar" headers then
                                        Decode.field "bar" Decode.string

                                    else
                                        Decode.field "foo" Decode.string
                                )
                        )
                    |> Expect.equal (Ok [ "a", "b", "c" ])
        , test "returns configured fields" <|
            \() ->
                "\n"
                    |> Decode.decodeCsv (CustomFieldNames [ "Foo", "Bar" ]) Decode.availableFields
                    |> Expect.equal (Ok [ [ "Foo", "Bar" ] ])
        , test "fails when no named fields" <|
            \() ->
                "\n"
                    |> Decode.decodeCsv NoFieldNames Decode.availableFields
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal (Err "Asked for available fields, but none were provided")
        ]
