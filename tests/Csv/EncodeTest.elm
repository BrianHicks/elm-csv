module Csv.EncodeTest exposing (..)

import Csv.Encode as Encode
import Csv.Parser as Parser
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


encodeTest : Test
encodeTest =
    let
        pets : List { id : Int, name : String, species : String }
        pets =
            [ { id = 1, name = "Atlas", species = "cat" }
            , { id = 2, name = "Axel", species = "puffin" }
            ]
    in
    describe "encode"
        [ describe "without field names" <|
            [ test "encodes without field names" <|
                \_ ->
                    pets
                        |> Encode.encode
                            { encoder =
                                Encode.withoutFieldNames
                                    (\{ id, name, species } ->
                                        [ String.fromInt id
                                        , name
                                        , species
                                        ]
                                    )
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "1,Atlas,cat\u{000D}\n2,Axel,puffin"
            , test "escapes quotes" <|
                \_ ->
                    [ "\"" ]
                        |> Encode.encode
                            { encoder = Encode.withoutFieldNames List.singleton
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "\"\"\"\""
            , test "escapes field separators (comma)" <|
                \_ ->
                    [ "," ]
                        |> Encode.encode
                            { encoder = Encode.withoutFieldNames List.singleton
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "\",\""
            , test "escapes field separators (semicolon)" <|
                \_ ->
                    [ ";" ]
                        |> Encode.encode
                            { encoder = Encode.withoutFieldNames List.singleton
                            , fieldSeparator = ';'
                            }
                        |> Expect.equal "\";\""
            , test "escapes newlines" <|
                \_ ->
                    [ "\n" ]
                        |> Encode.encode
                            { encoder = Encode.withoutFieldNames List.singleton
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "\"\n\""
            , test "escapes row separators" <|
                \_ ->
                    [ "\u{000D}\n" ]
                        |> Encode.encode
                            { encoder = Encode.withoutFieldNames List.singleton
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "\"\u{000D}\n\""
            ]
        , describe "with field names"
            [ test "encodes with field names" <|
                \_ ->
                    pets
                        |> Encode.encode
                            { encoder =
                                Encode.withFieldNames
                                    (\{ id, name, species } ->
                                        [ ( "ID", String.fromInt id )
                                        , ( "Name", name )
                                        , ( "Species", species )
                                        ]
                                    )
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "ID,Name,Species\u{000D}\n1,Atlas,cat\u{000D}\n2,Axel,puffin"
            , test "uses a blank character when a field name is missing" <|
                \_ ->
                    pets
                        |> Encode.encode
                            { encoder =
                                Encode.withFieldNames
                                    (\{ id, name, species } ->
                                        if id == 1 then
                                            [ ( "ID", String.fromInt id )
                                            , ( "Species", species )
                                            ]

                                        else
                                            [ ( "ID", String.fromInt id )
                                            , ( "Name", name )
                                            , ( "Species", species )
                                            ]
                                    )
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "ID,Name,Species\u{000D}\n1,,cat\u{000D}\n2,Axel,puffin"
            , test "uses the average of the field position when fields are sorted differently in different rows" <|
                \_ ->
                    [ { id = "a", name = "z" }
                    , { id = "z", name = "a" }
                    ]
                        |> Encode.encode
                            { encoder =
                                Encode.withFieldNames
                                    (\{ id, name } ->
                                        List.sortBy Tuple.second
                                            [ ( "ID", id )
                                            , ( "Name", name )
                                            ]
                                    )
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "ID,Name\u{000D}\na,z\u{000D}\nz,a"
            , test "uses the correct separator" <|
                \_ ->
                    pets
                        |> Encode.encode
                            { encoder =
                                Encode.withFieldNames
                                    (\{ id, name, species } ->
                                        [ ( "ID", String.fromInt id )
                                        , ( "Name", name )
                                        , ( "Species", species )
                                        ]
                                    )
                            , fieldSeparator = ';'
                            }
                        |> Expect.equal "ID;Name;Species\u{000D}\n1;Atlas;cat\u{000D}\n2;Axel;puffin"
            ]
        ]


roundTripTest : Test
roundTripTest =
    fuzz2 weirdCsvFuzzer fieldSeparatorFuzzer "anything we encode, we can parse" <|
        \weirdCsv fieldSeparator ->
            weirdCsv
                |> Encode.encode
                    { encoder = Encode.withoutFieldNames identity
                    , fieldSeparator = fieldSeparator
                    }
                |> Parser.parse { fieldSeparator = fieldSeparator }
                |> Expect.equal (Ok weirdCsv)


weirdCsvFuzzer : Fuzzer (List (List String))
weirdCsvFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "a"
        , Fuzz.constant "b "
        , Fuzz.constant "\""
        , Fuzz.constant ","
        , Fuzz.constant ";"
        , Fuzz.constant "\n"
        , Fuzz.constant "\u{000D}"
        ]
        |> nonEmptyList
        |> Fuzz.map String.concat
        |> nonEmptyList
        |> shortList


fieldSeparatorFuzzer : Fuzzer Char
fieldSeparatorFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant ','
        , Fuzz.constant ';'
        , Fuzz.constant '\t'
        ]


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList fuzzer =
    Fuzz.map2 (::) fuzzer (shortList fuzzer)


shortList : Fuzzer a -> Fuzzer (List a)
shortList fuzzer =
    Fuzz.oneOf
        [ Fuzz.constant []
        , Fuzz.map List.singleton fuzzer
        , Fuzz.map2 (\a b -> [ a, b ]) fuzzer fuzzer
        ]
