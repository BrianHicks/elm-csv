module Csv.EncodeTest exposing (..)

import Csv.Encode as Encode
import Expect
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
                                    [ "ID", "Name", "Species" ]
                                    (\{ id, name, species } ->
                                        [ ( "ID", String.fromInt id )
                                        , ( "Name", name )
                                        , ( "Species", species )
                                        ]
                                    )
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "ID,Name,Species\u{000D}\n1,Atlas,cat\u{000D}\n2,Axel,puffin"
            , test "encodes with field names even when the ordering does not match" <|
                \_ ->
                    pets
                        |> Encode.encode
                            { encoder =
                                Encode.withFieldNames
                                    [ "ID", "Name", "Species" ]
                                    (\{ id, name, species } ->
                                        [ ( "Species", species )
                                        , ( "ID", String.fromInt id )
                                        , ( "Name", name )
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
                                    [ "ID", "Name", "Species" ]
                                    (\{ id, species } ->
                                        [ ( "ID", String.fromInt id )
                                        , ( "Species", species )
                                        ]
                                    )
                            , fieldSeparator = ','
                            }
                        |> Expect.equal "ID,Name,Species\u{000D}\n1,,cat\u{000D}\n2,,puffin"
            , test "uses the correct separator" <|
                \_ ->
                    pets
                        |> Encode.encode
                            { encoder =
                                Encode.withFieldNames
                                    [ "ID", "Name", "Species" ]
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
