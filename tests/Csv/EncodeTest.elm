module Csv.EncodeTest exposing (..)

import Csv.Encode as Encode
import Expect
import Test exposing (..)


encodeTest : Test
encodeTest =
    describe "encode"
        [ describe "with field names"
            [ test "encodes with field names" <|
                \_ ->
                    [ { id = 1, name = "Atlas", species = "cat" }
                    , { id = 2, name = "Axel", species = "puffin" }
                    ]
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
                    [ { id = 1, name = "Atlas", species = "cat" }
                    , { id = 2, name = "Axel", species = "puffin" }
                    ]
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
                    [ { id = 1, name = "Atlas", species = "cat" }
                    , { id = 2, name = "Axel", species = "puffin" }
                    ]
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
            ]
        ]
