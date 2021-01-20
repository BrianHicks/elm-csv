module Csv.ParserTest exposing (..)

import Csv.Parser as Parser exposing (parse)
import Expect exposing (Expectation)
import Test exposing (..)


configTest : Test
configTest =
    describe "config"
        [ test "valid config" <|
            \_ ->
                Parser.config
                    { rowSeparator = "\n"
                    , fieldSeparator = ","
                    }
                    |> Expect.ok
        , test "blank row separator" <|
            \_ ->
                Parser.config
                    { rowSeparator = ""
                    , fieldSeparator = ","
                    }
                    |> Expect.equal (Err Parser.NeedNonBlankRowSeparator)
        , test "blank field separator" <|
            \_ ->
                Parser.config
                    { rowSeparator = "\n"
                    , fieldSeparator = ""
                    }
                    |> Expect.equal (Err Parser.NeedNonBlankFieldSeparator)
        ]


parseTest : Test
parseTest =
    let
        configurations =
            [ ( "classic CSV"
              , { rowSeparator = "\n"
                , fieldSeparator = ","
                }
              )

            -- , ( "CRLF CSV"
            --   , { rowSeparator = "\u{000D}\n"
            --     , fieldSeparator = ","
            --     }
            --   )
            , ( "classic TSV"
              , { rowSeparator = "\n"
                , fieldSeparator = "\t"
                }
              )
            ]
    in
    describe "parse"
        [ configurations
            |> List.map
                (\( description, config ) ->
                    describe description
                        [ test "a single value" <|
                            \_ ->
                                expectRoundTrip config
                                    [ [ "a" ] ]
                        , test "two fields" <|
                            \_ ->
                                expectRoundTrip config
                                    [ [ "a", "b" ] ]
                        , test "two rows" <|
                            \_ ->
                                expectRoundTrip config
                                    [ [ "a" ]
                                    , [ "b" ]
                                    ]
                        , test "two rows of  two fields" <|
                            \_ ->
                                expectRoundTrip config
                                    [ [ "a", "b" ]
                                    , [ "c", "d" ]
                                    ]
                        , test "blank fields" <|
                            \_ ->
                                expectRoundTrip config
                                    [ [ "", "", "" ] ]
                        ]
                )
            |> describe "with a valid configuration"
        ]


expectRoundTrip : { rowSeparator : String, fieldSeparator : String } -> List (List String) -> Expectation
expectRoundTrip separators rows =
    case Parser.config separators of
        Ok config ->
            rows
                |> List.map (String.join separators.fieldSeparator)
                |> String.join separators.rowSeparator
                |> parse config
                |> Expect.equal (Ok rows)

        otherwise ->
            Expect.ok otherwise
