module Csv.ParserTest exposing (..)

import Csv.Parser as Parser exposing (parse)
import Expect exposing (Expectation)
import Test exposing (..)


parseTest : Test
parseTest =
    let
        configurations : List ( String, { rowSeparator : String, fieldSeparator : Char } )
        configurations =
            [ ( "CRLF CSV (US locale)"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = ','
                }
              )
            , ( "CRLF CSV (EU locale, semicolon)"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = ';'
                }
              )
            , ( "LF-only CSV"
              , { rowSeparator = "\n"
                , fieldSeparator = ','
                }
              )
            , ( "CRLF TSV"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = '\t'
                }
              )
            , ( "LF-only TSV"
              , { rowSeparator = "\n"
                , fieldSeparator = '\t'
                }
              )
            ]
    in
    configurations
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
                    , test "two rows of two fields" <|
                        \_ ->
                            expectRoundTrip config
                                [ [ "a", "b" ]
                                , [ "c", "d" ]
                                ]
                    , test "blank fields" <|
                        \_ ->
                            expectRoundTrip config
                                [ [ "", "", "" ] ]
                    , test "only half of a row separator" <|
                        \_ ->
                            case String.uncons config.rowSeparator of
                                Nothing ->
                                    -- really shouldn't ever happen but
                                    -- we'll let it slide here since it's
                                    -- caught in other places.
                                    Expect.pass

                                Just ( _, "" ) ->
                                    -- not relevant here
                                    Expect.pass

                                Just ( first, _ ) ->
                                    String.fromList [ first ]
                                        |> parse { fieldSeparator = config.fieldSeparator }
                                        |> Expect.equal (Ok [ [ String.fromList [ first ] ] ])
                    , describe "quoted values"
                        [ test "quoted single values" <|
                            \_ ->
                                "\"a\""
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ "a" ] ])
                        , test "quoted row separators" <|
                            \_ ->
                                ("\"" ++ config.rowSeparator ++ "\"")
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ config.rowSeparator ] ])
                        , test "quoted field separators" <|
                            \_ ->
                                ("\"" ++ String.fromChar config.fieldSeparator ++ "\"")
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ String.fromChar config.fieldSeparator ] ])
                        , test "quoted quotes" <|
                            \_ ->
                                "\"\"\"\""
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ "\"" ] ])
                        , test "two quoted values in a row" <|
                            \_ ->
                                ("\"a\"" ++ String.fromChar config.fieldSeparator ++ "\"b\"")
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ "a", "b" ] ])
                        , test "two rows with quoted values" <|
                            \_ ->
                                ("\"a\"" ++ config.rowSeparator ++ "\"b\"")
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ "a" ], [ "b" ] ])
                        , test "a trailing newline should be ignored" <|
                            -- https://github.com/BrianHicks/elm-csv/issues/8
                            \_ ->
                                (encode config
                                    [ [ "Country", "Population" ]
                                    , [ "Agentina", "44361150" ]
                                    , [ "Brazil", "212652000" ]
                                    ]
                                    ++ config.rowSeparator
                                )
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal
                                        (Ok
                                            [ [ "Country", "Population" ]
                                            , [ "Agentina", "44361150" ]
                                            , [ "Brazil", "212652000" ]
                                            ]
                                        )
                        , test "a trailing newline after a quoted field should be ignored" <|
                            -- https://github.com/BrianHicks/elm-csv/issues/24
                            \_ ->
                                ("\"val\"" ++ config.rowSeparator)
                                    |> parse { fieldSeparator = config.fieldSeparator }
                                    |> Expect.equal (Ok [ [ "val" ] ])
                        , describe "errors"
                            [ test "not ending a quoted value is an error" <|
                                \_ ->
                                    "\"a"
                                        |> parse { fieldSeparator = config.fieldSeparator }
                                        |> Expect.equal (Err (Parser.SourceEndedWithoutClosingQuote 1))
                            , test "additional characters after the closing quote but before the field separator is an error" <|
                                \_ ->
                                    ("\"a\"b" ++ config.rowSeparator)
                                        |> parse { fieldSeparator = config.fieldSeparator }
                                        |> Expect.equal (Err (Parser.AdditionalCharactersAfterClosingQuote 1))
                            , test "additional characters after the closing quote but before the row separator is an error" <|
                                \_ ->
                                    ("\"a\"b" ++ String.fromChar config.fieldSeparator)
                                        |> parse { fieldSeparator = config.fieldSeparator }
                                        |> Expect.equal (Err (Parser.AdditionalCharactersAfterClosingQuote 1))
                            ]
                        ]
                    ]
            )
        |> describe "parse"


expectRoundTrip : { rowSeparator : String, fieldSeparator : Char } -> List (List String) -> Expectation
expectRoundTrip config rows =
    encode config rows
        |> parse { fieldSeparator = config.fieldSeparator }
        |> Expect.equal (Ok rows)


encode : { rowSeparator : String, fieldSeparator : Char } -> List (List String) -> String
encode config rows =
    rows
        |> List.map (String.join (String.fromChar config.fieldSeparator))
        |> String.join config.rowSeparator
