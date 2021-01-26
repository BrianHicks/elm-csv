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
        configurations : List ( String, { rowSeparator : String, fieldSeparator : String } )
        configurations =
            [ ( "CRLF CSV (US locale)"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = ","
                }
              )
            , ( "CRLF CSV (EU locale, semicolon)"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = ";"
                }
              )
            , ( "LF-only CSV"
              , { rowSeparator = "\n"
                , fieldSeparator = ","
                }
              )
            , ( "CRLF TSV"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = "\t"
                }
              )
            , ( "LF-only TSV"
              , { rowSeparator = "\n"
                , fieldSeparator = "\t"
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

                                Just ( first, "" ) ->
                                    -- not relevant here
                                    Expect.pass

                                Just ( first, _ ) ->
                                    String.fromList [ first ]
                                        |> parse (unsafeCustomConfig config)
                                        |> Expect.equal (Ok [ [ String.fromList [ first ] ] ])
                    , test "only half of a field separator" <|
                        \_ ->
                            case String.uncons config.fieldSeparator of
                                Nothing ->
                                    -- really shouldn't ever happen but
                                    -- we'll let it slide here since it's
                                    -- caught in other places.
                                    Expect.pass

                                Just ( first, "" ) ->
                                    -- not relevant here
                                    Expect.pass

                                Just ( first, _ ) ->
                                    String.fromList [ first ]
                                        |> parse (unsafeCustomConfig config)
                                        |> Expect.equal (Ok [ [ String.fromList [ first ] ] ])
                    , describe "quoted values"
                        [ test "quoted single values" <|
                            \_ ->
                                "\"a\""
                                    |> parse (unsafeCustomConfig config)
                                    |> Expect.equal (Ok [ [ "a" ] ])
                        , test "quoted row separators" <|
                            \_ ->
                                ("\"" ++ config.rowSeparator ++ "\"")
                                    |> parse (unsafeCustomConfig config)
                                    |> Expect.equal (Ok [ [ config.rowSeparator ] ])
                        , test "quoted field separators" <|
                            \_ ->
                                ("\"" ++ config.fieldSeparator ++ "\"")
                                    |> parse (unsafeCustomConfig config)
                                    |> Expect.equal (Ok [ [ config.fieldSeparator ] ])
                        , test "quoted quotes" <|
                            \_ ->
                                "\"\"\"\""
                                    |> parse (unsafeCustomConfig config)
                                    |> Expect.equal (Ok [ [ "\"" ] ])
                        , test "two quoted values in a row" <|
                            \_ ->
                                ("\"a\"" ++ config.fieldSeparator ++ "\"b\"")
                                    |> parse (unsafeCustomConfig config)
                                    |> Expect.equal (Ok [ [ "a", "b" ] ])
                        , test "two rows with quoted values" <|
                            \_ ->
                                ("\"a\"" ++ config.rowSeparator ++ "\"b\"")
                                    |> parse (unsafeCustomConfig config)
                                    |> Expect.equal (Ok [ [ "a", "b" ] ])
                        , describe "errors"
                            [ test "not ending a quoted value is an error" <|
                                \_ ->
                                    "\"a"
                                        |> parse (unsafeCustomConfig config)
                                        |> Expect.equal (Err (Parser.SourceEndedWithoutClosingQuote 1))
                            , test "additional characters after the closing quote but before the field separator is an error" <|
                                \_ ->
                                    ("\"a\"b" ++ config.rowSeparator)
                                        |> parse (unsafeCustomConfig config)
                                        |> Expect.equal (Err (Parser.AdditionalCharactersAfterClosingQuote 1))
                            , test "additional characters after the closing quote but before the row separator is an error" <|
                                \_ ->
                                    ("\"a\"b" ++ config.fieldSeparator)
                                        |> parse (unsafeCustomConfig config)
                                        |> Expect.equal (Err (Parser.AdditionalCharactersAfterClosingQuote 1))
                            ]
                        ]
                    ]
            )
        |> describe "parse"


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


unsafeCustomConfig : { rowSeparator : String, fieldSeparator : String } -> Parser.Config
unsafeCustomConfig separators =
    case Parser.config separators of
        Ok config ->
            config

        Err problem ->
            Debug.todo (Debug.toString problem)
