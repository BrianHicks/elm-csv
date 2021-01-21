module Csv.ParserTest exposing (..)

import Csv.Parser as Parser exposing (parse)
import Expect exposing (Expectation)
import Test exposing (..)


configTest : Test
configTest =
    describe "config"
        [ test "valid config" <|
            \_ ->
                Parser.customConfig
                    { rowSeparator = "\n"
                    , fieldSeparator = ","
                    }
                    |> Expect.ok
        , test "blank row separator" <|
            \_ ->
                Parser.customConfig
                    { rowSeparator = ""
                    , fieldSeparator = ","
                    }
                    |> Expect.equal (Err Parser.NeedNonBlankRowSeparator)
        , test "blank field separator" <|
            \_ ->
                Parser.customConfig
                    { rowSeparator = "\n"
                    , fieldSeparator = ""
                    }
                    |> Expect.equal (Err Parser.NeedNonBlankFieldSeparator)
        ]


parseTest : Test
parseTest =
    let
        configurations =
            [ ( "CRLF CSV"
              , { rowSeparator = "\u{000D}\n"
                , fieldSeparator = ","
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

            -- this one doesn't really exist in the real world, but we want to
            -- make sure that it will work if someone does need it.
            , ( "LF-only Double-bar separated values"
              , { rowSeparator = "\n"
                , fieldSeparator = "||"
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
                        ]
                    ]
            )
        |> describe "parse"


expectRoundTrip : { rowSeparator : String, fieldSeparator : String } -> List (List String) -> Expectation
expectRoundTrip separators rows =
    case Parser.customConfig separators of
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
    case Parser.customConfig separators of
        Ok config ->
            config

        Err problem ->
            Debug.todo (Debug.toString problem)
