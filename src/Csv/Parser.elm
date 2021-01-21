module Csv.Parser exposing
    ( Config, customConfig, ConfigProblem(..)
    , parse, Problem(..), Context(..)
    )

{-|

@docs Config, customConfig, ConfigProblem

@docs parse, Problem, Context

-}

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type Config
    = Config InternalConfig


type alias InternalConfig =
    { rowSeparator : Parser.Token Problem
    , newRowIndicator : Char
    , fieldSeparator : Parser.Token Problem
    , newFieldIndicator : Char
    }


type ConfigProblem
    = NeedNonBlankRowSeparator
    | NeedNonBlankFieldSeparator


customConfig :
    { rowSeparator : String
    , fieldSeparator : String
    }
    -> Result ConfigProblem Config
customConfig separators =
    case ( String.uncons separators.rowSeparator, String.uncons separators.fieldSeparator ) of
        ( Just ( newRowIndicator, _ ), Just ( newFieldIndicator, _ ) ) ->
            (Ok << Config)
                { rowSeparator = Parser.Token separators.rowSeparator (ExpectingRowSeparator separators.rowSeparator)
                , newRowIndicator = newRowIndicator
                , fieldSeparator = Parser.Token separators.fieldSeparator (ExpectingFieldSeparator separators.fieldSeparator)
                , newFieldIndicator = newFieldIndicator
                }

        ( Nothing, _ ) ->
            Err NeedNonBlankRowSeparator

        ( _, Nothing ) ->
            Err NeedNonBlankFieldSeparator


parse : Config -> String -> Result (List (Parser.DeadEnd Context Problem)) (List (List String))
parse (Config internalConfig) =
    Parser.run (parser internalConfig)


type Context
    = Row
    | Field
    | QuotedField


type Problem
    = ExpectingRowSeparator String
    | ExpectingFieldSeparator String
    | ExpectingStartOfRowSeparator Char
    | ExpectingStartOfFieldSeparator Char
    | ExpectingQuote
    | ExpectingEscapedQuote
    | ExpectingEnd


parser : InternalConfig -> Parser Context Problem (List (List String))
parser config_ =
    Parser.loop [] <|
        \soFar ->
            Parser.oneOf
                [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
                    |= Parser.end ExpectingEnd
                , Parser.succeed (\row -> Parser.Loop (row :: soFar))
                    |= rowParser config_
                ]


rowParser : InternalConfig -> Parser Context Problem (List String)
rowParser config_ =
    Parser.inContext Row <|
        Parser.loop [] <|
            \fields ->
                Parser.map
                    (\( field, afterField ) ->
                        case afterField of
                            AnotherField ->
                                Parser.Loop (field :: fields)

                            NoMore ->
                                Parser.Done (List.reverse (field :: fields))
                    )
                    (fieldParser config_)


type AfterField
    = AnotherField
    | NoMore


afterFieldParser : InternalConfig -> Parser Context Problem AfterField
afterFieldParser config_ =
    Parser.oneOf
        [ Parser.map (\_ -> AnotherField) (Parser.token config_.fieldSeparator)
        , Parser.map (\_ -> NoMore) (Parser.token config_.rowSeparator)
        , Parser.map (\_ -> NoMore) (Parser.end ExpectingEnd)
        ]


fieldParser : InternalConfig -> Parser Context Problem ( String, AfterField )
fieldParser config_ =
    Parser.oneOf
        [ quotedFieldParser config_
        , unquotedFieldParser config_
        ]


quotedFieldParser : InternalConfig -> Parser Context Problem ( String, AfterField )
quotedFieldParser config_ =
    Parser.inContext QuotedField <|
        Parser.succeed Tuple.pair
            |. Parser.token quote
            |= Parser.loop []
                (\soFar ->
                    Parser.oneOf
                        [ Parser.succeed (\() -> Parser.Loop ("\"" :: soFar))
                            |= Parser.token escapedQuote
                        , Parser.succeed (\() -> Parser.Done (String.concat (List.reverse soFar)))
                            |= Parser.token quote
                        , Parser.succeed (\segment -> Parser.Loop (segment :: soFar))
                            |= Parser.getChompedString (Parser.chompUntil quote)
                        ]
                )
            |= afterFieldParser config_


unquotedFieldParser : InternalConfig -> Parser Context Problem ( String, AfterField )
unquotedFieldParser config_ =
    Parser.inContext Field <|
        Parser.loop [] <|
            \segments ->
                Parser.oneOf
                    [ Parser.succeed
                        (\afterField ->
                            Parser.Done
                                ( String.concat (List.reverse segments)
                                , afterField
                                )
                        )
                        |= afterFieldParser config_
                    , Parser.succeed (\segment -> Parser.Loop (segment :: segments))
                        |= Parser.oneOf
                            [ Parser.chompIf
                                (\c -> c == config_.newRowIndicator)
                                (ExpectingStartOfRowSeparator config_.newRowIndicator)
                                |> Parser.getChompedString
                            , Parser.chompIf
                                (\c -> c == config_.newFieldIndicator)
                                (ExpectingStartOfFieldSeparator config_.newFieldIndicator)
                                |> Parser.getChompedString
                            , Parser.chompWhile
                                (\c -> c /= config_.newFieldIndicator && c /= config_.newRowIndicator)
                                |> Parser.getChompedString
                            ]
                    ]


quote : Parser.Token Problem
quote =
    Parser.Token "\"" ExpectingQuote


escapedQuote : Parser.Token Problem
escapedQuote =
    Parser.Token "\"\"" ExpectingEscapedQuote
