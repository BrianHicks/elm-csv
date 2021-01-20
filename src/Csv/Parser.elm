module Csv.Parser exposing
    ( Config, config, ConfigProblem(..)
    , parse, Problem(..), Context(..)
    )

{-|

@docs Config, config, ConfigProblem

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


config :
    { rowSeparator : String
    , fieldSeparator : String
    }
    -> Result ConfigProblem Config
config separators =
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


type Problem
    = ExpectingRowSeparator String
    | ExpectingFieldSeparator String
    | ExpectingQuote
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
    Parser.inContext Row
        (Parser.succeed (::)
            |= fieldParser config_
            |= Parser.loop []
                (\soFar ->
                    Parser.oneOf
                        [ -- if we see a field separator, it MUST be followed
                          -- by a field
                          Parser.succeed (\field -> Parser.Loop (field :: soFar))
                            |. Parser.token config_.fieldSeparator
                            |= fieldParser config_
                        , -- if the row is done, get out!
                          Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
                            |= Parser.oneOf
                                [ Parser.end ExpectingEnd
                                , Parser.token config_.rowSeparator
                                ]
                        ]
                )
        )


fieldParser : InternalConfig -> Parser Context Problem String
fieldParser config_ =
    Parser.inContext Field <|
        Parser.oneOf
            [ Parser.succeed identity
                |. Parser.token quote
                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
                |. Parser.token quote
            , Parser.chompWhile (\c -> c /= config_.newFieldIndicator && c /= config_.newRowIndicator)
                |> Parser.getChompedString
            ]


quote : Parser.Token Problem
quote =
    Parser.Token "\"" ExpectingQuote
