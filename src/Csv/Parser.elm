module Csv.Parser exposing
    ( Config, customConfig, ConfigProblem(..)
    , parse
    )

{-|

@docs Config, customConfig, ConfigProblem

@docs parse

-}

import Parser exposing ((|.), (|=), Parser)


type Config
    = Config InternalConfig


type alias InternalConfig =
    { rowSeparator : String
    , newRowIndicator : Char
    , fieldSeparator : String
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
                { rowSeparator = separators.rowSeparator
                , newRowIndicator = newRowIndicator
                , fieldSeparator = separators.fieldSeparator
                , newFieldIndicator = newFieldIndicator
                }

        ( Nothing, _ ) ->
            Err NeedNonBlankRowSeparator

        ( _, Nothing ) ->
            Err NeedNonBlankFieldSeparator


parse : Config -> String -> Result (List Parser.DeadEnd) (List (List String))
parse (Config internalConfig) =
    Parser.run (parser internalConfig)


parser : InternalConfig -> Parser (List (List String))
parser config_ =
    Parser.loop [] <|
        \soFar ->
            Parser.oneOf
                [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
                    |= Parser.end
                , Parser.succeed (\row -> Parser.Loop (row :: soFar))
                    |= rowParser config_
                ]


rowParser : InternalConfig -> Parser (List String)
rowParser config_ =
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


afterFieldParser : InternalConfig -> Parser AfterField
afterFieldParser config_ =
    Parser.oneOf
        [ Parser.map (\_ -> AnotherField) (Parser.token config_.fieldSeparator)
        , Parser.map (\_ -> NoMore) (Parser.token config_.rowSeparator)
        , Parser.map (\_ -> NoMore) Parser.end
        ]


fieldParser : InternalConfig -> Parser ( String, AfterField )
fieldParser config_ =
    Parser.oneOf
        [ quotedFieldParser config_
        , unquotedFieldParser config_
        ]


quotedFieldParser : InternalConfig -> Parser ( String, AfterField )
quotedFieldParser config_ =
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


unquotedFieldParser : InternalConfig -> Parser ( String, AfterField )
unquotedFieldParser config_ =
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
                        [ Parser.chompIf (\c -> c == config_.newRowIndicator)
                            |> Parser.getChompedString
                        , Parser.chompIf (\c -> c == config_.newFieldIndicator)
                            |> Parser.getChompedString
                        , Parser.chompWhile
                            (\c -> c /= config_.newFieldIndicator && c /= config_.newRowIndicator)
                            |> Parser.getChompedString
                        ]
                ]


quote : String
quote =
    "\""


escapedQuote : String
escapedQuote =
    "\"\""
