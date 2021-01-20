module Csv.Parser exposing (parse)

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


parse : String -> Result (List (Parser.DeadEnd Context Problem)) (List (List String))
parse =
    Parser.run parser


type Context
    = Row
    | Field


type Problem
    = ExpectingRowSeparator
    | ExpectingFieldSeparator
    | ExpectingEnd


parser : Parser Context Problem (List (List String))
parser =
    Parser.loop [] <|
        \soFar ->
            Parser.oneOf
                [ Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
                    |= Parser.end ExpectingEnd
                , Parser.succeed (\row -> Parser.Loop (row :: soFar))
                    |= rowParser
                ]


rowParser : Parser Context Problem (List String)
rowParser =
    Parser.inContext Row <|
        Parser.loop [] <|
            \soFar ->
                Parser.oneOf
                    [ -- if the row is done, get out!
                      Parser.succeed (\_ -> Parser.Done (List.reverse soFar))
                        |= Parser.oneOf
                            [ Parser.end ExpectingEnd
                            , Parser.token (Parser.Token "\n" ExpectingRowSeparator)
                            ]
                    , -- if we see a field separator, it MUST be followed
                      -- by a field
                      Parser.succeed (\field -> Parser.Loop (field :: soFar))
                        |. Parser.token (Parser.Token "," ExpectingFieldSeparator)
                        |= fieldParser
                    , -- ... except at the beginning of the row, where there
                      -- must not be a separator.
                      Parser.succeed (\field -> Parser.Loop (field :: soFar))
                        |= fieldParser
                    ]


fieldParser : Parser Context Problem String
fieldParser =
    Parser.inContext Field <|
        Parser.oneOf
            [ Parser.chompWhile (\c -> c /= ',' && c /= '\n')
                |> Parser.getChompedString
            ]
