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
      --
    | Unimplemented


parser : Parser Context Problem (List (List String))
parser =
    Parser.succeed (\field -> [ [ field ] ])
        |= fieldParser


fieldParser : Parser Context Problem String
fieldParser =
    Parser.inContext Field <|
        Parser.oneOf
            [ Parser.chompWhile (\c -> c /= ',' && c /= '\n')
                |> Parser.getChompedString
            ]
