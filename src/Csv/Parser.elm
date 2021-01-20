module Csv.Parser exposing (parse)

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


parse : String -> Result (List (Parser.DeadEnd Context Problem)) (List (List String))
parse =
    Parser.run parser


type Context
    = TODOContext


type Problem
    = TODOProblem


parser : Parser Context Problem (List (List String))
parser =
    Parser.problem TODOProblem
