module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Csv.Parser as Parser


stringSplittingParser : String -> List (List String)
stringSplittingParser =
    String.split "\u{000D}\n" >> List.map (String.split ",")


encodeCsv : Int -> String
encodeCsv howManyRows =
    List.range 0 (howManyRows - 1)
        |> List.map (\_ -> String.join "," (List.repeat 5 "a"))
        |> String.join "\u{000D}\n"


crashButWithoutDependingOnDebug : () -> a
crashButWithoutDependingOnDebug _ =
    crashButWithoutDependingOnDebug ()


main : BenchmarkProgram
main =
    let
        config =
            case Parser.config { rowSeparator = "\u{000D}\n", fieldSeparator = "," } of
                Ok config_ ->
                    config_

                Err _ ->
                    crashButWithoutDependingOnDebug ()
    in
    [ 0, 1, 2, 4, 8 ]
        |> List.map
            (\size ->
                let
                    csv =
                        encodeCsv size
                in
                Benchmark.compare (String.fromInt size ++ " rows")
                    "String.split"
                    (\_ -> stringSplittingParser csv)
                    "Csv.Parser.parse"
                    (\_ -> Parser.parse config csv)
            )
        |> describe "elm-csv"
        |> program
