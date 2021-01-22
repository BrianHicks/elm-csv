module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Csv.Parser as Parser


naiveParser : String -> List (List String)
naiveParser =
    String.split "\u{000D}\n" >> List.map (String.split ",")


triplesCsv : Int -> String
triplesCsv howManyRows =
    List.range 0 howManyRows
        |> List.map (String.fromInt >> List.repeat 3 >> String.join ",")
        |> String.join "\u{000D}\n"


naiveAgainstCsvParser : Int -> Benchmark
naiveAgainstCsvParser howManyRows =
    let
        csv =
            triplesCsv howManyRows

        config =
            case
                Parser.config
                    { rowSeparator = "\u{000D}\n"
                    , fieldSeparator = ","
                    }
            of
                Ok config_ ->
                    config_

                Err problem ->
                    crashButWithoutDependingOnDebug ()
    in
    Benchmark.compare "naive against Csv.Parser"
        "naive"
        (\_ -> naiveParser csv)
        "Csv.Parser"
        (\_ -> Parser.parse config csv)


crashButWithoutDependingOnDebug : () -> a
crashButWithoutDependingOnDebug _ =
    crashButWithoutDependingOnDebug ()


main : BenchmarkProgram
main =
    program (describe "elm-csv" [ naiveAgainstCsvParser 100 ])
