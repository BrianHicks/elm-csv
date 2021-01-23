module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Csv.Parser as Parser


naiveParser : String -> List (List String)
naiveParser =
    String.split "\u{000D}\n" >> List.map (String.split ",")


triplesCsv : Int -> String
triplesCsv howManyRows =
    List.range 0 (howManyRows - 1)
        |> List.map (String.fromInt >> List.repeat 3 >> String.join ",")
        |> String.join "\u{000D}\n"


naive : Benchmark
naive =
    [ 0, 1, 2, 4, 8 ]
        |> List.map
            (\size ->
                let
                    csv =
                        triplesCsv size
                in
                ( String.fromInt size ++ " rows"
                , \_ -> naiveParser csv
                )
            )
        |> Benchmark.scale "naive"


parser : Benchmark
parser =
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
                        triplesCsv size
                in
                ( String.fromInt size ++ " rows"
                , \_ -> Parser.parse config csv
                )
            )
        |> Benchmark.scale "Csv.Parser"


crashButWithoutDependingOnDebug : () -> a
crashButWithoutDependingOnDebug _ =
    crashButWithoutDependingOnDebug ()


main : BenchmarkProgram
main =
    program (describe "elm-csv" [ naive, parser ])
