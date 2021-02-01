module HowManyLookups exposing (..)

import Array
import Benchmark exposing (benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)


lookup : Int -> List a -> Maybe a
lookup index items =
    List.head (List.drop index items)


main : BenchmarkProgram
main =
    [ 1, 2, 4, 8, 16, 32 ]
        |> List.map
            (\cap ->
                let
                    sourceList =
                        List.range 1 cap

                    indexes =
                        List.range 0 (cap - 1)
                in
                Benchmark.compare (String.fromInt cap ++ " items")
                    "List"
                    (\_ -> List.map (\index -> lookup index sourceList) indexes)
                    "Array"
                    (\_ ->
                        let
                            sourceArray =
                                Array.fromList sourceList
                        in
                        List.map (\index -> Array.get index sourceArray) indexes
                    )
            )
        |> describe "lookup scaling"
        |> program
