module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)


main : BenchmarkProgram
main =
    program (describe "elm-csv" [])
