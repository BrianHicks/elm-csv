module ConsVsArray exposing (..)

import Array
import Benchmark exposing (benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)


main : BenchmarkProgram
main =
    let
        targetSize =
            10

        items =
            List.range 1 targetSize
    in
    describe "collection construction"
        [ benchmark "List (::)" (\_ -> List.foldl (::) [] items)
        , benchmark "List (::) |> List.reverse" (\_ -> List.foldl (::) [] items |> List.reverse)
        , benchmark "List (::) |> Array.fromList" (\_ -> List.foldl (::) [] items |> Array.fromList)
        , benchmark "Array.push" (\_ -> List.foldl Array.push Array.empty items)
        , let
            premade =
                Array.initialize targetSize identity
          in
          benchmark "Array.set (shared)"
            (\_ ->
                List.foldl
                    (\item ( soFar, index ) ->
                        ( Array.set index item soFar
                        , index + 1
                        )
                    )
                    ( premade, 0 )
                    items
            )
        , benchmark "Array.set"
            (\_ ->
                List.foldl
                    (\item ( soFar, index ) ->
                        ( Array.set index item soFar
                        , index + 1
                        )
                    )
                    ( Array.initialize targetSize identity, 0 )
                    items
            )
        ]
        |> program
