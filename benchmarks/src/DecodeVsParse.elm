module DecodeVsParse exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Csv.Parser as Parser
import Json.Decode as Decode
import Json.Encode as Encode


encodeCsv : Int -> String
encodeCsv howManyRows =
    List.range 0 (howManyRows - 1)
        |> List.map (\_ -> String.join "," (List.repeat 5 "a"))
        |> String.join "\u{000D}\n"


encodeJson : Int -> String
encodeJson howManyRows =
    List.range 0 (howManyRows - 1)
        |> Encode.list
            (\_ ->
                List.range 0 5
                    |> Encode.list (\_ -> Encode.string "a")
            )
        |> Encode.encode 0


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
    [ 0, 1, 2, 4, 8, 16, 32 ]
        |> List.map
            (\size ->
                let
                    csv =
                        encodeCsv size

                    json =
                        encodeJson size
                in
                Benchmark.compare (String.fromInt size ++ " rows")
                    ("Json.Decode.fromString" ++ " (" ++ String.fromInt (String.length json) ++ " bytes)")
                    (\_ -> Decode.decodeString Decode.value csv)
                    ("Csv.Parser.parse" ++ " (" ++ String.fromInt (String.length csv) ++ " bytes)")
                    (\_ -> Parser.parse config csv)
            )
        |> describe "elm-csv"
        |> program
