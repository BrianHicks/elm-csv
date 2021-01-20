module Csv.ParserTest exposing (..)

import Csv.Parser as Parser exposing (parse)
import Expect
import Test exposing (..)


parseTest : Test
parseTest =
    describe "parse"
        [ test "a single value" <|
            \_ ->
                parse "a"
                    |> Expect.equal (Ok [ [ "a" ] ])
        , test "two fields, separated by a comma" <|
            \_ ->
                parse "a,b"
                    |> Expect.equal (Ok [ [ "a", "b" ] ])
        , test "two rows, separated by a newline" <|
            \_ ->
                parse "a\nb"
                    |> Expect.equal (Ok [ [ "a" ], [ "b" ] ])
        , test "two rows, two values" <|
            \_ ->
                parse "a,b\nc,d"
                    |> Expect.equal
                        (Ok
                            [ [ "a", "b" ]
                            , [ "c", "d" ]
                            ]
                        )
        ]
