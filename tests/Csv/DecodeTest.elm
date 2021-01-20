module Csv.DecodeTest exposing (..)

import Csv.Decode as Decode
import Expect
import Test exposing (..)


stringTest : Test
stringTest =
    describe "string"
        [ test "a blank string" <|
            \_ ->
                ""
                    |> Decode.decodeCsvString (Decode.string (Decode.column 0))
                    |> Expect.equal (Ok [ "" ])
        , todo "a unquoted value"
        , todo "an integer"
        ]
