module Csv.Decode exposing
    ( Decoder, string, int, float
    , column
    , decodeString, Error, errorToString, Problem(..)
    , map, map2, map3
    , succeed, fail, andThen
    )

{-|

@docs Decoder, string, int, float

@docs column

@docs decodeString, Error, errorToString, Problem

@docs map, map2, map3

@docs succeed, fail, andThen

-}

import Csv.Parser as Parser
import Parser.Advanced



-- PRIMITIVES


type Decoder a
    = Decoder (List String -> Result Problem a)


{-| Decode a string from a CSV.

    decodeString string "a" --> Ok [ "a" ]

Unless you specify otherwise (e.g. with `column`) this will assume there is
only one column in the CSV and try to decode that.

    decodeString string "a,b" --> Err { row = 0, problem = AmbiguousColumn }

-}
string : Decoder String
string =
    Decoder (getOnly Ok)


{-| Decode an integer from a CSV.

    decodeString int "1" --> Ok [ 1 ]

    decodeString int "-1" --> Ok [ -1 ]

    decodeString int "volcano"
    --> Err { row = 0, problem = ExpectedInt "volcano" }

Unless you specify otherwise (e.g. with `column`) this will assume there is
only one column in the CSV and try to decode that.

    decodeString int "1,2"
    --> Err { row = 0, problem = AmbiguousColumn }

-}
int : Decoder Int
int =
    Decoder
        (getOnly
            (\value ->
                case String.toInt value of
                    Just parsed ->
                        Ok parsed

                    Nothing ->
                        Err (ExpectedInt value)
            )
        )


{-| Decode an integer from a CSV.

    decodeString float "3" --> Ok [ 3.0 ]

    decodeString float "3.14" --> Ok [ 3.14 ]

    decodeString float "mimesis"
    --> Err { row = 0, problem = ExpectedFloat "mimesis" }

Unless you specify otherwise (e.g. with `column`) this will assume there is
only one column in the CSV and try to decode that.

    decodeString float "1.0,2.0"
    --> Err { row = 0, problem = AmbiguousColumn }

-}
float : Decoder Float
float =
    Decoder
        (getOnly
            (\value ->
                case String.toFloat value of
                    Just parsed ->
                        Ok parsed

                    Nothing ->
                        Err (ExpectedFloat value)
            )
        )


getOnly : (String -> Result Problem a) -> List String -> Result Problem a
getOnly transform row =
    case row of
        [] ->
            Err (ExpectedColumn 0)

        [ only ] ->
            transform only

        _ ->
            Err AmbiguousColumn



-- LOCATIONS
-- TODO: field


{-| Parse a value at a given column in the CSV.

    decodeString (column 0 string) "Argentina" --> Ok [ "Argentina" ]

    decodeString (column 1 int) "1,2,3"--> Ok [ 2 ]

    decodeString (column 100 float) "3.14"
    --> Err { row = 0, problem = ExpectedColumn 100 }

-}
column : Int -> Decoder a -> Decoder a
column col (Decoder decoder) =
    Decoder <|
        \row ->
            case row |> List.drop col |> List.head of
                Just value ->
                    decoder [ value ]

                Nothing ->
                    Err (ExpectedColumn col)



-- RUN DECODERS


{-| Convert a String into some type you care about using the `Decoder`s in
this module!
-}
decodeString : Decoder a -> String -> Result Error (List a)
decodeString (Decoder decode) source =
    case Parser.parse Parser.crlfCsvConfig source of
        Ok rows ->
            rows
                |> List.foldl
                    (\row ->
                        Result.andThen
                            (\( soFar, rowNum ) ->
                                case decode row of
                                    Ok val ->
                                        Ok ( val :: soFar, rowNum - 1 )

                                    Err problem ->
                                        Err { row = rowNum, problem = problem }
                            )
                    )
                    (Ok ( [], 0 ))
                |> Result.map (Tuple.first >> List.reverse)

        Err _ ->
            -- TODO: really punting on error message quality here but we'll
            -- get back to it!
            Err { row = 0, problem = ParsingProblem }


{-| Sometimes things go wrong. This record lets you know where exactly that
happened so you can go fix it.
-}
type alias Error =
    { problem : Problem
    , row : Int
    }


{-| What, exactly, went wrong?
-}
type Problem
    = ParsingProblem
    | ExpectedColumn Int
    | AmbiguousColumn
    | ExpectedInt String
    | ExpectedFloat String
    | Failure String


{-| Want an easier-to-read version of an `Error`? Here we are!
-}
errorToString : Error -> String
errorToString _ =
    "TODO"



-- MAPPING


{-| Decode a value, then transform it before returning.

    decodeString (map (\i -> i * 2) int) "15" --> Ok [ 30 ]

    decodeString (map String.reverse string) "slap" --> Ok [ "pals" ]

-}
map : (from -> to) -> Decoder from -> Decoder to
map transform (Decoder decoder) =
    Decoder (decoder >> Result.map transform)


{-| Combine two decoders to make something else, for example a record:

    decodeString
        (map2 (\id name -> { id = id, name = name })
            (column 0 int)
            (column 1 string)
        )
        "1,Atlas"
        --> Ok [ { id = 1, name = "Atlas" } ]

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 transform (Decoder decodeA) (Decoder decodeB) =
    Decoder
        (\row ->
            Result.map2 transform
                (decodeA row)
                (decodeB row)
        )


{-| Like `map2`, but with three decoders.
-}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 transform (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) =
    Decoder
        (\row ->
            Result.map3 transform
                (decodeA row)
                (decodeB row)
                (decodeC row)
        )



-- map4, map5, map6, map7, map8 or maybe pipelines?
-- FANCY DECODING


{-| Always succeed, no matter what. Mostly useful with `andThen` (see that
example.)
-}
succeed : a -> Decoder a
succeed value =
    Decoder (\_ -> Ok value)


{-| Always fail with the given message, no matter what. Mostly useful with
`andThen` (see that example.)
-}
fail : String -> Decoder a
fail message =
    Decoder (\_ -> Err (Failure message))


{-| Decode some value, and then make a decoding decision based on the
outcome. For example, if you want to parse an integer without using `int`,
you might do this:

    myInt : Decoder Int
    myInt =
        string
            |> andThen
                (\rawInt ->
                    case String.toInt rawInt of
                        Just parsedInt ->
                            Decode.succeed parsedInt

                        Nothing ->
                            Decode.fail "Hey, that's not an int!"
                )

You could then use it like this:

    decodeString myInt "1" -- Ok [ 1 ]

    decodeString myInt "fruit"
    -- Err { row = 0, problem = Failure "Hey, that's not an int!" }

-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen next (Decoder first) =
    Decoder
        (\row ->
            first row
                |> Result.andThen
                    (\nextValue ->
                        let
                            (Decoder final) =
                                next nextValue
                        in
                        final row
                    )
        )
