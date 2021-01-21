module Csv.Decode exposing
    ( Decoder, string, int, float
    , column, field
    , Headers(..), decodeCsv, decodeCustom, Error(..), errorToString, Problem(..)
    , map, map2, map3, pipeline, required
    , succeed, fail, andThen
    )

{-| Decode values from CSV. A crash course on constructing decoders:

  - To decode basic values in different columns, use [`field`](#field) or
    [`column`](#column) along with [`int`](#int), [`string`](#string), or
    [`float`](#float).
  - To transform values in ways that can't fail, pass them to [`map`](#map). To
    transform them in ways that _can_ fail, pass decoders to
    [`andThen`](#andThen) and call [`succeed`](#succeed) or [`fail`](#fail)
    to handle the failure gracefully.
  - To decode simple multi-field values, pass a constructor to [`map2`](#map2)
    or [`map3`](#map3).
  - For records with more than two or three values, use [`pipeline`](#pipeline)
    and [`required`](#required).

All of those functions have examples in their documentation. Check 'em out!

@docs Decoder, string, int, float

@docs column, field

@docs Headers, decodeCsv, decodeCustom, Error, errorToString, Problem

@docs map, map2, map3, pipeline, required

@docs succeed, fail, andThen

-}

import Csv.Parser as Parser
import Dict
import Parser as ElmParser
import Parser.Advanced



-- PRIMITIVES


{-| A way to specify what kind of thing you want to decode into. For example,
if you have a `Pet` data type, you'd want a `Decoder Pet`.
-}
type Decoder a
    = Decoder (List String -> Result Problem a)


{-| Decode a string from a CSV.

    decodeCsv NoHeaders string "a" --> Ok [ "a" ]

Unless you specify otherwise (e.g. with `column`) this will assume there is
only one column in the CSV and try to decode that.

    decodeCsv NoHeaders string "a,b"
    --> Err (DecodingError { row = 0, problem = AmbiguousColumn })

-}
string : Decoder String
string =
    Decoder (getOnly Ok)


{-| Decode an integer from a CSV.

    decodeCsv NoHeaders int "1" --> Ok [ 1 ]

    decodeCsv NoHeaders int "-1" --> Ok [ -1 ]

    decodeCsv NoHeaders int "volcano"
    --> Err (DecodingError { row = 0, problem = ExpectedInt "volcano" })

Unless you specify otherwise (e.g. with `column`) this will assume there is
only one column in the CSV and try to decode that.

    decodeCsv NoHeaders int "1,2"
    --> Err (DecodingError { row = 0, problem = AmbiguousColumn })

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

    decodeCsv NoHeaders float "3" --> Ok [ 3.0 ]

    decodeCsv NoHeaders float "3.14" --> Ok [ 3.14 ]

    decodeCsv NoHeaders float "mimesis"
    --> Err (DecodingError { row = 0, problem = ExpectedFloat "mimesis" })

Unless you specify otherwise (e.g. with `column`) this will assume there is
only one column in the CSV and try to decode that.

    decodeCsv NoHeaders float "1.0,2.0"
    --> Err (DecodingError { row = 0, problem = AmbiguousColumn })

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

    decodeCsv NoHeaders (column 0 string) "Argentina" --> Ok [ "Argentina" ]

    decodeCsv NoHeaders (column 1 int) "1,2,3"--> Ok [ 2 ]

    decodeCsv NoHeaders (column 100 float) "3.14"
    --> Err (DecodingError { row = 0, problem = ExpectedColumn 100 })

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


field : String -> Decoder a -> Decoder a
field name (Decoder decoder) =
    let
        headers =
            Dict.empty
    in
    Decoder <|
        \row ->
            case Dict.get name headers |> Maybe.andThen (\col -> row |> List.drop col |> List.head) of
                Just value ->
                    decoder [ value ]

                Nothing ->
                    Err (ExpectedField name)



-- RUN DECODERS


type Headers
    = NoHeaders
    | CustomHeaders (List String)


{-| Convert a CSV string into some type you care about using the `Decoder`s
in this module!
-}
decodeCsv : Headers -> Decoder a -> String -> Result Error (List a)
decodeCsv =
    decodeCustom
        { rowSeparator = "\u{000D}\n"
        , fieldSeparator = ","
        }


{-| Convert something shaped roughly like a CSV. For example, to decode
tab-separated values where the row separator is just a newline character:

    decodeCustom
        { rowSeparator = "\n"
        , fieldSeparator = "\t"
        }
        NoHeaders
        (map2 Tuple.pair
            (column 0 int)
            (column 1 string)
        )
        "1\tBrian\n2\tAtlas"
        --> Ok [ ( 1, "Brian" ), ( 2, "Atlas" ) ]

-}
decodeCustom : { rowSeparator : String, fieldSeparator : String } -> Headers -> Decoder a -> String -> Result Error (List a)
decodeCustom separators headers (Decoder decode) source =
    case Parser.customConfig separators of
        Err configProblem ->
            Err (ConfigError configProblem)

        Ok config ->
            case Parser.parse config source of
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
                        |> Result.mapError DecodingError

                Err deadEnds ->
                    Err (ParsingError deadEnds)


{-| Sometimes things go wrong. This record lets you know where exactly that
happened so you can go fix it.
-}
type Error
    = ConfigError Parser.ConfigProblem
    | ParsingError (List ElmParser.DeadEnd)
    | DecodingError { row : Int, problem : Problem }


{-| What, exactly, went wrong?
-}
type Problem
    = ExpectedColumn Int
    | ExpectedField String
    | AmbiguousColumn
    | ExpectedInt String
    | ExpectedFloat String
    | Failure String


{-| Want an easier-to-read version of an `Error`? Here we are!
-}
errorToString : Error -> String
errorToString error =
    case error of
        ConfigError Parser.NeedNonBlankFieldSeparator ->
            "Field separator must not be blank."

        ConfigError Parser.NeedNonBlankRowSeparator ->
            "Row separator must not be blank."

        ParsingError deadEnds ->
            deadEnds
                |> List.map
                    (\deadEnd ->
                        let
                            -- NOTE: we don't actually expect to see most of these,
                            -- but I'm going to avoid punting on making nice errors!
                            problemString =
                                case deadEnd.problem of
                                    ElmParser.Expecting expecting ->
                                        "expected to see `" ++ expecting ++ "`."

                                    ElmParser.ExpectingInt ->
                                        "expected to see an int."

                                    ElmParser.ExpectingHex ->
                                        "expected to see a hex number."

                                    ElmParser.ExpectingOctal ->
                                        "expected to see an octal number."

                                    ElmParser.ExpectingBinary ->
                                        "expected to see a binary number."

                                    ElmParser.ExpectingFloat ->
                                        "expected to see a floating-point number."

                                    ElmParser.ExpectingNumber ->
                                        "expected to see a number."

                                    ElmParser.ExpectingVariable ->
                                        "expected to see a variable."

                                    ElmParser.ExpectingSymbol symbol ->
                                        "expected to see a `" ++ symbol ++ "` symbol."

                                    ElmParser.ExpectingKeyword keyword ->
                                        "expected to see a `" ++ keyword ++ "` keyword."

                                    ElmParser.ExpectingEnd ->
                                        "expected the end of the input."

                                    ElmParser.UnexpectedChar ->
                                        "unexpected character."

                                    ElmParser.Problem problem ->
                                        problem

                                    ElmParser.BadRepeat ->
                                        "got a bad repeat (this is an internal problem and should be reported as a bug.)"
                        in
                        " - at line "
                            ++ String.fromInt deadEnd.row
                            ++ ", character "
                            ++ String.fromInt deadEnd.col
                            ++ ": "
                            ++ problemString
                    )
                |> String.concat
                |> (++) "There were some problems parsing the source:\n\n"

        DecodingError err ->
            let
                problemString =
                    case err.problem of
                        ExpectedColumn i ->
                            "I looked for a value in column " ++ String.fromInt i ++ ", but that column doesn't exist."

                        ExpectedField name ->
                            "I looked for a column named `" ++ name ++ "`, but couldn't find one."

                        AmbiguousColumn ->
                            "I needed there to be exactly one column."

                        ExpectedInt notInt ->
                            "I expected to parse an int from `" ++ notInt ++ "`, but couldn't."

                        ExpectedFloat notFloat ->
                            "I expected to parse an float from `" ++ notFloat ++ "`, but couldn't."

                        Failure custom ->
                            custom
            in
            "There was a problem on row "
                ++ String.fromInt err.row
                ++ ": "
                ++ problemString



-- MAPPING


{-| Decode a value, then transform it before returning.

    decodeCsv NoHeaders (map (\i -> i * 2) int) "15" --> Ok [ 30 ]

    decodeCsv NoHeaders (map String.reverse string) "slap" --> Ok [ "pals" ]

-}
map : (from -> to) -> Decoder from -> Decoder to
map transform (Decoder decoder) =
    Decoder (decoder >> Result.map transform)


{-| Combine two decoders to make something else, for example a record:

    decodeCsv
        NoHeaders
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


{-| Need to decode into an object? Use a pipeline instead. The way this works:
you provide a function that takes as many arguments as you need, and then
send it values by providing decoders.

    type alias Pet =
        { id : Int
        , name : String
        , species : String
        , weight : Float
        }

    petDecoder : Decoder Pet
    petDecoder =
        pipeline Pet
            |> required (column 0 int)
            |> required (column 1 string)
            |> required (column 2 string)
            |> required (column 3 float)

Now you can decode pets like this:

    decodeCsv NoHeaders petDecoder "1,Atlas,cat,14.5"
    --> Ok [ { id = 1, name = "Atlas", species = "cat", weight = 14.5 } ]

-}
pipeline : (a -> b) -> Decoder (a -> b)
pipeline =
    succeed


{-| See [`pipeline`](#pipeline).
-}
required : Decoder a -> Decoder (a -> b) -> Decoder b
required =
    map2 (\value fn -> fn value)



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

    decodeCsv NoHeaders myInt "1" -- Ok [ 1 ]

    decodeCsv NoHeaders myInt "fruit"
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
