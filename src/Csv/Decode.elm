module Csv.Decode exposing
    ( Decoder, string, int, float, blank
    , column, field
    , FieldNames(..), decodeCsv, decodeCustom, Error(..), errorToString, Problem(..)
    , map, map2, map3, pipeline, required
    , oneOf, succeed, fail, andThen, fromResult
    )

{-| Decode values from CSV. A crash course on constructing decoders:

  - To decode basic values in different columns, use [`field`](#field) or
    [`column`](#column) along with [`int`](#int), [`string`](#string), or
    [`float`](#float).
  - To transform values in ways that can't fail, pass them to [`map`](#map). To
    transform them in ways that _can_ fail, pass decoders to
    [`andThen`](#andThen) and call [`succeed`](#succeed) or [`fail`](#fail)
    to handle the failure gracefully. If you just need to lift a
    `Result String a` into a decoder, use [`fromResult`](#fromResult)
  - To decode tuples, pass a constructor to [`map2`](#map2) or [`map3`](#map3).
  - To decode records, use [`pipeline`](#pipeline) and [`required`](#required).

All of those functions have examples in their documentation. Check 'em out!

If you run into trouble, this library intentionally sticks as close to [`elm/json`](https://package.elm-lang.org/packages/elm/json/latest/) and [`NoRedInk/elm-json-decode-pipeline`](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/) semantics as possible.
Figuring out how you'd write an equivalent JSON decoder may help!
(But if you run into something you truly can't do, please [open an issue](https://github.com/BrianHicks/elm-csv/issues/new).)


## Basic Decoders

@docs Decoder, string, int, float, blank


## Finding Values

@docs column, field


## Running Decoders

@docs FieldNames, decodeCsv, decodeCustom, Error, errorToString, Problem


## Transforming Values

@docs map, map2, map3, pipeline, required


## Fancy Decoding

@docs oneOf, succeed, fail, andThen, fromResult

-}

import Csv.Parser as Parser
import Dict exposing (Dict)



-- PRIMITIVES


{-| A way to specify what kind of thing you want to decode into. For example,
if you have a `Pet` data type, you'd want a `Decoder Pet`.
-}
type Decoder a
    = Decoder (( Dict String Int, List String ) -> Result Problem a)


{-| Decode a string from a CSV.

    decodeCsv NoFieldNames string "a" --> Ok [ "a" ]

Unless you specify otherwise (e.g. with [`column`](#column)) this will assume
there is only one column in the CSV and try to decode that.

    decodeCsv NoFieldNames string "a,b"
    --> Err (DecodingError { row = 0, problem = AmbiguousColumn })

-}
string : Decoder String
string =
    Decoder (Tuple.second >> getOnly Ok)


{-| Decode an integer from a CSV.

    decodeCsv NoFieldNames int "1" --> Ok [ 1 ]

    decodeCsv NoFieldNames int "-1" --> Ok [ -1 ]

    decodeCsv NoFieldNames int "volcano"
    --> Err (DecodingError { row = 0, problem = ExpectedInt "volcano" })

Unless you specify otherwise (e.g. with [`column`](#column)) this will assume
there is only one column in the CSV and try to decode that.

    decodeCsv NoFieldNames int "1,2"
    --> Err (DecodingError { row = 0, problem = AmbiguousColumn })

-}
int : Decoder Int
int =
    Decoder
        (Tuple.second
            >> getOnly
                (\value ->
                    case String.toInt value of
                        Just parsed ->
                            Ok parsed

                        Nothing ->
                            Err (ExpectedInt value)
                )
        )


{-| Decode an integer from a CSV.

    decodeCsv NoFieldNames float "3" --> Ok [ 3.0 ]

    decodeCsv NoFieldNames float "3.14" --> Ok [ 3.14 ]

    decodeCsv NoFieldNames float "mimesis"
    --> Err (DecodingError { row = 0, problem = ExpectedFloat "mimesis" })

Unless you specify otherwise (e.g. with [`column`](#column)) this will assume
there is only one column in the CSV and try to decode that.

    decodeCsv NoFieldNames float "1.0,2.0"
    --> Err (DecodingError { row = 0, problem = AmbiguousColumn })

-}
float : Decoder Float
float =
    Decoder
        (Tuple.second
            >> getOnly
                (\value ->
                    case String.toFloat value of
                        Just parsed ->
                            Ok parsed

                        Nothing ->
                            Err (ExpectedFloat value)
                )
        )


{-| Handle blank fields by making them into `Maybe`s.

    decodeCsv NoFieldNames (blank int) "\r\n1"
    --> Ok [ Nothing, Just 1 ]

    decodeCsv NoFieldNames (blank int) "not a number"
    --> Err (DecodingError { row = 0, problem = ExpectedInt "not a number" })

-}
blank : Decoder a -> Decoder (Maybe a)
blank decoder =
    andThen
        (\maybeBlank ->
            if String.isEmpty maybeBlank then
                succeed Nothing

            else
                map Just decoder
        )
        string


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


{-| Parse a value at a numbered column in the CSV, starting from 0.

    decodeCsv NoFieldNames (column 0 string) "Argentina" --> Ok [ "Argentina" ]

    decodeCsv NoFieldNames (column 1 int) "1,2,3" --> Ok [ 2 ]

    decodeCsv NoFieldNames (column 100 float) "3.14"
    --> Err (DecodingError { row = 0, problem = ExpectedColumn 100 })

-}
column : Int -> Decoder a -> Decoder a
column col (Decoder decoder) =
    Decoder <|
        \( names, row ) ->
            case row |> List.drop col |> List.head of
                Just value ->
                    decoder ( names, [ value ] )

                Nothing ->
                    Err (ExpectedColumn col)


{-| Parse a value at a named column in the CSV. There are a number of ways
to provide these names, see [`FieldNames`](#FieldNames)

    decodeCsv
        FieldNamesFromFirstRow
        (field "Country" string)
        "Country\r\nArgentina"
    --> Ok [ "Argentina" ]

    decodeCsv
        (CustomFieldNames [ "a", "b", "c" ])
        (field "b" int)
        "1,2,3"
    --> Ok [ 2 ]

    decodeCsv
        (CustomFieldNames [ "Constant" ])
        (field "Nonexistent" float)
        "3.14"
    --> Err (DecodingError { row = 0, problem = ExpectedField "Nonexistent" })

-}
field : String -> Decoder a -> Decoder a
field name (Decoder decoder) =
    Decoder <|
        \( names, row ) ->
            -- TODO: AHHHHHHHHHHHHH an array would be way better
            case Dict.get name names |> Maybe.andThen (\col -> row |> List.drop col |> List.head) of
                Just value ->
                    decoder ( names, [ value ] )

                Nothing ->
                    Err (ExpectedField name)



-- RUN DECODERS


{-| Where do we get names for use with [`field`](#field)?

  - `NoFieldNames`: don't get field names at all. [`field`](#field) will always fail.
  - `CustomFieldNames`: use the provided field names in order (so `["Id", "Name"]`
    will mean that "Id" is in column 0 and "Name" is in column 1.)
  - `FieldNamesFromFirstRow`: use the first row of the CSV as the source of
    field names.

-}
type FieldNames
    = NoFieldNames
    | CustomFieldNames (List String)
    | FieldNamesFromFirstRow


getFieldNames : FieldNames -> List (List String) -> Result Error ( Dict String Int, Int, List (List String) )
getFieldNames headers rows =
    let
        fromList : List String -> Dict String Int
        fromList names =
            names
                |> List.foldl
                    (\name ( soFar, i ) ->
                        ( Dict.insert name i soFar
                        , i + 1
                        )
                    )
                    ( Dict.empty, 0 )
                |> Tuple.first
    in
    case headers of
        NoFieldNames ->
            Ok ( Dict.empty, 0, rows )

        CustomFieldNames names ->
            Ok ( fromList names, 0, rows )

        FieldNamesFromFirstRow ->
            case rows of
                [] ->
                    Err (DecodingError { row = 0, problem = NoFieldNamesOnFirstRow })

                first :: rest ->
                    Ok ( fromList first, 1, rest )


{-| Convert a CSV string into some type you care about using the
[`Decoder`](#Decoder)s in this module!
-}
decodeCsv : FieldNames -> Decoder a -> String -> Result Error (List a)
decodeCsv =
    decodeCustom { fieldSeparator = ',' }


{-| Convert something shaped roughly like a CSV. For example, to decode
tab-separated values where the row separator is a single newline character:

    decodeCustom {  fieldSeparator = '\t' }
        NoFieldNames
        (map2 Tuple.pair
            (column 0 int)
            (column 1 string)
        )
        "1\tBrian\n2\tAtlas"
        --> Ok [ ( 1, "Brian" ), ( 2, "Atlas" ) ]

-}
decodeCustom : { fieldSeparator : Char } -> FieldNames -> Decoder a -> String -> Result Error (List a)
decodeCustom config fieldNames decoder source =
    Parser.parse config source
        |> Result.mapError ParsingError
        |> Result.andThen (applyDecoder fieldNames decoder)


applyDecoder : FieldNames -> Decoder a -> List (List String) -> Result Error (List a)
applyDecoder fieldNames (Decoder decode) allRows =
    Result.andThen
        (\( names, firstRowNumber, rows ) ->
            rows
                |> List.foldl
                    (\row ->
                        Result.andThen
                            (\( soFar, rowNum ) ->
                                case decode ( names, row ) of
                                    Ok val ->
                                        Ok ( val :: soFar, rowNum - 1 )

                                    Err problem ->
                                        Err { row = rowNum, problem = problem }
                            )
                    )
                    (Ok ( [], firstRowNumber ))
                |> Result.map (Tuple.first >> List.reverse)
                |> Result.mapError DecodingError
        )
        (getFieldNames fieldNames allRows)


{-| Sometimes things go wrong. This is how we tell you what happened. If you
need to present this to someone, you can get a human-readable version with
[`errorToString`](#errorToString)

Some more detail:

  - `ConfigError`: `decodeCustom` got a bad separator character
  - `ParsingError`: there was a problem parsing the data (the most common issue
    is problems with quoted fields. Check that any quoted fields are closed
    and that quotes are escaped by doubling.)
  - `DecodingError`: we couldn't decode a value using the specified
    decoder. See [`Problem`](#Problem) for more details.

-}
type Error
    = ParsingError Parser.Problem
    | DecodingError { row : Int, problem : Problem }


{-| Things that went wrong specifically while decoding.

  - `NoFieldNamesOnFirstRow`: we tried to get the field names from the first
    row (using [`FieldNames`](#FieldNames)) but couldn't find any, probably
    because the input was blank.
  - `ExpectedColumn Int` and `ExpectedField String`: we looked for a value
    at a specific column, but couldn't find it. The argument specifies where
    we tried to look.
  - `AmbiguousColumn`: basic decoders expect to find a single value. If there
    are multiple fields in a row, and you don't specify which one to use with
    [`column`](#column) or [`field`](#field), you'll get this error.
  - `ExpectedInt String` and `ExpectedFloat String`: we tried to parse a
    string into a number, but couldn't. The arguments specify the strings
    we got.
  - `Failure`: messages from [`fail`](#fail) end up here.
  - `ManyProblems`: when there are multiple failures, messages from
    [`oneOf`](#oneOf) end up here.

-}
type Problem
    = NoFieldNamesOnFirstRow
    | ExpectedColumn Int
    | ExpectedField String
    | AmbiguousColumn
    | ExpectedInt String
    | ExpectedFloat String
    | Failure String
    | ManyProblems Problem Problem


{-| Want an human-readable version of an [`Error`](#Error)? Here we are!
-}
errorToString : Error -> String
errorToString error =
    case error of
        ParsingError (Parser.SourceEndedWithoutClosingQuote row) ->
            "The source ended on row " ++ String.fromInt row ++ " in a quoted field without a closing quote."

        ParsingError (Parser.AdditionalCharactersAfterClosingQuote row) ->
            "On row " ++ String.fromInt row ++ " in the source, there were additional characters in a field after a closing quote."

        DecodingError err ->
            let
                problems : Problem -> List String
                problems problem =
                    case problem of
                        NoFieldNamesOnFirstRow ->
                            [ "I expected to see field names on the first row, but there were none." ]

                        ExpectedColumn i ->
                            [ "I looked for a value in column " ++ String.fromInt i ++ ", but that column doesn't exist." ]

                        ExpectedField name ->
                            [ "I looked for a column named `" ++ name ++ "`, but couldn't find one." ]

                        AmbiguousColumn ->
                            [ "I needed there to be exactly one column." ]

                        ExpectedInt notInt ->
                            [ "I expected to parse an int from `" ++ notInt ++ "`, but couldn't." ]

                        ExpectedFloat notFloat ->
                            [ "I expected to parse an float from `" ++ notFloat ++ "`, but couldn't." ]

                        Failure custom ->
                            [ custom ]

                        ManyProblems first second ->
                            problems first ++ problems second
            in
            case problems err.problem of
                [] ->
                    "There was an internal error and I don't have any info about what went wrong. Please open an issue!"

                [ only ] ->
                    "There was a problem on row "
                        ++ String.fromInt err.row
                        ++ ": "
                        ++ only

                multi ->
                    "There were some problems on row "
                        ++ String.fromInt err.row
                        ++ ":\n\n"
                        ++ String.join "\n" (List.map (\problem -> " - " ++ problem) multi)



-- MAPPING


{-| Decode a value, then transform it before returning.

    decodeCsv NoFieldNames (map (\i -> i * 2) int) "15" --> Ok [ 30 ]

    decodeCsv NoFieldNames (map String.reverse string) "slap" --> Ok [ "pals" ]

-}
map : (from -> to) -> Decoder from -> Decoder to
map transform (Decoder decoder) =
    Decoder (decoder >> Result.map transform)


{-| Combine two decoders to make something else, for example a tuple:

    decodeCsv NoFieldNames
        (map2 Tuple.pair
            (column 0 int)
            (column 1 string)
        )
        "1,Atlas"
        --> Ok [ (1, "Atlas") ]

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 transform (Decoder decodeA) (Decoder decodeB) =
    Decoder
        (\row ->
            Result.map2 transform
                (decodeA row)
                (decodeB row)
        )


{-| Like [`map2`](#map2), but with three decoders. `map4` and beyond don't
exist in this package. Use [`pipeline`](#pipeline) to decode records instead!

    decodeCsv NoFieldNames
        (map3 (\r g b -> (r, g, b))
            (column 0 int)
            (column 1 int)
            (column 2 int)
        )
        "255,255,0"
        --> Ok [ (255, 255, 0) ]

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


{-| Need to decode into an object? Use a pipeline. The way this works: you
provide a function that takes as many arguments as you need, and then send
it values by providing decoders.

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

    decodeCsv NoFieldNames petDecoder "1,Atlas,cat,14.5"
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


{-| Try several possible decoders in sequence, committing to the first one
that passes.

    decodeCsv NoFieldNames (oneOf (map Just int) [ succeed Nothing ]) "1"
    --> Ok [ Just 1 ]

    decodeCsv NoFieldNames (oneOf (map Just int) [ succeed Nothing ]) "a"
    --> Ok [ Nothing ]

-}
oneOf : Decoder a -> List (Decoder a) -> Decoder a
oneOf first rest =
    case rest of
        [] ->
            first

        next :: others ->
            recover first (oneOf next others)


recover : Decoder a -> Decoder a -> Decoder a
recover (Decoder first) (Decoder second) =
    Decoder <|
        \rowAndNames ->
            case first rowAndNames of
                Ok value ->
                    Ok value

                Err problem ->
                    case second rowAndNames of
                        Ok value ->
                            Ok value

                        Err problem2 ->
                            Err (ManyProblems problem problem2)


{-| Always succeed, no matter what. Mostly useful with [`andThen`](#andThen).
-}
succeed : a -> Decoder a
succeed value =
    Decoder (\_ -> Ok value)


{-| Always fail with the given message, no matter what. Mostly useful with
[`andThen`](#andThen).
-}
fail : String -> Decoder a
fail message =
    Decoder (\_ -> Err (Failure message))


{-| Decode some value _and then_ make a decoding decision based on the
outcome. For example, if you wanted to reject negative numbers, you might
do something like this:

    positiveInt : Decoder Int
    positiveInt =
        int
            |> andThen
                (\rawInt ->
                    if rawInt < 0 then
                        Decode.fail "Only positive numbers allowed!"

                    else
                        Decode.succeed rawInt
                )

You could then use it like this:

    decodeCsv NoFieldNames positiveInt "1" -- Ok [ 1 ]

    decodeCsv NoFieldNames positiveInt "-1"
    -- Err { row = 0, problem = Failure "Only positive numbers allowed!" }

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


{-| Make creating custom decoders a little easier. If you already have a
function that parses a string into something you care about, you can probably
provide it here and avoid having to build your own with [`andThen`](#andThen).

For example, here's how you could parse a hexadecimal number with
[`rtfeldman/elm-hex`](https://package.elm-lang.org/packages/rtfeldman/elm-hex/latest/):

    import Hex

    hex : Decoder Int
    hex =
        fromResult Hex.fromString

    decodeCsv NoFieldNames hex "ff"
    --> Ok [ 255 ]

-}
fromResult : (String -> Result String a) -> Decoder a
fromResult convert =
    andThen
        (\input ->
            case convert input of
                Ok great ->
                    succeed great

                Err problem ->
                    fail problem
        )
        string
