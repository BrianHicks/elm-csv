module Csv.Decode exposing
    ( Decoder, string, int, float, blank
    , column, field, optionalColumn, optionalField
    , FieldNames(..), decodeCsv, decodeCustom, Error(..), errorToString, Column(..), Problem(..)
    , map, map2, map3, into, pipeline
    , oneOf, andThen, succeed, fail, fromResult, fromMaybe
    )

{-| Decode values from CSV. This package tries to be as
unsurprising as possible, imitating [`elm/json`][elm-json] and
[`NoRedInk/elm-json-decode-pipeline`][json-decode-pipeline] so that you can
apply whatever you already know about JSON decoders to a different data format.

[elm-json]: https://package.elm-lang.org/packages/elm/json/latest/
[json-decode-pipline]: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/


## A Crash Course on Constructing Decoders

Say you have a CSV like this:

    ID,Name,Species
    1,Atlas,cat
    2,Axel,puffin

You want to get some data out of it, so you're looking through these docs.
Where do you begin?

The first thing you need to know is that decoders are designed to fit together
to match whatever data shapes are in your CSV. So to decode the ID (an `Int` in
the "ID" field), you'd combine [`int`](#int) and [`field`](#field) like this:

    data : String
    data =
        -- \u{000D} is the carriage return
        "ID,Name,Species\u{000D}\n1,Atlas,cat\u{000D}\n2,Axel,puffin"

    decodeCsv FieldNamesFromFirstRow (field "ID" int) data
    --> Ok [ 1, 2 ]

But this is probably not enough, so we'll need to combine a bunch of decoders
together using [`into`](#into):

    decodeCsv FieldNamesFromFirstRow
        (into
            (\id name species ->
                { id = id
                , name = name
                , species = species
                }
            )
            |> pipeline (field "ID" int)
            |> pipeline (field "Name" string)
            |> pipeline (field "Species" string)
        )
        data
    --> Ok
    -->     [ { id = 1, name = "Atlas", species = "cat" }
    -->     , { id = 2, name = "Axel", species = "puffin" }
    -->     ]

You can decode as many things as you want by giving [`into`](#into) a function
that takes more arguments.


## Basic Decoders

@docs Decoder, string, int, float, blank


## Finding Values

@docs column, field, optionalColumn, optionalField


## Running Decoders

@docs FieldNames, decodeCsv, decodeCustom, Error, errorToString, Column, Problem


## Transforming Values

@docs map, map2, map3, into, pipeline


## Fancy Decoding

@docs oneOf, andThen, succeed, fail, fromResult, fromMaybe

-}

import Csv.Parser as Parser
import Dict exposing (Dict)



-- BASIC DECODERS


{-| A way to specify what kind of thing you want to decode into. For example,
if you have a `Pet` data type, you'd want a `Decoder Pet`.
-}
type Decoder a
    = Decoder
        (Location
         -> Dict String Int
         -> Int
         -> List String
         ->
            Result
                { row : Int
                , column : Column
                , problems : List Problem
                }
                a
        )


fromString : (String -> Result Problem a) -> Decoder a
fromString convert =
    Decoder <|
        \location fieldNames rowNum row ->
            case location of
                Column_ colNum ->
                    case row |> List.drop colNum |> List.head of
                        Just value ->
                            case convert value of
                                Ok converted ->
                                    Ok converted

                                Err problem ->
                                    Err
                                        { row = rowNum
                                        , column = locationToColumn fieldNames location
                                        , problems = [ problem ]
                                        }

                        Nothing ->
                            Err
                                { row = rowNum
                                , column = locationToColumn fieldNames location
                                , problems = [ ColumnNotFound colNum ]
                                }

                Field_ name ->
                    case Dict.get name fieldNames of
                        Just colNum ->
                            case row |> List.drop colNum |> List.head of
                                Just value ->
                                    case convert value of
                                        Ok converted ->
                                            Ok converted

                                        Err problem ->
                                            Err
                                                { row = rowNum
                                                , column = locationToColumn fieldNames location
                                                , problems = [ problem ]
                                                }

                                Nothing ->
                                    Err
                                        { row = rowNum
                                        , column = locationToColumn fieldNames location
                                        , problems = [ FieldNotFound name ]
                                        }

                        Nothing ->
                            Err
                                { row = rowNum
                                , column = locationToColumn fieldNames location
                                , problems = [ FieldNotProvided name ]
                                }

                OnlyColumn_ ->
                    case row of
                        [] ->
                            Err
                                { row = rowNum
                                , column = locationToColumn fieldNames location
                                , problems = [ ColumnNotFound 0 ]
                                }

                        [ only ] ->
                            case convert only of
                                Ok converted ->
                                    Ok converted

                                Err problem ->
                                    Err
                                        { row = rowNum
                                        , column = locationToColumn fieldNames location
                                        , problems = [ problem ]
                                        }

                        _ ->
                            Err
                                { row = rowNum
                                , column = locationToColumn fieldNames location
                                , problems = [ ExpectedOneColumn (List.length row) ]
                                }


{-| Decode a string.

    decodeCsv NoFieldNames string "a" --> Ok [ "a" ]

Unless you specify otherwise (e.g. with [`field`](#field)) this will assume
there is only one column in the CSV and try to decode that.

    decodeCsv NoFieldNames string "a,b"
    --> Err
    -->     (DecodingErrors
    -->         [ { row = 0
    -->           , column = OnlyColumn
    -->           , problems = [ ExpectedOneColumn 2 ]
    -->           }
    -->         ]
    -->     )

-}
string : Decoder String
string =
    fromString Ok


{-| Decode an integer.

    decodeCsv NoFieldNames int "1" --> Ok [ 1 ]

    decodeCsv NoFieldNames int "volcano"
    --> Err
    -->     (DecodingErrors
    -->         [ { row = 0
    -->           , column = OnlyColumn
    -->           , problems = [ ExpectedInt "volcano" ]
    -->           }
    -->         ]
    -->     )

Unless you specify otherwise (e.g. with [`field`](#field)) this will assume
there is only one column in the CSV and try to decode that.

    decodeCsv NoFieldNames int "1,2"
    --> Err
    -->     (DecodingErrors
    -->         [ { row = 0
    -->           , column = OnlyColumn
    -->           , problems = [ ExpectedOneColumn 2 ]
    -->           }
    -->         ]
    -->     )

-}
int : Decoder Int
int =
    fromString <|
        \value ->
            case String.toInt (String.trim value) of
                Just parsed ->
                    Ok parsed

                Nothing ->
                    Err (ExpectedInt value)


{-| Decode a floating-point number.

    decodeCsv NoFieldNames float "3.14" --> Ok [ 3.14 ]

    decodeCsv NoFieldNames float "mimesis"
    --> Err
    -->     (DecodingErrors
    -->         [ { row = 0
    -->           , column = OnlyColumn
    -->           , problems = [ ExpectedFloat "mimesis" ]
    -->           }
    -->         ]
    -->     )

Unless you specify otherwise (e.g. with [`field`](#field)) this will assume
there is only one column in the CSV and try to decode that.

    decodeCsv NoFieldNames float "1.0,2.0"
    --> Err
    -->     (DecodingErrors
    -->         [ { row = 0
    -->           , column = OnlyColumn
    -->           , problems = [ ExpectedOneColumn 2 ]
    -->           }
    -->         ]
    -->     )

-}
float : Decoder Float
float =
    fromString <|
        \value ->
            case String.toFloat (String.trim value) of
                Just parsed ->
                    Ok parsed

                Nothing ->
                    Err (ExpectedFloat value)


{-| Handle blank fields by turning them into `Maybe`s. We consider a field
to be blank if it's empty or consists solely of whitespace characters.

    decodeCsv NoFieldNames (blank int) "\r\n1"
    --> Ok [ Nothing, Just 1 ]

-}
blank : Decoder a -> Decoder (Maybe a)
blank decoder =
    andThen
        (\maybeBlank ->
            if String.isEmpty (String.trim maybeBlank) then
                succeed Nothing

            else
                map Just decoder
        )
        string



-- LOCATIONS


type Location
    = Column_ Int
    | Field_ String
    | OnlyColumn_


{-| Parse a value at a numbered column, starting from 0.

    decodeCsv NoFieldNames (column 1 string) "a,b,c" --> Ok [ "b" ]

    decodeCsv NoFieldNames (column 100 float) "3.14"
    --> Err
    -->     (DecodingErrors
    -->         [ { row = 0
    -->           , column = Column 100
    -->           , problems = [ ColumnNotFound 100 ]
    -->           }
    -->         ]
    -->     )

-}
column : Int -> Decoder a -> Decoder a
column col (Decoder decoder) =
    Decoder (\_ fieldNames row -> decoder (Column_ col) fieldNames row)


{-| Like `column`, parse a value at a numbered column. The parsing succeeds even if the column is missing.

    decodeCsv
        NoFieldNames
        (optionalColumn 1 string)
        "Pie\r\nApple,Argentina"
    --> Ok [ Nothing, Just "Argentina" ]

-}
optionalColumn : Int -> Decoder a -> Decoder (Maybe a)
optionalColumn col (Decoder decoder) =
    Decoder
        (\_ fieldNames rowNum row ->
            if col < List.length row then
                Result.map Just (decoder (Column_ col) fieldNames rowNum row)

            else
                Ok Nothing
        )


{-| Parse a value at a named column. There are a number of ways to provide
these names, see [`FieldNames`](#FieldNames)

    decodeCsv
        FieldNamesFromFirstRow
        (field "Country" string)
        "Country\r\nArgentina"
    --> Ok [ "Argentina" ]

-}
field : String -> Decoder a -> Decoder a
field name (Decoder decoder) =
    Decoder (\_ fieldNames row -> decoder (Field_ name) fieldNames row)


{-| Like `field`, parse a value at a named column. The parsing succeeds even if the column is missing.

    decodeCsv
        FieldNamesFromFirstRow
        (optionalField "Country" string)
        "Country\r\nArgentina"
    --> Ok [ Just "Argentina" ]


    decodeCsv
        FieldNamesFromFirstRow
        (optionalField "Country" string)
        "Pie\r\nApple"
    --> Ok [ Nothing ]

-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField name (Decoder decoder) =
    Decoder
        (\_ fieldNames rowNum row ->
            if Dict.member name fieldNames then
                Result.map Just (decoder (Field_ name) fieldNames rowNum row)

            else
                Ok Nothing
        )



-- RUN DECODERS


{-| Where do we get names for use with [`field`](#field)?

  - `NoFieldNames`: don't get field names at all. [`field`](#field) will
    always fail.
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
                    Err NoFieldNamesOnFirstRow

                first :: rest ->
                    Ok ( fromList (List.map String.trim first), 1, rest )


{-| Convert a CSV string into some type you care about using the
[`Decoder`](#Decoder)s in this module!
-}
decodeCsv : FieldNames -> Decoder a -> String -> Result Error (List a)
decodeCsv =
    decodeCustom { fieldSeparator = ',' }


{-| Convert something shaped roughly like a CSV. For example, to decode
a TSV (_tab_-separated values) string:

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
    let
        defaultLocation : Location
        defaultLocation =
            OnlyColumn_
    in
    Result.andThen
        (\( resolvedNames, firstRowNumber, rows ) ->
            rows
                |> List.foldl
                    (\row ( soFar, rowNum ) ->
                        ( case decode defaultLocation resolvedNames rowNum row of
                            Ok val ->
                                case soFar of
                                    Ok values ->
                                        Ok (val :: values)

                                    Err errs ->
                                        Err errs

                            Err err ->
                                case soFar of
                                    Ok _ ->
                                        Err [ err ]

                                    Err errs ->
                                        Err (err :: errs)
                        , rowNum + 1
                        )
                    )
                    ( Ok [], firstRowNumber )
                |> Tuple.first
                |> Result.map List.reverse
                |> Result.mapError (DecodingErrors << List.reverse)
        )
        (getFieldNames fieldNames allRows)


{-| Sometimes we cannot decode every row in a CSV. This is how we tell
you what went wrong. If you need to present this to someone, you can get a
human-readable version with [`errorToString`](#errorToString)

Some more detail:

  - `ParsingError`: there was a problem parsing the CSV into rows and
    columns. All these errors have to do with quoting issues. Check that
    any quoted fields are closed and that quotes are escaped.
  - `NoFieldNamesOnFirstRow`: we tried to get the field names from the first
    row (using [`FieldNames`](#FieldNames)) but couldn't find any, probably
    because the input was blank.
  - `DecodingErrors`: we couldn't decode a value using the specified
    decoder. See [`Problem`](#Problem) for more details.

-}
type Error
    = ParsingError Parser.Problem
    | NoFieldNamesOnFirstRow
    | DecodingErrors
        (List
            { row : Int
            , column : Column
            , problems : List Problem
            }
        )


{-| Where did the problem happen?

  - `Column`: at the given column number
  - `Field`: at the given named column (with optional column number if we were
    able to look up what column we _should_ have found.)
  - `OnlyColumn`: at the only column in the row

-}
type Column
    = Column Int
    | Field String (Maybe Int)
    | OnlyColumn


locationToColumn : Dict String Int -> Location -> Column
locationToColumn fieldNames location =
    case location of
        Column_ i ->
            Column i

        Field_ name ->
            Field name (Dict.get name fieldNames)

        OnlyColumn_ ->
            OnlyColumn


{-| Things that can go wrong while decoding:

  - `ColumnNotFound Int` and `FieldNotFound String`: we looked for the
    specified column, but couldn't find it. The argument specifies where we
    tried to look.
  - `FieldNotProvided String`: we looked for a specific field, but it wasn't
    present in the first row or the provided field names (depending on your
    configuration.)
  - `ExpectedOneColumn Int`: basic decoders like [`string`](#string) and
    [`int`](#int) expect to find a single column per row. If there are multiple
    columns, and you don't specify which to use with [`column`](#column)
    or [`field`](#field), you'll get this error. The argument says how many
    columns we found.
  - `ExpectedInt String` and `ExpectedFloat String`: we failed to parse a
    string into a number. The argument specifies the string we got.
  - `Failure`: we got a custom failure message from [`fail`](#fail).

-}
type Problem
    = ColumnNotFound Int
    | FieldNotFound String
    | FieldNotProvided String
    | ExpectedOneColumn Int
    | ExpectedInt String
    | ExpectedFloat String
    | Failure String


{-| Produce a human-readable version of an [`Error`](#Error)?!
-}
errorToString : Error -> String
errorToString error =
    case error of
        ParsingError (Parser.SourceEndedWithoutClosingQuote row) ->
            "The source ended on row " ++ String.fromInt row ++ " in a quoted field without a closing quote."

        ParsingError (Parser.AdditionalCharactersAfterClosingQuote row) ->
            "On row " ++ String.fromInt row ++ " in the source, there were additional characters in a field after a closing quote."

        NoFieldNamesOnFirstRow ->
            "I expected to see field names on the first row, but there were none."

        DecodingErrors errs ->
            let
                problemString : Problem -> String
                problemString problem =
                    case problem of
                        ColumnNotFound i ->
                            "I couldn't find column #" ++ String.fromInt i ++ "."

                        FieldNotFound name ->
                            "I couldn't find the `" ++ name ++ "` column."

                        FieldNotProvided name ->
                            "The `" ++ name ++ "` field wasn't provided in the field names."

                        ExpectedOneColumn howMany ->
                            "I expected exactly one column, but there were " ++ String.fromInt howMany ++ "."

                        ExpectedInt notInt ->
                            "I could not parse an int from `" ++ notInt ++ "`."

                        ExpectedFloat notFloat ->
                            "I could not parse a float from `" ++ notFloat ++ "`."

                        Failure custom ->
                            custom

                rowAndColumnString : { a | row : Int, column : Column } -> String
                rowAndColumnString err =
                    "row "
                        ++ String.fromInt err.row
                        ++ ", "
                        ++ (case err.column of
                                Column col ->
                                    "column " ++ String.fromInt col

                                Field name Nothing ->
                                    "in the `" ++ name ++ "` field"

                                Field name (Just col) ->
                                    "in the `" ++ name ++ "` field (column " ++ String.fromInt col ++ ")"

                                OnlyColumn ->
                                    "column 0 (the only column present)"
                           )

                errString : { row : Int, column : Column, problems : List Problem } -> String
                errString err =
                    case List.map problemString err.problems of
                        [] ->
                            "There was an internal error while generating an error on "
                                ++ rowAndColumnString err
                                ++ " and I don't have any info about what went wrong. Please open an issue!"

                        [ only ] ->
                            "There was a problem on "
                                ++ rowAndColumnString err
                                ++ ": "
                                ++ only

                        many ->
                            "There were some problems on "
                                ++ rowAndColumnString err
                                ++ ":\n\n"
                                ++ String.join "\n" (List.map (\problem -> " - " ++ problem) many)
            in
            case errs of
                [] ->
                    "Something went wrong, but I got an blank error list so I don't know what it was. Please open an issue!"

                [ only ] ->
                    errString only

                many ->
                    "I saw "
                        ++ String.fromInt (List.length many)
                        ++ " problems while decoding this CSV:\n\n"
                        ++ String.join "\n\n" (List.map errString errs)



-- MAPPING


{-| Transform a decoded value.

    decodeCsv NoFieldNames (map (\i -> i * 2) int) "15"
    --> Ok [ 30 ]

    decodeCsv NoFieldNames (map String.reverse string) "slap"
    --> Ok [ "pals" ]

-}
map : (from -> to) -> Decoder from -> Decoder to
map transform (Decoder decoder) =
    Decoder (\location fieldNames rowNum row -> decoder location fieldNames rowNum row |> Result.map transform)


{-| Combine two decoders to make something else.

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
        (\location fieldNames rowNum row ->
            Result.map2 transform
                (decodeA location fieldNames rowNum row)
                (decodeB location fieldNames rowNum row)
        )


{-| Like [`map2`](#map2), but with three decoders. `map4` and beyond don't
exist in this package. Use [`into`](#into) to decode records instead!

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
        (\location fieldNames rowNum row ->
            Result.map3 transform
                (decodeA location fieldNames rowNum row)
                (decodeB location fieldNames rowNum row)
                (decodeC location fieldNames rowNum row)
        )


{-| Combine an arbitrary amount of fields. You provide a function that takes
as many arguments as you need, then send it values by providing decoders with
[`pipeline`](#pipeline).

    type alias Pet =
        { id : Int
        , name : String
        , species : String
        , weight : Float
        }

    petDecoder : Decoder Pet
    petDecoder =
        into Pet
            |> pipeline (column 0 int)
            |> pipeline (column 1 string)
            |> pipeline (column 2 string)
            |> pipeline (column 3 float)

Now you can decode pets like this:

    decodeCsv NoFieldNames petDecoder "1,Atlas,cat,14\r\n2,Axel,puffin,1.37"
    --> Ok
    -->     [ { id = 1, name = "Atlas", species = "cat", weight = 14 }
    -->     , { id = 2, name = "Axel", species = "puffin", weight = 1.37 }
    -->     ]

-}
into : (a -> b) -> Decoder (a -> b)
into =
    succeed


{-| See [`into`](#into).
-}
pipeline : Decoder a -> Decoder (a -> b) -> Decoder b
pipeline =
    map2 (\value fn -> fn value)



-- FANCY DECODING


{-| Try several possible decoders in sequence, committing to the first one
that passes.

    decodeCsv NoFieldNames
        (oneOf
            (map Just int)
            [ succeed Nothing ]
        )
        "1"
    --> Ok [ Just 1 ]

    decodeCsv NoFieldNames
        (oneOf
            (map Just int)
            [ succeed Nothing ]
        )
        "a"
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
        \location fieldNames rowNum row ->
            case first location fieldNames rowNum row of
                Ok value ->
                    Ok value

                Err err ->
                    case second location fieldNames rowNum row of
                        Ok value ->
                            Ok value

                        Err { problems } ->
                            Err { err | problems = err.problems ++ problems }


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
        (\location fieldNames rowNum row ->
            first location fieldNames rowNum row
                |> Result.andThen
                    (\nextValue ->
                        let
                            (Decoder final) =
                                next nextValue
                        in
                        final location fieldNames rowNum row
                    )
        )


{-| Always succeed, no matter what. Mostly useful with [`andThen`](#andThen).
-}
succeed : a -> Decoder a
succeed value =
    Decoder (\_ _ _ _ -> Ok value)


{-| Always fail with the given message, no matter what. Mostly useful with
[`andThen`](#andThen).
-}
fail : String -> Decoder a
fail message =
    Decoder
        (\location fieldNames rowNum _ ->
            Err
                { row = rowNum
                , column = locationToColumn fieldNames location
                , problems = [ Failure message ]
                }
        )


{-| Make creating custom decoders a little easier. If you already have a
function that parses into something you care about, you can combine it with
this.

For example, here's how you could parse a hexadecimal number with
[`rtfeldman/elm-hex`](https://package.elm-lang.org/packages/rtfeldman/elm-hex/latest/):

    import Hex

    hex : Decoder Int
    hex =
        andThen
            (\value -> fromResult (Hex.fromString value))
            string

    decodeCsv NoFieldNames hex "ff"
    --> Ok [ 255 ]

-}
fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok great ->
            succeed great

        Err problem ->
            fail problem


{-| Like [`fromResult`](#fromResult) but you have to specify the error
message since `Nothing` has no further information.

For example, you could implement something like [`int`](#int) using this:

    myInt : Decoder Int
    myInt =
        andThen
            (\value ->
                fromMaybe "Expected an int"
                    (String.toInt value)
            )
            string

    decodeCsv NoFieldNames myInt "123"
    --> Ok [ 123 ]

(That said, you probably want to use [`int`](#int) instead... it has better
error messages and is more tolerant of unusual situations!)

-}
fromMaybe : String -> Maybe a -> Decoder a
fromMaybe problem maybe =
    case maybe of
        Just value ->
            succeed value

        Nothing ->
            fail problem
