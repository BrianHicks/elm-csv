module Csv.Parser exposing
    ( Config, config, ConfigProblem(..)
    , parse
    )

{-| CSV (and TSV) parsing.


## Configuration

@docs Config, config, ConfigProblem


## Parsing

@docs parse

-}

import Parser exposing ((|.), (|=), Parser)


{-| See [`config`](#config)
-}
type Config
    = Config InternalConfig


type alias InternalConfig =
    { row : String
    , rowLength : Int
    , field : String
    , fieldLength : Int
    }


{-| Not every string is a valid separator. This structure lets you know if
[`config`](#config) gets something bad.
-}
type ConfigProblem
    = NeedNonBlankRowSeparator
    | NeedNonBlankFieldSeparator


{-| Parse a row and field separator into something [`parse`](#parse) can use.
-}
config :
    { rowSeparator : String
    , fieldSeparator : String
    }
    -> Result ConfigProblem Config
config separators =
    case ( String.uncons separators.rowSeparator, String.uncons separators.fieldSeparator ) of
        ( Just ( rowFirst, rowRest ), Just ( fieldFirst, fieldRest ) ) ->
            (Ok << Config)
                { row = separators.rowSeparator
                , rowLength = String.length separators.rowSeparator
                , field = separators.fieldSeparator
                , fieldLength = String.length separators.fieldSeparator
                }

        ( Nothing, _ ) ->
            Err NeedNonBlankRowSeparator

        ( _, Nothing ) ->
            Err NeedNonBlankFieldSeparator


{-| Parse some data into a string-only list of lists. Prefer
using `Csv.Decode.decodeCsv` or `Csv.Decode.decodeCustom`
unless you need something unusally custom (and please [open an
issue](https://github.com/BrianHicks/elm-csv/issues/new) if you do!)
-}
parse : Config -> String -> Result String (List (List String))
parse (Config internalConfig) source =
    let
        finalLength : Int
        finalLength =
            String.length source

        parseHelp : List String -> List (List String) -> Int -> Int -> Result String (List (List String))
        parseHelp row rows startOffset endOffset =
            if endOffset >= finalLength then
                let
                    finalRow =
                        List.reverse (String.slice startOffset endOffset source :: row)
                in
                Ok (List.reverse (finalRow :: rows))

            else if String.slice endOffset (endOffset + internalConfig.fieldLength) source == internalConfig.field then
                let
                    newPos =
                        endOffset + internalConfig.fieldLength
                in
                parseHelp
                    (String.slice startOffset endOffset source :: row)
                    rows
                    newPos
                    newPos

            else if String.slice endOffset (endOffset + internalConfig.rowLength) source == internalConfig.row then
                let
                    newPos =
                        endOffset + internalConfig.rowLength
                in
                parseHelp
                    []
                    (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                    newPos
                    newPos

            else if String.slice endOffset (endOffset + 1) source == "\"" then
                -- implementing quoted fields will be A Thing, but the benchmark
                -- shouldn't go into this branch. I'm just adding it to get a
                -- more accurate read on where to start.
                parseHelp row rows startOffset endOffset

            else
                parseHelp
                    row
                    rows
                    startOffset
                    (endOffset + 1)
    in
    if String.isEmpty source then
        Ok []

    else
        parseHelp [] [] 0 0
