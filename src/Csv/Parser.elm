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
    { rowFirst : Char
    , rowRest : String
    , fieldFirst : Char
    , fieldRest : String
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
                { rowFirst = rowFirst
                , rowRest = rowRest
                , fieldFirst = fieldFirst
                , fieldRest = fieldRest
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
        parseHelp : String -> List String -> List (List String) -> Int -> Int -> Result String (List (List String))
        parseHelp nextSource row rows startOffset endOffset =
            Err "NO"
    in
    if String.isEmpty source then
        Ok []

    else
        parseHelp source [] [] 0 0
