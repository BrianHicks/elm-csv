module Csv.Parser exposing
    ( Config, config, ConfigProblem(..)
    , parse, Problem(..)
    )

{-| CSV (and TSV) parsing.


## Configuration

@docs Config, config, ConfigProblem


## Parsing

@docs parse, Problem

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


type
    Problem
    -- TODO: need source location for both of these!
    = SourceEndedWithoutClosingQuote
    | AdditionalCharactersAfterClosingQuote


{-| Parse some data into a string-only list of lists. Prefer
using `Csv.Decode.decodeCsv` or `Csv.Decode.decodeCustom`
unless you need something unusally custom (and please [open an
issue](https://github.com/BrianHicks/elm-csv/issues/new) if you do!)
-}
parse : Config -> String -> Result Problem (List (List String))
parse (Config internalConfig) source =
    let
        _ =
            Debug.log "source" ( source, finalLength )

        finalLength : Int
        finalLength =
            String.length source

        parseQuotedField : List String -> Int -> Int -> Result Problem ( String, Int )
        parseQuotedField segments startOffset endOffset =
            let
                _ =
                    Debug.log "parseQuotedField" (String.left startOffset source ++ "|" ++ String.slice startOffset endOffset source ++ "|" ++ String.dropLeft endOffset source)
            in
            if endOffset >= finalLength then
                Err SourceEndedWithoutClosingQuote

            else if Debug.log "+1" (String.slice endOffset (endOffset + 1) source) == "\"" then
                let
                    segment =
                        String.slice startOffset endOffset source
                            |> Debug.log "segment"
                in
                if Debug.log "+2 offset" (endOffset + 2) > finalLength then
                    Ok
                        ( List.foldl (++) "" (segment :: segments)
                        , endOffset + 1
                        )
                        |> Debug.log "final because end of string"

                else if Debug.log "+2" (String.slice (endOffset + 1) (endOffset + 2) source) == "\"" then
                    -- "" is a quoted ". Unescape it and keep going.
                    let
                        newPos =
                            endOffset + 2
                    in
                    parseQuotedField
                        ("\"" :: segment :: segments)
                        newPos
                        newPos

                else
                    Err AdditionalCharactersAfterClosingQuote

            else
                parseQuotedField segments startOffset (endOffset + 1)

        parseHelp : List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
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
                let
                    newPos =
                        endOffset + 1
                in
                case parseQuotedField [] newPos newPos of
                    Ok ( value, afterQuotedField ) ->
                        if afterQuotedField >= finalLength then
                            Ok (List.reverse ((value :: row) :: rows))

                        else
                            parseHelp (value :: row) rows afterQuotedField afterQuotedField

                    Err problem ->
                        Err problem

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
