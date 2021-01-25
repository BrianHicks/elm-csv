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
    if String.length separators.rowSeparator == 0 then
        Err NeedNonBlankRowSeparator

    else if String.length separators.fieldSeparator == 0 then
        Err NeedNonBlankFieldSeparator

    else
        (Ok << Config)
            { row = separators.rowSeparator
            , rowLength = String.length separators.rowSeparator
            , field = separators.fieldSeparator
            , fieldLength = String.length separators.fieldSeparator
            }


{-| Something went wrong during parsing! What was it?

  - `SourceEndedWithoutClosingQuote`: we started parsing a quoted field,
    but the file ended before we saw a closing quote. If you meant to have
    a literal quote in your data, quote the whole field and then escape the
    literal quote by replacing it with `""`. For example, `": double prime`
    would be escaped as `"": double prime`
  - `AdditionalCharactersAfterClosingQuote`: we found the closing pair of a
    quoted field, but there was data after it instead of the end of the file
    or a separator. Follow the quote-escaping advice above to get around this.

-}
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
        finalLength : Int
        finalLength =
            String.length source

        parseQuotedField : List String -> Int -> Int -> Result Problem ( String, Int )
        parseQuotedField segments startOffset endOffset =
            if endOffset >= finalLength then
                Err SourceEndedWithoutClosingQuote

            else if String.slice endOffset (endOffset + 1) source == "\"" then
                let
                    segment : String
                    segment =
                        String.slice startOffset endOffset source
                in
                if (endOffset + 2) > finalLength then
                    Ok
                        ( List.foldl (++) "" (segment :: segments)
                        , endOffset + 1
                        )

                else if String.slice (endOffset + 1) (endOffset + 2) source == "\"" then
                    -- "" is a quoted ". Unescape it and keep going.
                    let
                        newPos : Int
                        newPos =
                            endOffset + 2
                    in
                    parseQuotedField
                        ("\"" :: segment :: segments)
                        newPos
                        newPos

                else if String.slice (endOffset + 1) (endOffset + 1 + internalConfig.fieldLength) source == internalConfig.field then
                    Ok
                        ( List.foldl (++) "" (segment :: segments)
                        , endOffset + 1 + internalConfig.fieldLength
                        )

                else
                    Err AdditionalCharactersAfterClosingQuote

            else
                parseQuotedField segments startOffset (endOffset + 1)

        parseHelp : List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseHelp row rows startOffset endOffset =
            if endOffset >= finalLength then
                let
                    finalRow : List String
                    finalRow =
                        List.reverse (String.slice startOffset endOffset source :: row)
                in
                Ok (List.reverse (finalRow :: rows))

            else if String.slice endOffset (endOffset + internalConfig.fieldLength) source == internalConfig.field then
                let
                    newPos : Int
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
                    newPos : Int
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
                    newPos : Int
                    newPos =
                        endOffset + 1
                in
                case parseQuotedField [] newPos newPos of
                    Ok ( value, afterQuotedField ) ->
                        if afterQuotedField >= finalLength then
                            Ok (List.reverse (List.reverse (value :: row) :: rows))

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

        {- This should be *exactly* the same as parseHelp, except it tries
           to consistently compare slices to literals instead of looking them
           up in `internalConfig`. Using literal values like this lets the
           compiler optimize to a literal `===`, which is much faster than the
           recursive compare-by-value equality call it'd generate otherwise.

           This gives like a 20% speedup for short sources, and the speedup
           grows with the source length.

           HOWEVER I recognize this optimization is specific to North
           America. In locales that write numbers like `1000,00` instead of
           `1000.00`, a semicolon is more common. I'm going to try and figure
           out what locales people are using this library with and consider
           adding more optimizations like thiis. I don't want to do that
           prematurely, however, since we're trading code size for speed.
        -}
        parseCsvHelp : List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseCsvHelp row rows startOffset endOffset =
            if endOffset >= finalLength then
                let
                    finalRow : List String
                    finalRow =
                        List.reverse (String.slice startOffset endOffset source :: row)
                in
                Ok (List.reverse (finalRow :: rows))

            else if String.slice endOffset (endOffset + 1) source == "," then
                let
                    newPos : Int
                    newPos =
                        endOffset + 1
                in
                parseCsvHelp
                    (String.slice startOffset endOffset source :: row)
                    rows
                    newPos
                    newPos

            else if String.slice endOffset (endOffset + 2) source == "\u{000D}\n" then
                let
                    newPos : Int
                    newPos =
                        endOffset + 2
                in
                parseCsvHelp
                    []
                    (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                    newPos
                    newPos

            else if String.slice endOffset (endOffset + 1) source == "\"" then
                let
                    newPos : Int
                    newPos =
                        endOffset + 1
                in
                case parseQuotedField [] newPos newPos of
                    Ok ( value, afterQuotedField ) ->
                        if afterQuotedField >= finalLength then
                            Ok (List.reverse (List.reverse (value :: row) :: rows))

                        else
                            parseCsvHelp (value :: row) rows afterQuotedField afterQuotedField

                    Err problem ->
                        Err problem

            else
                parseCsvHelp
                    row
                    rows
                    startOffset
                    (endOffset + 1)
    in
    if String.isEmpty source then
        Ok []

    else if internalConfig.field == "," && internalConfig.row == "\u{000D}\n" then
        parseCsvHelp [] [] 0 0

    else
        parseHelp [] [] 0 0
