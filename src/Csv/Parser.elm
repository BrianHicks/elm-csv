module Csv.Parser exposing (parse, Problem(..))

{-| CSV (and TSV) parsing.


## Parsing

@docs parse, Problem

-}


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
type Problem
    = SourceEndedWithoutClosingQuote Int
    | AdditionalCharactersAfterClosingQuote Int


{-| Parse some data into a string-only list of lists. Prefer
using `Csv.Decode.decodeCsv` or `Csv.Decode.decodeCustom`
unless you need something unusally custom (and please [open an
issue](https://github.com/BrianHicks/elm-csv/issues/new) if you do!)
-}
parse : { fieldSeparator : Char } -> String -> Result Problem (List (List String))
parse config source =
    let
        fieldSeparator : String
        fieldSeparator =
            String.fromChar config.fieldSeparator

        finalLength : Int
        finalLength =
            String.length source

        parseQuotedField : (String -> Bool) -> String -> Int -> Int -> Result (Int -> Problem) ( String, Int )
        parseQuotedField isFieldSeparator soFar startOffset endOffset =
            if endOffset - finalLength >= 0 then
                Err SourceEndedWithoutClosingQuote

            else if String.slice endOffset (endOffset + 1) source == "\"" then
                let
                    segment : String
                    segment =
                        String.slice startOffset endOffset source
                in
                if (endOffset + 2) - finalLength >= 0 then
                    Ok
                        ( soFar ++ segment
                        , endOffset + 1
                        )

                else
                    let
                        next : String
                        next =
                            String.slice (endOffset + 1) (endOffset + 2) source
                    in
                    if next == "\"" then
                        -- "" is a quoted ". Unescape it and keep going.
                        let
                            newPos : Int
                            newPos =
                                endOffset + 2
                        in
                        parseQuotedField
                            isFieldSeparator
                            (soFar ++ segment ++ "\"")
                            newPos
                            newPos

                    else if isFieldSeparator next || next == "\n" then
                        Ok
                            ( soFar ++ segment
                            , endOffset + 2
                            )

                    else if next == "\u{000D}" && String.slice (endOffset + 2) (endOffset + 3) source == "\n" then
                        Ok
                            ( soFar ++ segment
                            , endOffset + 3
                            )

                    else
                        Err AdditionalCharactersAfterClosingQuote

            else
                parseQuotedField isFieldSeparator soFar startOffset (endOffset + 1)

        parseHelp : (String -> Bool) -> List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseHelp isFieldSeparator row rows startOffset endOffset =
            if endOffset - finalLength >= 0 then
                let
                    finalField : String
                    finalField =
                        String.slice startOffset endOffset source
                in
                if finalField == "" && row == [] then
                    Ok (List.reverse rows)

                else
                    Ok (List.reverse (List.reverse (finalField :: row) :: rows))

            else
                let
                    first : String
                    first =
                        String.slice endOffset (endOffset + 1) source
                in
                if isFieldSeparator first then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    parseHelp
                        isFieldSeparator
                        (String.slice startOffset endOffset source :: row)
                        rows
                        newPos
                        newPos

                else if first == "\n" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    parseHelp
                        isFieldSeparator
                        []
                        (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                        newPos
                        newPos

                else if first == "\u{000D}" && String.slice (endOffset + 1) (endOffset + 2) source == "\n" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 2
                    in
                    parseHelp
                        isFieldSeparator
                        []
                        (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                        newPos
                        newPos

                else if first == "\"" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    case parseQuotedField isFieldSeparator "" newPos newPos of
                        Ok ( value, afterQuotedField ) ->
                            if afterQuotedField >= finalLength then
                                Ok (List.reverse (List.reverse (value :: row) :: rows))

                            else
                                parseHelp isFieldSeparator (value :: row) rows afterQuotedField afterQuotedField

                        Err problem ->
                            Err (problem (List.length rows + 1))

                else
                    parseHelp
                        isFieldSeparator
                        row
                        rows
                        startOffset
                        (endOffset + 1)

        {- This and `parseSemicolon` below are just specialized versions of
           `parseHelp` that produce more efficient generated code. The whole
           trick here is to compare to literals instead of variables, which
           makes the Elm compiler produce code that compares with `===`
           instead of a helper function that implements value-level comparison.

           To update these functions, just copy the body of `parseHelp`, then:

            1. replace the calls to `isFieldSeparator` with literal equality
               checks (e.g. `first == ","` in `parseComma`.)
            2. create a new `isFieldSeparator` to pass to `parseQuotedField`
               that does the same.

           Benchmark numbers without these functions ported will appear to
           be much slower, but it's fine to temporarily disable them in the
           bottom `if` of this function to fix bugs and stuff.
        -}
        parseComma : List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseComma row rows startOffset endOffset =
            if endOffset - finalLength >= 0 then
                let
                    finalField : String
                    finalField =
                        String.slice startOffset endOffset source
                in
                if finalField == "" && row == [] then
                    Ok (List.reverse rows)

                else
                    Ok (List.reverse (List.reverse (finalField :: row) :: rows))

            else
                let
                    first : String
                    first =
                        String.slice endOffset (endOffset + 1) source
                in
                if first == "," then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    parseComma
                        (String.slice startOffset endOffset source :: row)
                        rows
                        newPos
                        newPos

                else if first == "\n" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    parseComma
                        []
                        (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                        newPos
                        newPos

                else if first == "\u{000D}" && String.slice (endOffset + 1) (endOffset + 2) source == "\n" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 2
                    in
                    parseComma
                        []
                        (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                        newPos
                        newPos

                else if first == "\"" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    case parseQuotedField (\c -> c == ",") "" newPos newPos of
                        Ok ( value, afterQuotedField ) ->
                            if afterQuotedField >= finalLength then
                                Ok (List.reverse (List.reverse (value :: row) :: rows))

                            else
                                parseComma (value :: row) rows afterQuotedField afterQuotedField

                        Err problem ->
                            Err (problem (List.length rows + 1))

                else
                    parseComma
                        row
                        rows
                        startOffset
                        (endOffset + 1)

        parseSemicolon : List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseSemicolon row rows startOffset endOffset =
            if endOffset - finalLength >= 0 then
                let
                    finalField : String
                    finalField =
                        String.slice startOffset endOffset source
                in
                if finalField == "" && row == [] then
                    Ok (List.reverse rows)

                else
                    Ok (List.reverse (List.reverse (finalField :: row) :: rows))

            else
                let
                    first : String
                    first =
                        String.slice endOffset (endOffset + 1) source
                in
                if first == ";" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    parseSemicolon
                        (String.slice startOffset endOffset source :: row)
                        rows
                        newPos
                        newPos

                else if first == "\n" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    parseSemicolon
                        []
                        (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                        newPos
                        newPos

                else if first == "\u{000D}" && String.slice (endOffset + 1) (endOffset + 2) source == "\n" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 2
                    in
                    parseSemicolon
                        []
                        (List.reverse (String.slice startOffset endOffset source :: row) :: rows)
                        newPos
                        newPos

                else if first == "\"" then
                    let
                        newPos : Int
                        newPos =
                            endOffset + 1
                    in
                    case parseQuotedField (\c -> c == ";") "" newPos newPos of
                        Ok ( value, afterQuotedField ) ->
                            if afterQuotedField >= finalLength then
                                Ok (List.reverse (List.reverse (value :: row) :: rows))

                            else
                                parseSemicolon (value :: row) rows afterQuotedField afterQuotedField

                        Err problem ->
                            Err (problem (List.length rows + 1))

                else
                    parseSemicolon
                        row
                        rows
                        startOffset
                        (endOffset + 1)
    in
    if String.isEmpty source then
        Ok []

    else if config.fieldSeparator == ',' then
        parseComma [] [] 0 0

    else if config.fieldSeparator == ';' then
        parseSemicolon [] [] 0 0

    else
        parseHelp (\s -> s == fieldSeparator) [] [] 0 0
