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

        parseQuotedField : (String -> Bool) -> List String -> Int -> Int -> Result (Int -> Problem) ( String, Int )
        parseQuotedField isFieldSeparator segments startOffset endOffset =
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
                        ( List.foldl (++) "" (segment :: segments)
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
                            ("\"" :: segment :: segments)
                            newPos
                            newPos

                    else if isFieldSeparator next || next == "\n" then
                        Ok
                            ( List.foldl (++) "" (segment :: segments)
                            , endOffset + 2
                            )

                    else if next == "\u{000D}" && String.slice (endOffset + 2) (endOffset + 3) source == "\n" then
                        Ok
                            ( List.foldl (++) "" (segment :: segments)
                            , endOffset + 3
                            )

                    else
                        Err AdditionalCharactersAfterClosingQuote

            else
                parseQuotedField isFieldSeparator segments startOffset (endOffset + 1)

        parseHelp : (String -> Bool) -> List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseHelp isFieldSeparator row rows startOffset endOffset =
            if endOffset - finalLength >= 0 then
                let
                    finalRow : List String
                    finalRow =
                        List.reverse (String.slice startOffset endOffset source :: row)
                in
                Ok (List.reverse (finalRow :: rows))

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
                    case parseQuotedField isFieldSeparator [] newPos newPos of
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

        parseComma : List String -> List (List String) -> Int -> Int -> Result Problem (List (List String))
        parseComma row rows startOffset endOffset =
            if endOffset - finalLength >= 0 then
                let
                    finalRow : List String
                    finalRow =
                        List.reverse (String.slice startOffset endOffset source :: row)
                in
                Ok (List.reverse (finalRow :: rows))

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
                    case parseQuotedField (\s -> s == ",") [] newPos newPos of
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
                    finalRow : List String
                    finalRow =
                        List.reverse (String.slice startOffset endOffset source :: row)
                in
                Ok (List.reverse (finalRow :: rows))

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
                    case parseQuotedField (\s -> s == ";") [] newPos newPos of
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
