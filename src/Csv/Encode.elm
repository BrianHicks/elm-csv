module Csv.Encode exposing (encode, Encoder, withFieldNames, withoutFieldNames)

{-|

@docs encode, Encoder, withFieldNames, withoutFieldNames

-}

import Dict


{-| Describe how you want the output CSV to be shaped. Constructe
encoders with [`withFieldNames`](#withFieldNames) and
[`withoutFieldNames`](#withoutFieldNames).
-}
type Encoder a
    = WithFieldNames (a -> List ( String, String ))
    | WithoutFieldNames (a -> List String)


{-| When provided a function that maps field names to values, this function
uses it to produce a perfectly rectangular CSV.

    [ ( "FF", "FF", "FF" )
    , ( "80", "80", "80" )
    , ( "00", "00", "00" )
    ]
        |> encode
            { encoder =
                withFieldNames
                    (\( r, g, b ) ->
                        [ ( "red", r )
                        , ( "green", g )
                        , ( "blue", b )
                        ]
                    )
            , fieldSeparator = ','
            }
        --> "red,green,blue\r\nFF,FF,FF\r\n80,80,80\r\n00,00,00"

The ordering of columns is determined by the order of values returned from
the function.

  - If the function returns fields in an inconsistent order, we will determine
    a final ordering based on the average position of each column.

  - If the function sometimes omits `(field, value)` pairs, we will leave
    fields blank to avoid generating a misaligned CSV.

-}
withFieldNames : (a -> List ( String, String )) -> Encoder a
withFieldNames =
    WithFieldNames


{-| Encode your data however you like. This is the "live an exciting adventure"
encoder in that it will let you output rows with uneven lengths.

    [ ( "FF", "FF", "FF" )
    , ( "80", "80", "80" )
    , ( "00", "00", "00" )
    ]
        |> encode
            { encoder = withoutFieldNames (\(r,g, b) -> [ r, g, b ] )
            , fieldSeparator = ','
            }
        --> "FF,FF,FF\r\n80,80,80\r\n00,00,00"

-}
withoutFieldNames : (a -> List String) -> Encoder a
withoutFieldNames =
    WithoutFieldNames


{-| Encode some data to a CSV string, quoting and escaping characters as
necessary.
-}
encode :
    { encoder : Encoder a
    , fieldSeparator : Char
    }
    -> List a
    -> String
encode { encoder, fieldSeparator } items =
    let
        fieldSeparatorString : String
        fieldSeparatorString =
            String.fromChar fieldSeparator
    in
    items
        |> encodeItems encoder
        |> List.map (String.join fieldSeparatorString << List.map (quoteIfNecessary fieldSeparatorString))
        |> String.join "\u{000D}\n"


encodeItems : Encoder a -> List a -> List (List String)
encodeItems encoder rows =
    case encoder of
        WithFieldNames convert ->
            let
                ( converted, namePositions ) =
                    List.foldr
                        (\row ( converted_, names ) ->
                            let
                                convertedRow : List ( String, String )
                                convertedRow =
                                    convert row
                            in
                            ( Dict.fromList convertedRow :: converted_
                            , List.foldl
                                (\( name, _ ) ( soFar, column ) ->
                                    ( Dict.update name
                                        (\value ->
                                            case value of
                                                Just columns ->
                                                    Just (column :: columns)

                                                Nothing ->
                                                    Just [ column ]
                                        )
                                        soFar
                                    , column + 1
                                    )
                                )
                                ( names, 0 )
                                convertedRow
                                |> Tuple.first
                            )
                        )
                        ( [], Dict.empty )
                        rows

                ordering : List String
                ordering =
                    namePositions
                        |> Dict.map (\_ positions -> List.sum positions / toFloat (List.length positions))
                        |> Dict.toList
                        |> List.sortBy Tuple.second
                        |> List.map Tuple.first
            in
            ordering
                :: List.map
                    (\row ->
                        List.map
                            (\field -> Dict.get field row |> Maybe.withDefault "")
                            ordering
                    )
                    converted

        WithoutFieldNames convert ->
            List.map convert rows


quoteIfNecessary : String -> String -> String
quoteIfNecessary fieldSeparator value =
    if
        String.contains "\"" value
            || String.contains fieldSeparator value
            || String.contains "\u{000D}\n" value
            || String.contains "\n" value
    then
        "\"" ++ String.replace "\"" "\"\"" value ++ "\""

    else
        value
