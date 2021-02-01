module Csv.Encode exposing (encode, Encoder, withFieldNames, withoutFieldNames)

{-|

@docs encode, Encoder, withFieldNames, withoutFieldNames

-}

import Dict exposing (Dict)


{-| -}
type Encoder a
    = WithFieldNames (List String) (a -> List ( String, String ))
    | WithoutFieldNames (a -> List String)


{-| Encode your data with a header full of field names at the top.

    [ ( "FF", "FF", "FF" )
    , ( "80", "80", "80" )
    , ( "00", "00", "00" )
    ]
        |> encode
            { encoder =
                withFieldNames
                    [ "red", "green", "blue" ]
                    (\( r, g, b ) ->
                        [ ( "red", r )
                        , ( "green", g )
                        , ( "blue", b )
                        ]
                    )
            , fieldSeparator = ','
            }
        --> "red,green,blue\r\nFF,FF,FF\r\n80,80,80\r\n00,00,00"

If you provide a field name which isn't in the list for a row, it will be
replaced with a blank field to avoid generating a misaligned CSV.

-}
withFieldNames : List String -> (a -> List ( String, String )) -> Encoder a
withFieldNames =
    WithFieldNames


{-| Encode your data whatver way you like. This is the "live an exciting
adventure" encoder: it will let you output rows with uneven lengths. It will
still escape quotes properly, however!

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


{-| Encode some data to a CSV string, quoting and escaping
characters as necessary. See [`withFieldNames`](#withFieldNames) and
[`withoutFieldNames`](#withoutFieldNames) for more on how to encode different
kinds of data.
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
        |> addFieldNames encoder
        |> List.map (String.join fieldSeparatorString << List.map (quoteIfNecessary fieldSeparatorString))
        |> String.join "\u{000D}\n"


encodeItems : Encoder a -> List a -> List (List String)
encodeItems encoder rows =
    case encoder of
        WithFieldNames names convert ->
            List.map
                (\row ->
                    let
                        named : Dict String String
                        named =
                            Dict.fromList (convert row)
                    in
                    List.map
                        (\name -> Dict.get name named |> Maybe.withDefault "")
                        names
                )
                rows

        WithoutFieldNames convert ->
            List.map convert rows


addFieldNames : Encoder a -> List (List String) -> List (List String)
addFieldNames encoder rows =
    case encoder of
        WithFieldNames names _ ->
            names :: rows

        WithoutFieldNames _ ->
            rows


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
