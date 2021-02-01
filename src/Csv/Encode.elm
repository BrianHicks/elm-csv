module Csv.Encode exposing (encode, Encoder, withFieldNames, withoutFieldNames)

{-| Turn a list of values into a CSV-formatted string, escaping characters
as necessary.

@docs encode, Encoder, withFieldNames, withoutFieldNames

-}

import Dict exposing (Dict)


type Encoder a
    = WithFieldNames (List String) (a -> List ( String, String ))
    | WithoutFieldNames (a -> List String)


withFieldNames : List String -> (a -> List ( String, String )) -> Encoder a
withFieldNames =
    WithFieldNames


withoutFieldNames : (a -> List String) -> Encoder a
withoutFieldNames =
    WithoutFieldNames


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
