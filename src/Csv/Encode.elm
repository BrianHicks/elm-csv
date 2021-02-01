module Csv.Encode exposing (..)

import Dict


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
    items
        |> encodeItems encoder
        |> addFieldNames encoder
        |> List.map (String.join (String.fromChar fieldSeparator))
        |> String.join "\u{000D}\n"


encodeItems : Encoder a -> List a -> List (List String)
encodeItems encoder rows =
    case encoder of
        WithFieldNames names convert ->
            List.map
                (\row ->
                    let
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
