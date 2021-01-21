module Csv.Decode exposing
    ( Decoder, string, int, float
    , column
    , decodeString, Error, errorToString, Problem(..)
    , map, map2, map3
    , succeed, fail, andThen
    )

{-|

@docs Decoder, string, int, float

@docs column

@docs decodeString, Error, errorToString, Problem

@docs map, map2, map3

@docs succeed, fail, andThen

-}

import Csv.Parser as Parser
import Parser.Advanced



-- PRIMITIVES


type Decoder a
    = Decoder (List String -> Result Problem a)


string : Decoder String
string =
    Decoder (getOnly Ok)


int : Decoder Int
int =
    Decoder
        (getOnly
            (\value ->
                case String.toInt value of
                    Just parsed ->
                        Ok parsed

                    Nothing ->
                        Err (ExpectedInt value)
            )
        )


float : Decoder Float
float =
    Decoder
        (getOnly
            (\value ->
                case String.toFloat value of
                    Just parsed ->
                        Ok parsed

                    Nothing ->
                        Err (ExpectedFloat value)
            )
        )


getOnly : (String -> Result Problem a) -> List String -> Result Problem a
getOnly transform row =
    case row of
        [] ->
            Err (ExpectedColumn 0)

        [ only ] ->
            transform only

        _ ->
            Err AmbiguousColumn



-- LOCATIONS
-- TODO: field


column : Int -> Decoder a -> Decoder a
column col (Decoder decoder) =
    Decoder <|
        \row ->
            case row |> List.drop col |> List.head of
                Just value ->
                    decoder [ value ]

                Nothing ->
                    Err (ExpectedColumn col)



-- RUN DECODERS


decodeString : Decoder a -> String -> Result Error (List a)
decodeString (Decoder decode) source =
    case Parser.parse Parser.crlfCsvConfig source of
        Ok rows ->
            rows
                |> List.foldl
                    (\row ->
                        Result.andThen
                            (\( soFar, rowNum ) ->
                                case decode row of
                                    Ok val ->
                                        Ok ( val :: soFar, rowNum - 1 )

                                    Err problem ->
                                        Err { row = rowNum, problem = problem }
                            )
                    )
                    (Ok ( [], 0 ))
                |> Result.map (Tuple.first >> List.reverse)

        Err _ ->
            -- TODO: really punting on error message quality here but we'll
            -- get back to it!
            Err { row = 0, problem = ParsingProblem }


type alias Error =
    { problem : Problem
    , row : Int
    }


type Problem
    = ParsingProblem
    | ExpectedColumn Int
    | AmbiguousColumn
    | ExpectedInt String
    | ExpectedFloat String
    | Failure String


errorToString : Error -> String
errorToString _ =
    "TODO"



-- MAPPING


map : (from -> to) -> Decoder from -> Decoder to
map transform (Decoder decoder) =
    Decoder (decoder >> Result.map transform)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 transform (Decoder decodeA) (Decoder decodeB) =
    Decoder
        (\row ->
            Result.map2 transform
                (decodeA row)
                (decodeB row)
        )


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 transform (Decoder decodeA) (Decoder decodeB) (Decoder decodeC) =
    Decoder
        (\row ->
            Result.map3 transform
                (decodeA row)
                (decodeB row)
                (decodeC row)
        )



-- map4, map5, map6, map7, map8 or maybe pipelines?
-- FANCY DECODING


succeed : a -> Decoder a
succeed value =
    Decoder (\_ -> Ok value)


fail : String -> Decoder a
fail message =
    Decoder (\_ -> Err (Failure message))


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen next (Decoder first) =
    Decoder
        (\row ->
            first row
                |> Result.andThen
                    (\nextValue ->
                        let
                            (Decoder final) =
                                next nextValue
                        in
                        final row
                    )
        )
