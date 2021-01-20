module Csv.Decode exposing (..)

import Array exposing (Array)
import Csv.Parser as Parser
import Parser.Advanced



-- PRIMITIVES


type Decoder a
    = Decoder (Array String -> Result Problem a)


string : Location -> Decoder String
string (Location get) =
    Decoder get


int : Location -> Decoder Int
int (Location get) =
    Decoder
        (get
            >> Result.andThen
                (\value ->
                    case String.toInt value of
                        Just parsed ->
                            Ok parsed

                        Nothing ->
                            Err (ExpectedInt value)
                )
        )


float : Location -> Decoder Float
float (Location get) =
    Decoder
        (get
            >> Result.andThen
                (\value ->
                    case String.toFloat value of
                        Just parsed ->
                            Ok parsed

                        Nothing ->
                            Err (ExpectedFloat value)
                )
        )



{-

   bool : Location -> Decoder Bool






      -- DATA STRUCTURES


      -- nullable doesn't make sense as a name since CSV does not have a
      -- null data type. When this gets uncommented, what about `optional`?
      nullable : Decoder a -> Decoder (Maybe a)

      -- maybe also add `blankable?`



      -- escape hatch to JSON?
      -- list
      -- array
      -- dict
      -- keyValuePairs
      -- oneOrMore
      ----------
      -- OBJECT PRIMITIVES

      field : String -> Decoder a -> Decoder a
      -- index
-}


type Location
    = Location (Array String -> Result Problem String)



-- field : String -> Location
-- field name =
--     LocationTODO


column : Int -> Location
column col =
    Location <|
        \row ->
            case Array.get col row of
                Just value ->
                    Ok value

                Nothing ->
                    Err (ExpectedColumn col)



{-

   ----------
   -- INCONSISTENT STRUCTURE
   -- maybe


   oneOf : List (Decoder a) -> Decoder a

-}
-- RUN DECODERS


decodeCsvString : Decoder a -> String -> Result Error (List a)
decodeCsvString (Decoder decode) source =
    case Parser.parse Parser.crlfCsvConfig source of
        Ok rows ->
            rows
                |> List.foldl
                    (\next ->
                        Result.andThen
                            (\( soFar, rowNum ) ->
                                case decode (Array.fromList next) of
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



{-
   decodeValue : Decoder a -> Value -> Result Error a


   type Value
       = TODODefinedElsewhere


-}


type alias Error =
    { problem : Problem
    , row : Int
    }


type Problem
    = ParsingProblem
    | ExpectedColumn Int
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



-- map4, map5, map6, map7, map8
{-



   lazy : (() -> Decoder a) -> Decoder a


   -- value


   null : a -> Decoder a


   succeed : a -> Decoder a


   fail : String -> Decoder a


   andThen : (from -> Decoder to) -> Decoder from -> Decoder to
-}
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
