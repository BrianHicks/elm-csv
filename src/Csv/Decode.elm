module Csv.Decode exposing (..)

import Csv.Parser as Parser
import Parser.Advanced



-- PRIMITIVES


type Decoder a
    = Decoder (List (List String) -> Result Error (List a))


string : Location -> Decoder String
string (Location get) =
    Decoder <|
        \rows ->
            rows
                |> List.foldr
                    (\next ->
                        Result.andThen
                            (\( soFar, rowNum ) ->
                                case get next of
                                    Ok val ->
                                        Ok ( val :: soFar, rowNum - 1 )

                                    Err problem ->
                                        Err { row = rowNum, problem = problem }
                            )
                    )
                    (Ok ( [], List.length rows ))
                |> Result.map Tuple.first



{-

   bool : Location -> Decoder Bool


   int : Location -> Decoder Int


   float : Location -> Decoder Float




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
    = Location (List String -> Result Problem String)



-- field : String -> Location
-- field name =
--     LocationTODO


column : Int -> Location
column col =
    Location <|
        \row ->
            case row |> List.drop (col - 1) |> List.head of
                Just value ->
                    Ok value

                Nothing ->
                    Err (MissingColumn col)



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
            decode (Debug.log "original rows" rows)

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
    | MissingColumn Int


errorToString : Error -> String
errorToString _ =
    "TODO"



{-

   -- MAPPING


   map : (from -> to) -> Decoder from -> Decoder to



   -- map2, map3, map4, map5, map6, map7, map8
   -- FANCY DECODING


   lazy : (() -> Decoder a) -> Decoder a


   -- value


   null : a -> Decoder a


   succeed : a -> Decoder a


   fail : String -> Decoder a


   andThen : (from -> Decoder to) -> Decoder from -> Decoder to
-}
