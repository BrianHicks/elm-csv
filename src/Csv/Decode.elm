module Csv.Decode exposing (..)

import Csv.Parser as Parser
import Parser.Advanced



-- PRIMITIVES


type Decoder a
    = DecoderTodo


string : Location -> Decoder String
string loc =
    DecoderTodo



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
    = LocationTODO


field : String -> Location
field name =
    LocationTODO


column : Int -> Location
column col =
    LocationTODO



{-

   ----------
   -- INCONSISTENT STRUCTURE
   -- maybe


   oneOf : List (Decoder a) -> Decoder a

-}
-- RUN DECODERS


decodeCsvString : Decoder a -> String -> Result Error (List a)
decodeCsvString decoder source =
    -- case Parser.parse Parser.crlfCsvConfig source of
    --     Ok rows ->
    --         decode rows
    --     Err err ->
    --         Err (ParsingError err)
    Err (ParsingError [])



{-
   decodeValue : Decoder a -> Value -> Result Error a


   type Value
       = TODODefinedElsewhere


-}


type Error
    = ParsingError (List (Parser.Advanced.DeadEnd Parser.Context Parser.Problem))


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
