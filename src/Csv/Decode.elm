module Csv.Decode exposing (..)

import Array exposing (Array)
import Csv.Parser as Parser
import Parser.Advanced



-- PRIMITIVES


type Decoder a
    = Decoder (List (Array String) -> Result Error (List a))


string : Location -> Decoder String
string =
    parseRows Ok


int : Location -> Decoder Int
int =
    parseRows <|
        \value ->
            case String.toInt value of
                Just parsed ->
                    Ok parsed

                Nothing ->
                    Err (ExpectedInt value)


float : Location -> Decoder Float
float =
    parseRows <|
        \value ->
            case String.toFloat value of
                Just parsed ->
                    Ok parsed

                Nothing ->
                    Err (ExpectedFloat value)


parseRows : (String -> Result Problem a) -> Location -> Decoder a
parseRows transform (Location get) =
    Decoder <|
        \rows ->
            rows
                |> List.foldr
                    (\next ->
                        Result.andThen
                            (\( soFar, rowNum ) ->
                                case Result.andThen transform (get next) of
                                    Ok val ->
                                        Ok ( val :: soFar, rowNum - 1 )

                                    Err problem ->
                                        Err { row = rowNum, problem = problem }
                            )
                    )
                    (Ok ( [], List.length rows - 1 ))
                |> Result.map Tuple.first



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
            decode (List.map Array.fromList rows)

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


errorToString : Error -> String
errorToString _ =
    "TODO"



{-

   -- MAPPING
   -- map2, map3, map4, map5, map6, map7, map8
   -- FANCY DECODING


   lazy : (() -> Decoder a) -> Decoder a


   -- value


   null : a -> Decoder a


   succeed : a -> Decoder a


   fail : String -> Decoder a


   andThen : (from -> Decoder to) -> Decoder from -> Decoder to
-}


map : (from -> to) -> Decoder from -> Decoder to
map transform (Decoder decoder) =
    Decoder (decoder >> Result.map (List.map transform))
