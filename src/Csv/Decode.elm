module Csv.Decode exposing (..)

-- PRIMITIVES


type Decoder a
    = TODODecoder


string : Decoder String


bool : Decoder Bool


int : Decoder Int


float : Decoder Float



-- DATA STRUCTURES


nullable : Decoder a -> Decoder (Maybe a)



-- escape hatch to JSON?
-- list
-- array
-- dict
-- keyValuePairs
-- oneOrMore
----------
-- OBJECT PRIMITIVES


field : String -> Decoder a -> Decoder a


index : Int -> Decoder a -> Decoder a



-- at
----------
-- INCONSISTENT STRUCTURE


maybe : Decoder a -> Decoder (Maybe a)


oneOf : List (Decoder a) -> Decoder a



-- RUN DECODERS


decodeString : Decoder a -> String -> Result Error a


decodeValue : Decoder a -> Value -> Result Error a


type Value
    = TODODefinedElsewhere


type Error
    = TODOError


errorToString : Error -> String



-- MAPPING


map : (from -> to) -> Decoder from -> Decoder to



-- map2, map3, map4, map5, map6, map7, map8
-- FANCY DECODING


lazy : (() -> Decoder a) -> Decoder a


value : Decoder Value


null : a -> Decoder a


succeed : a -> Decoder a


fail : String -> Decoder a


andThen : (from -> Decoder to) -> Decoder from -> Decoder to
