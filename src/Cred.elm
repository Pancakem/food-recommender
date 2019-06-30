module Cred exposing (..)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)

type Cred
    = Cred String


-- name 

-- add http auth header

encodeToken : String -> Value
encodeToken str =
    Encode.string str

decoder : Decoder Cred 
decoder = 
    Decode.map Cred Decode.string 