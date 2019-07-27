module Cred exposing (Cred, encodeToken, decoder, getToken)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)

type Cred
    = Cred String


getToken : Cred -> String
getToken (Cred cred) = 
    cred

encodeToken : String -> Value
encodeToken str =
    Encode.string str

decoder : Decoder Cred 
decoder = 
    Decode.map Cred Decode.string 