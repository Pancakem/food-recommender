module Cred exposing (Cred, decoder, encodeToken, getToken)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


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
