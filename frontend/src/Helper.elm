module Helper exposing (endPoint, ErrorMessage(..), asString)

import RemoteData exposing (..)
import Session exposing (Session)
import Http

type alias Model = 
    { session : Session }


endPoint : String 
endPoint = 
    ""

type ErrorMessage = 
    ErrorMessage String

                
asString : ErrorMessage -> String
asString (ErrorMessage str) =
    str