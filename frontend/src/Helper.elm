module Helper exposing (endPoint, Response, decodeResponse, informHttpError, prepareAuthHeader, decodeProfile)

import User exposing (Profile)
import Json.Decode exposing (field, Decoder, string, map3, map2)
import Json.Encode as Encode
import Http
import Session exposing (Session, cred)
import Cred exposing (getToken)
import Url.Builder as Builder

endPoint : List String -> String
endPoint lis = 
     Builder.crossOrigin
            "http://127.0.0.1:5000" lis []

type alias Response =
    { token : String
    , profile : Profile
    }

decodeResponse : Decoder Response
decodeResponse = 
    map2 Response
        (field "token" string)
        (field "profile" decodeProfile)

decodeProfile : Decoder Profile
decodeProfile = 
    map3 Profile
        (field "username" string)
        (field "email" string)
        (field "id" string)

prepareAuthHeader : Session -> Http.Header
prepareAuthHeader session = 
    let
        token = case cred session of
                    Just tk ->
                        getToken tk
                    
                    Nothing ->
                        ""
    in
    Http.header "Authorization" token


informHttpError : Http.Error -> String
informHttpError err = 
    case err of 
        Http.BadStatus _ ->
            "Oh the request failed. Something must've gone wrong. Try again."
        
        Http.Timeout ->
            "Server took too long to respond."
        
        Http.NetworkError ->
            "Network error. Please check your wifi."
        
        Http.BadBody _ ->
            "Oooh something went wrong. Try again!"   
        
        Http.BadUrl _ ->
            "Invalid url."
                            
