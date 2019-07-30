module Helper exposing (endPoint, Response, decodeResponse, informHttpError, prepareAuthHeader)

import User exposing (Profile)
import Json.Decode exposing (field, Decoder, string, map3, map2)
import Json.Encode as Encode
import Http
import Session exposing (Session, cred)
import Cred exposing (getToken)
import Url.Builder as Builder

endPoint : String 
endPoint = 
    "http//:localhost:5000"

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
        (field "fullname" string)
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
        Http.BadStatus st ->
            let
                _ = Debug.log "" st
            in
            "Oh the request failed. Something must've gone wrong. Try again."
        
        Http.Timeout ->
            "Server took too long to respond."
        
        Http.NetworkError ->
            "Network error. Please check your wifi."
        
        Http.BadBody bb ->
            let
                _ = Debug.log "" bb
            in
            "Oooh something went wrong. Try again!"   
        
        Http.BadUrl bd ->
            let
                _ = Debug.log "" bd
            in
            "Invalid url."
                            
