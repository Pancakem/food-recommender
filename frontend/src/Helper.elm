module Helper exposing (endPoint, Response, decodeResponse, informHttpError)

import User exposing (Profile)
import Json.Decode exposing (field, Decoder, string, map3, map2)
import Json.Encode as Encode
import Http

endPoint : String 
endPoint = 
    ""

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
                            
