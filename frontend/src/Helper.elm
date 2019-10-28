module Helper exposing (Response, decodeProfile, decodeResponse, endPoint, informHttpError, prepareAuthHeader)

import Cred exposing (getToken)
import Http
import Json.Decode exposing (Decoder, field, map2, map3, string)
import Json.Encode as Encode
import Session exposing (Session, cred)
import Url.Builder as Builder
import User exposing (Profile)


endPoint : List String -> String
endPoint lis =
    Builder.crossOrigin
        ""
        lis
        []


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
        token =
            case cred session of
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
                _ =
                    Debug.log "" st
            in
            "Oh the request failed. Something must've gone wrong. Try again."

        Http.Timeout ->
            "Server took too long to respond."

        Http.NetworkError ->
            "Network error. Please check your wifi."

        Http.BadBody bb ->
            let
                _ =
                    Debug.log "" bb
            in
            "Oooh something went wrong. Try again!"

        Http.BadUrl bd ->
            let
                _ =
                    Debug.log "" bd
            in
            "Invalid url."
