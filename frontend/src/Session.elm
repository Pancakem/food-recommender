port module Session exposing (Session, changes, cred, decode, login, logout, navKey, viewer)

import Browser.Navigation as Nav
import Cred exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import User exposing (..)


type Session
    = LoggedIn Nav.Key User
    | Guest Nav.Key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn key _ ->
            key


decode : Nav.Key -> Decode.Value -> Session
decode key value =
    let
        decoded_session =
            Decode.decodeValue Decode.string value
                |> Result.andThen (Decode.decodeString User.decode)
                |> Result.toMaybe
    in
    case decoded_session of
        Just user ->
            LoggedIn key user

        Nothing ->
            Guest key


viewer : Session -> Maybe User
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just (User.cred val)

        Guest _ ->
            Nothing


port storeSession : Maybe String -> Cmd msg


port sessionChanged : (Decode.Value -> msg) -> Sub msg


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    sessionChanged (\val -> toMsg (decode key val))


logout : Cmd msg
logout =
    storeSession Nothing


login : { token : String, profile : Profile } -> Cmd msg
login { token, profile } =
    let
        session_value =
            Encode.object
                [ ( "token", Cred.encodeToken token )
                , ( "profile"
                  , Encode.object
                        [ ( "id", Encode.string profile.id )
                        , ( "username", Encode.string profile.username )
                        , ( "email", Encode.string profile.email )
                        ]
                  )
                ]

        encoded_s =
            Encode.encode 0 session_value
    in
    encoded_s
        |> Just
        |> storeSession
