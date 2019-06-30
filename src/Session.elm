port module Session exposing(Session, decode, navKey, login, changes, cred, viewer, logout)

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Player exposing (..)
import Player.Cred as Cred exposing (..)
import Profile exposing (..)


type Session
    = LoggedIn Nav.Key Player
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
                |> Result.andThen (Decode.decodeString Player.decode)
                |> Result.toMaybe
    in
    case decoded_session of
        Just player ->
            LoggedIn key player

        Nothing ->
            Guest key


viewer : Session -> Maybe Player
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
            Just (Player.cred val)

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


login : { token : String, user : Profile } -> Cmd msg
login { token, user } =
    let
        session_value =
            Encode.object
                [ ( "token", Cred.encodeToken token )
                , ( "profile"
                  , Encode.object
                        [ ( "id", Encode.string (idToString user.id) )
                        , ( "username", Encode.string user.username )
                        , ( "email", Encode.string user.email )
                        ]
                  )
                ]

        encoded_s =
            Encode.encode 0 session_value
    in
    encoded_s
        |> Just
        |> storeSession