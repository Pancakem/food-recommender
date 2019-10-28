module User exposing (..)

import Cred exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)


type alias Profile =
    { username : String
    , email : String
    , id : String
    }


profileEncoder : Profile -> Value
profileEncoder info =
    Encode.object
        [ ( "username", Encode.string info.username )
        , ( "email", Encode.string info.email )
        , ( "id", Encode.string info.id )
        ]


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> required "username" Decode.string
        |> required "email" Decode.string
        |> required "id" Decode.string



--|> Decode.map
-- Types


type User
    = User Internals


type alias Internals =
    { cred : Cred
    , profile : Profile
    }



-- info


cred : User -> Cred
cred (User data) =
    data.cred


id : User -> String
id (User data) =
    data.profile.id


profile : User -> Profile
profile (User data) =
    data.profile


decode : Decoder User
decode =
    Decode.map2 Internals
        (Decode.field "token" Cred.decoder)
        (Decode.field "profile" profileDecoder)
        |> Decode.map User
