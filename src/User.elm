module Player exposing (..)

import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

type alias Profile =
    { username : String
    , email :  String
    , id : String
    }


encode : Profile -> Value
encode info = 
    Encode.object
        [ ( "username", Encode.string info.username)
        , ( "email", Encode.string info.email ) 
        , ( "id", Encode.string info.id )
        ]

decoder : Decoder Profile
decoder =
    Decode.succeed Profile
        |> required "username" (Decode.string)
        |> required "email" (Decode.string)
        |> required "id" (Decode.string)
        --|> Decode.map 

-- Types 


type Player 
    = Player Internals

type alias Internals = 
    { cred : Cred
    , profile : Profile 
    }


-- info

cred : Player -> Cred 
cred (Player data) =
    data.cred

id : Player -> Id
id (Player data) =
    data.profile.id

profile : Player -> Profile 
profile (Player data) =
    data.profile 


decode : Decoder Player
decode =
    Decode.map2 Internals 
        (Decode.field "token" Cred.decoder)
        (Decode.field "profile" Profile.decoder)
        |> Decode.map Player