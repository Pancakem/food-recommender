module Page.Home exposing (..)

import Session exposing (Session)

init : Session -> (Model, Cmd msg)
init session =
    ({
        session = session
    }
    , Cmd.none
     )
        
type alias Model = 
    {session : Session}


-- type Msg = 

