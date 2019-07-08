module Page.Home exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Session exposing (Session, cred)
import Route

init : Session -> (Model, Cmd msg)
init session =
    let
        cmd =
            case cred session of
                Just cred ->
                    Route.replaceUrl (Session.navKey session) Route.Login

                Nothing ->
                    Cmd.none
    in
    
    ({
        session = session
    }
    , cmd
     )
        
type alias Model = 
    {session : Session}

type Msg
    = Msg1
    | Msg2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg1 ->
            (model, Cmd.none)

        Msg2 ->
            (model, Cmd.none)


view : Model -> {title : String, content : Html Msg}
view model =
    {title = "Home"
    , content = 
        div []
            [ text "New Html Program" ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

toSession : Model -> Session
toSession {session} = 
    session

