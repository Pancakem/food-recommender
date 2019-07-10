module Page.Landing exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Html.Events exposing (..)
import Session exposing (..)
import Route

init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            }

        cmd =
            case Session.cred session of
                Just cred ->
                    Route.pushUrl (Session.navKey session) Route.Home

                Nothing ->
                    Cmd.none
    in
    ( model, cmd )

type alias Model = 
    {session : Session}

type Msg
    = ClickedLogin
    | ClickedRegister


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedLogin ->
            (model, Cmd.none)

        ClickedRegister ->
            (model
            , Route.pushUrl (Session.navKey model.session) Route.Register)


view : Model -> { title : String, content  : Html Msg  }
view model =
    { title = "Landing Page"
    , content = 
        div []
            [ text "New Html Program" 
            , br [] []
            , button [onClick ClickedRegister] [text "SIGN UP"]
            , br [] []
            , button [] [text "SIGN IN"]
            ]
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

toSession : Model -> Session
toSession model = 
    model.session