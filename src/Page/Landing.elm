module Page.Landing exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)

init : (Model, Cmd Msg)
init = 
    ({}, Cmd.none)

type alias Model = {}

type Msg
    = ClickedLogin
    | ClickedRegister


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedLogin ->
            (model, Cmd.none)

        ClickedRegister ->
            (model, Cmd.none)


view : Model -> { title : String, content  : Html msg  }
view model =
    { title = "Landing Page"
    , content = 
        div []
            [ text "New Html Program" 
            , br [] []
            , button [] [text "SIGN UP"]
            , br [] []
            , button [] [text "SIGN IN"]
            ]
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


