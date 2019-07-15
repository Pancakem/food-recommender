module Page.Landing exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Session exposing (..)
import Route
import Bootstrap.Navbar as Navbar

init : Session -> ( Model, Cmd Msg )
init session =
    let
        (navstate, navCmd) = Navbar.initialState NavbarMsg

        cmd =
            case Session.cred session of
                Just cred ->
                    Cmd.batch [Route.pushUrl (Session.navKey session) Route.Home]

                Nothing ->
                    navCmd
    in
    ( {session =session, navbarState = navstate}, cmd )

type alias Model = 
    {session : Session
    , navbarState : Navbar.State}

type Msg
    = NavbarMsg Navbar.State


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of        
        NavbarMsg state ->
            ({model | navbarState = state}, Cmd.none)


view : Model -> { title : String, content  : Html Msg  }
view model =
    { title = "Landing Page"
    , content = 
        div []
            [ 
            viewNavbar model 
            , br [] []
            ]
    }

viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "/" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "/login" ] [ text "Sign In" ]
            , Navbar.itemLink [ href "/register" ] [ text "Sign Up" ]
            ]
        |> Navbar.view model.navbarState

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

toSession : Model -> Session
toSession model = 
    model.session
