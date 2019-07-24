module Page.Home exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Session exposing (Session, cred)
import Route
import Bootstrap.Navbar as Navbar
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button
import Html.Events exposing (..)
import Html.Attributes exposing (..)

init : Session -> (Model, Cmd Msg)
init session =
    let
        (navstate, navCmd) = Navbar.initialState NavbarMsg

        dropdownstate = Dropdown.initialState

        model = {
            session = session
            , navbarState = navstate
            , dropdownState = dropdownstate
            }

        cmd =
            case Session.cred session of
                Just cred ->
                    Route.pushUrl (Session.navKey session) Route.Home

                Nothing ->
                    navCmd
    in
    ( model , cmd )
        
type alias Model = 
    {session : Session
    , navbarState : Navbar.State
    , dropdownState : Dropdown.State
    }

type Msg
    = NavbarMsg Navbar.State
    | Msg2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavbarMsg state ->
            ({model | navbarState = state}, Cmd.none)

        Msg2 ->
            (model, Cmd.none)


view : Model -> {title : String, content : Html Msg}
view model =
    {title = "Home"
    , content = 
        div []
            [ viewNavbar model ]
    }


viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "/" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown", href "/settings" ] [ text "Settings" ]
            , Navbar.itemLink [ class "navbar-dropdown", href "/logout" ] [ text "Logout" ]
            ]
        |> Navbar.view model.navbarState


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

toSession : Model -> Session
toSession {session} = 
    session

