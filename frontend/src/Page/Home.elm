module Page.Home exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Session exposing (Session, cred)
import Route
import Bootstrap.Navbar as Navbar
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Helper exposing (prepareAuthHeader, endPoint)
import Json.Decode as Decode

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
                    navCmd

                Nothing ->
                    Route.pushUrl (Session.navKey session) Route.Login
                    
    in
    ( model , cmd )
        
type alias Model = 
    {session : Session
    , navbarState : Navbar.State
    , dropdownState : Dropdown.State
    }

type Msg
    = NavbarMsg Navbar.State
    | GetRecommendation
    | GotRecommendation  (Result Http.Error Recommendation)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavbarMsg state ->
            ({model | navbarState = state}, Cmd.none)

        GetRecommendation ->
            (model, getRecommendation model.session)
        
        GotRecommendation resp ->
            (model, Cmd.none)


view : Model -> {title : String, content : Html Msg}
view model =
    {title = "Home"
    , content = 
        div []
            [ viewNavbar model
            , viewRecommendation
            ]
    }


viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ class "nav-menu-logo", href "/" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/settings" ] [ text "Settings" ]
            , Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/logout" ] [ text "Logout" ]
            ]
        |> Navbar.view model.navbarState

viewRecommendation : Html Msg
viewRecommendation =
    div [ style "text-align" "center"]
        [ button [ class "recommend-button", onClick GetRecommendation ] [ text "Get Recommendation" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

toSession : Model -> Session
toSession {session} = 
    session

getRecommendation : Session -> Cmd Msg
getRecommendation session = 
    Http.request 
        { headers = [ prepareAuthHeader session, Http.header "Origin" "http://localhost:5000" ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson GotRecommendation decodeRecommendation
        , url = endPoint [""]
        , body = Http.emptyBody
        }

type alias Recommendation =
    { food : String
    }


decodeRecommendation : Decode.Decoder Recommendation
decodeRecommendation = 
    Decode.map Recommendation
        (Decode.field "food" Decode.string)

