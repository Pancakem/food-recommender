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
import Helper exposing (prepareAuthHeader, endPoint, informHttpError)
import Json.Decode as Decode

init : Session -> (Model, Cmd Msg)
init session =
    let
        (navstate, navCmd) = Navbar.initialState NavbarMsg

        dropdownstate = Dropdown.initialState

        model = {
            session = session
            , navbarState = navstate
            , recommendation = { food = ""}
            , dropdownState = dropdownstate
            , problem = []
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
    { session : Session
    , recommendation : Recommendation
    , navbarState : Navbar.State
    , dropdownState : Dropdown.State
    , problem : List Problem
    }

type Problem = ServerError String

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
            let
                newModel = 
                    case resp of
                        Ok data ->
                            {model | recommendation = data }
                        
                        Err er ->
                            {model | problem = [ServerError <| informHttpError er]}
            in
            
            (newModel, Cmd.none)


view : Model -> {title : String, content : Html Msg}
view model =
    {title = "Home"
    , content = 
        div []
            [ viewNavbar model
            , p [class "validation-problem"]
                (List.map (\str -> viewServerError str) model.problem)
            , viewRecommendation model
            ]
    }

viewServerError : Problem -> Html Msg
viewServerError (ServerError str) =
    text str

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

viewRecommendation : Model -> Html Msg
viewRecommendation model =
    div [ style "text-align" "center"]
        [
        div [] 
            [ text model.recommendation.food ]
        , br [] []
        , div []
            [ button [ class "recommend-button", onClick GetRecommendation ] [ text "Get Recommendation" ]]
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
        , url = endPoint ["recommend"]
        , body = Http.emptyBody
        }

type alias Recommendation =
    { food : String
    }


decodeRecommendation : Decode.Decoder Recommendation
decodeRecommendation = 
    Decode.map Recommendation
        (Decode.field "food" Decode.string)

