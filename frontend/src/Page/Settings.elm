module Page.Settings exposing (Model, Msg(..), FoodPreference, init, toSession, view, update, subscriptions)

import Array as Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import Route
import Session exposing (Session, logout)
import Http
import Bootstrap.Navbar as Navbar
import Bootstrap.Carousel as Carousel
import Bootstrap.Button as Button
import Helper exposing (prepareAuthHeader, endPoint, informHttpError, decodeProfile)
import Json.Decode as Decode


init : Session -> ( Model, Cmd Msg )
init session =
    let
        (navstate, navCmd) = Navbar.initialState NavbarMsg

        carouselstate = Carousel.initialStateWithOptions Carousel.defaultStateOptions 

        model = {
            session = session
            , navbarState = navstate
            , form =
            { email = ""
            , profileName = ""
            , password = ""            
            }
            , problem = []
            , likes = []
            }

        cmd =
            case Session.cred session of
                Just cred ->
                    Cmd.batch 
                        [ navCmd, (getAccountInfo session) ]--, --(getFoodPreferences session)]

                Nothing ->
                    Route.pushUrl (Session.navKey session) Route.Login
    in
    ( model , cmd )


type alias Model =
    { session : Session
    , problem : List Problem
    , form : Form
    , navbarState : Navbar.State
    , likes : Likes 
    }


type Problem
    = ServerError String


type alias Form =
    { email : String
    , profileName : String
    , password : String
    }

type alias Likes = List String

-- UPDATE

type Msg
    = GotAccountInfo (Result Http.Error Account) 
    | GotSettingsInfo (Result Http.Error FoodPreference)
    | SubmitAccount
    | SetField Field String
    | NavbarMsg Navbar.State
    | ClickedLogout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAccountInfo resp ->
            let
                newModel = 
                    case resp of
                        Ok a ->
                            updateForm (\f -> { f | profileName = a.username, email = a.email }) model
                            
                        Err e ->
                            { model | problem = [ ServerError <| informHttpError e]}
            in
                   
            ( newModel, Cmd.none )

        GotSettingsInfo resp ->                     
            let
                newModel = 
                    case resp of
                        Ok a ->
                            { model | likes = a.likes }
                            
                        Err e ->
                            { model | problem = [ ServerError <| informHttpError e]}
            in
                   
            ( newModel, Cmd.none )

        SubmitAccount ->
            ( model, Cmd.none )

        SetField field val ->
            ( model |> setField field val
            , Cmd.none
            )
        
        NavbarMsg state ->
            ({model | navbarState = state}, Cmd.none)
        
        ClickedLogout ->
            (model, logout)


-- record update helpers

updateForm : (Form -> Form) -> Model -> Model
updateForm transform model =
    { model | form = transform model.form }


setField : Field -> String -> Model -> Model
setField field val model =
    case field of
        Email ->
            updateForm (\form -> { form | email = val }) model

        ProfileName ->
            updateForm (\form -> { form | profileName = val }) model
        
-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content = viewSettings model
    }


viewSettings : Model -> Html Msg
viewSettings model =
    div []
    [ viewNavbar model
    , br [][], br [][]
    , div [ class "settings-container" ]
            [ ul [] (List.map (\str -> viewError str) model.problem)
            , viewAccountInfo model
            --, viewPersonalSettings model
            ]
    ]
    

viewError : Problem -> Html msg
viewError (ServerError str) = 
    div[ style "color" "red" ]
        [ text str ]
    
viewAccountInfo : Model -> Html Msg
viewAccountInfo model = 
    div [ class "task-form settings-form" ]
        [ text " Account Info"
        , inputField ProfileName model model.form.profileName "Full Name" "text"
        , inputField Email model model.form.email "Email" "text"
        ]

viewPersonalSettings : Model -> Html Msg
viewPersonalSettings model = 
    div [ class "task-form settings-form" ]
        [ text "Food Preferences"
        ]

viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ class "nav-menu-logo", href "/home" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/home" ] [ text "Home" ]
            , Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/", onClick ClickedLogout ] [ text "Logout" ]
            ]
        |> Navbar.view model.navbarState

type Field
    = Email
    | ProfileName

inputField : Field -> Model -> String -> String -> String -> Html Msg
inputField field {form} plceholder lbel taype =
    let
        val = 
            case field of
                Email -> 
                    form.email
                
                ProfileName ->
                    form.profileName
                
    in
    div [ class "" ]
        [ span [] [label [class "task-form-input-title"] [ text lbel ]]
        , input
            [ class "task-form-input"
            , type_ taype
            , placeholder plceholder
            , onInput <| SetField field
            , value val
            ]
            []
        ]


toSession : Model -> Session
toSession model =
    model.session

getInt : String -> Int
getInt str =
    String.toInt str |> Maybe.withDefault 0


type alias Account =
    { email : String
    , id : String
    , username : String
    }

type alias FoodPreference =
    { likes : List String
    }


-- Subscriptions 
subscriptions : Model -> Sub Msg 
subscriptions _ =
    Sub.none

-- http
getAccountInfo : Session -> Cmd Msg
getAccountInfo session = 
    Http.request
        { headers = [ prepareAuthHeader session, Http.header "Origin" "http://localhost:5000" ]
        , url = endPoint ["status"]
        , body = Http.emptyBody
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson GotAccountInfo decodeProfile
        }


-- getFoodPreferences : Session -> Cmd Msg
-- getFoodPreferences session = 
--     Http.request
--         { headers = [ prepareAuthHeader session, Http.header "Origin" "http://localhost:5000" ]
--         , url = endPoint ["status"]
--         , body = Http.emptyBody
--         , method = "GET"
--         , timeout = Nothing
--         , tracker = Nothing
--         , expect = Http.expectJson GotSettingsInfo decodeFoodPreference
--         }

-- decodeFoodPreference : Decode.Decoder FoodPreference
-- decodeFoodPreference = 
--     Decode.map FoodPreference
--         (Decode.field "likes" (Decode.list Decode.string))