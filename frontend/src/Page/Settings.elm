module Page.Settings exposing (Model, Msg(..), Settings, init, toSession, view, update, subscriptions)

import Array as Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import Route
import Session exposing (Session)
import Http
import Bootstrap.Navbar as Navbar
import Bootstrap.Carousel as Carousel
import Bootstrap.Button as Button


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
    { session : Session
    , problem : List Problem
    , form : Form
    , navbarState : Navbar.State
    }


type Problem
    = ServerError String


type alias Form =
    { email : String
    , profileName : String
    , password : String
    }



-- UPDATE

type Msg
    = GotAccountInfo (Result Http.Error Account) 
    | GotSettingsInfo (Result Http.Error Settings)
    | SubmitAccount
    | SetField Field String
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAccountInfo resp ->       
            ( model, Cmd.none )

        GotSettingsInfo resp ->                     
            ( model, Cmd.none )

        SubmitAccount ->
            ( model, Cmd.none )

        SetField field val ->
            ( model |> setField field val
            , Cmd.none
            )
        
        NavbarMsg state ->
            ({model | navbarState = state}, Cmd.none)




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

        Password ->
            updateForm (\form -> { form | password = val }) model
        
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
        , inputField Password model model.form.password "Password" "text"
        ]

viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ class "nav-menu-logo", href "/" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/home" ] [ text "Home" ]
            , Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/logout" ] [ text "Logout" ]
            ]
        |> Navbar.view model.navbarState

type Field
    = Email
    | Password
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
                
                _ -> 
                    ""
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

type alias Settings =
    {
    id : String
    }


-- Subscriptions 
subscriptions : Model -> Sub Msg 
subscriptions _ =
    Sub.none
