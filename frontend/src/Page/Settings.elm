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
import Helper exposing (prepareAuthHeader, endPoint, informHttpError, decodeProfile)
import Json.Decode as Decode


init : Session -> ( Model, Cmd Msg )
init session =
    let
        (navstate, navCmd) = Navbar.initialState NavbarMsg   

        model = {
            session = session
            , navbarState = navstate
            , user =
            { email = ""
            , profileName = ""            
            }
            , problem = []
            , preferences = {
                likes = ""
                , prIn = 0
                , carbIn = 0
                , vitaIn = 0
            }
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
    , user : User
    , navbarState : Navbar.State
    , preferences : FoodPreference 
    } 


type Problem
    = ServerError String


type alias User =
    { email : String
    , profileName : String
    }

-- UPDATE

type Msg
    = GotAccountInfo (Result Http.Error Account) 
    | GotSettingsInfo (Result Http.Error FoodPreference)
    | SubmitAccount
    | SetField Field String
    | NavbarMsg Navbar.State
    | ClickedLogout
    | Increase Ty
    | Decrease Ty


type Ty = 
    Carbohydrate
    | Vitamin
    | Protein

updateIntake : Ty -> FoodPreference -> FoodPreference
updateIntake ty fp =
    case ty of
        Carbohydrate ->
            {fp | carbIn = fp.carbIn + 1}
    
        Vitamin ->
            {fp | vitaIn = fp.vitaIn + 1}
        
        Protein ->
            {fp | prIn = fp.prIn + 1}
            

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ty ->
            let
                newModel = 
                    case ty of
                        Carbohydrate ->
                            {model | preferences = updateIntake ty model.preferences}
                        
                        Vitamin ->
                            {model | preferences = updateIntake ty model.preferences}

                        Protein ->
                            {model | preferences = updateIntake ty model.preferences}
                            
            in
            (newModel, Cmd.none)

        Decrease ty ->
            let
                newModel = 
                    case ty of
                        Carbohydrate ->
                            {model | preferences = updateIntake ty model.preferences}
                        
                        Vitamin ->
                            {model | preferences = updateIntake ty model.preferences}

                        Protein ->
                            {model | preferences = updateIntake ty model.preferences}
                            
            in
            (newModel, Cmd.none)

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
                            { model | preferences = a }
                            
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
            (model, Cmd.batch[ Session.logout, Route.pushUrl (Session.navKey model.session) Route.Login])


-- record update helpers

updateForm : (User -> User) -> Model -> Model
updateForm transform model =
    { model | user = transform model.user }
 

setField : Field -> String -> Model -> Model
setField field val model =
    case field of
        Email ->
            updateForm (\user -> { user | email = val }) model

        ProfileName ->
            updateForm (\user -> { user | profileName = val }) model
        
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
            , viewPersonalSettings model
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
        , inputField ProfileName model model.user.profileName "Full Name" "text"
        , inputField Email model model.user.email "Email" "text"
        ]

viewPersonalSettings : Model -> Html Msg
viewPersonalSettings model =
    let
        txt = 
            if model.preferences.likes == "vegan" then
                veganMojo
            else if model.preferences.likes == "vegeterian" then
                vegetarianMojo
            else
                theRestMojo
    in
    div [ class "" ]
        [ p [] [text"Food Preferences"]
        , hr [] []
        , label [] [
            text txt
            , input [
            type_ "text"
            , value model.preferences.likes 
            , disabled True
            ] []
        ]
        , p [] [text "Protein"]
        , button [onClick <| Increase Protein] [text "Increase protein uptake"]
        , button [onClick <| Decrease Protein] [text "Decrease protein uptake"]
        , hr [] []
        , p [] [text "Carbohydrates"]
        , button [onClick <| Increase Carbohydrate] [text "Increase carb uptake"]
        , button  [onClick <| Decrease Carbohydrate] [text "Decrease carb uptake"]
        , hr [] []
        , p [] [text "Vitamins"]
        , button [onClick <| Increase Vitamin] [text "Increase vitamin uptake"]
        , button [onClick <| Decrease Vitamin] [text "Decrease vitamin uptake"]      
        ]

viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ class "nav-menu-logo", href "/home" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/home" ] [ text "Home" ]
            , Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", onClick ClickedLogout ] [ text "Logout" ]
            ]
        |> Navbar.view model.navbarState

type Field
    = Email
    | ProfileName

inputField : Field -> Model -> String -> String -> String -> Html Msg
inputField field {user} plceholder lbel taype =
    let
        val = 
            case field of
                Email -> 
                    user.email
                
                ProfileName ->
                    user.profileName
                
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
    { likes : String
    , prIn: Int
    , carbIn : Int
    , vitaIn : Int
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


getFoodPreferences : Session -> Cmd Msg
getFoodPreferences session = 
    Http.request
        { headers = [ prepareAuthHeader session, Http.header "Origin" "http://localhost:5000" ]
        , url = endPoint ["status"]
        , body = Http.emptyBody
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson GotSettingsInfo decodeFoodPreference
        }

decodeFoodPreference : Decode.Decoder FoodPreference
decodeFoodPreference = 
    Decode.map4 FoodPreference
        (Decode.field "likes" Decode.string)
        (Decode.field "prIn" Decode.int)
        (Decode.field "carbIn" Decode.int)
        (Decode.field "vitaIn" Decode.int)


veganMojo = 
    """
    Veganism is the practice of abstaining from the use of animal products, 
    particularly in diet, and an associated philosophy that rejects the commodity status of animals. 
    A follower of the diet or the philosophy is known as a vegan.
    """

vegetarianMojo =
    """Vegetarianism is the practice of abstaining from the consumption of meat, 
    and may also include abstention from by-products of animals processed for food. 
    Vegetarianism may be adopted for various reasons. 
    Many people object to eating meat out of respect for sentient life
    """

theRestMojo =
    """You are so strong you can do anything that's why you eat anything. We love you.
    """
