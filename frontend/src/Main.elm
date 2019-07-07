module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Browser
import Browser.Navigation exposing (..)
import Url
import Page
import Route
import Page.Landing as Landing
import Page.NotFound as NtFound
import Page.Register as Register
import Page.Login as Login
import Page.Settings as Settings
import Session exposing (Session)
import Json.Encode exposing (Value)

init : Value -> Url.Url -> Key -> (Model, Cmd Msg)
init flags url key = 
    let
        decoded_session = Session.decode key flags
    in
    changeRouteTo (Route.fromUrl url) <| Load decoded_session

main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
    }

type Model =
    NotFound Session
    | Load Session
    | LandingModel Landing.Model
    | RegisterModel Register.Model
    | LoginModel Login.Model
    | SettingModel Settings.Model

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Empty
    | LandingMsg Landing.Msg
    | RegisterMsg Register.Msg
    | LoginMsg Login.Msg
    | SettingMsg Settings.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (LandingMsg subMsg, LandingModel landing)  ->
            Landing.update subMsg landing 
                |> updateWith LandingModel LandingMsg model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model, load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        (RegisterMsg subMsg, RegisterModel register) ->
            Register.update subMsg register
                |> updateWith RegisterModel RegisterMsg model
        
        (LoginMsg subMsg, LoginModel login) ->
            Login.update subMsg login
                |> updateWith LoginModel LoginMsg model
        
        (SettingMsg subMsg, SettingModel settings) ->
            Settings.update subMsg settings
                |> updateWith SettingModel SettingMsg model
        
        (_, _) ->
            (model, Cmd.none)

changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session = toSession model
    in
    case maybeRoute of        
        Just Route.Landing ->
            Landing.init session
                |> updateWith LandingModel LandingMsg model

        Just Route.Home ->
            (model, Cmd.none)
        
        Just Route.Login ->
            Login.init session
                |> updateWith LoginModel LoginMsg model
        
        Just Route.Register ->
            Register.init session
                |> updateWith RegisterModel RegisterMsg model
        
        Just Route.Settings ->
            Settings.init session
                |> updateWith SettingModel SettingMsg model
        
        Nothing -> 
            (NotFound session, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    let
         viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        LandingModel mod ->
            viewPage Page.Home LandingMsg (Landing.view mod)
                
        NotFound _ -> 
            viewPage Page.Other (\_ -> Empty) NtFound.view 
        
        Load _ -> 
            viewPage Page.Other (\_ -> Empty) emptyview
        
        RegisterModel mod -> 
            viewPage Page.Register RegisterMsg (Register.view mod)

        LoginModel mod -> 
            viewPage Page.Login LoginMsg (Login.view mod)
        
        SettingModel mod ->
            viewPage Page.Settings SettingMsg (Settings.view mod)
            
toSession : Model -> Session
toSession page =
    case page of
        NotFound session ->
            session

        Load session ->
            session

        LandingModel model ->
            Landing.toSession model
        
        RegisterModel model ->
            Register.toSession model
        
        LoginModel model ->
            Login.toSession model

        SettingModel model ->
            Settings.toSession model
        

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

--empty view 
emptyview : { title : String, content : Html msg }
emptyview = 
    { title = ""
    , content = Html.text ""
    }

