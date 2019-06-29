module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Browser
import Browser.Navigation exposing (..)
import Url
import Page
import Route
import Page.Landing as Landing
import Page.NotFound as NtFound

init : () -> Url.Url -> Key -> (Model, Cmd Msg)
init _ url _ = 
    changeRouteTo (Route.fromUrl url) <| Load

main : Program () Model Msg
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
    NotFound
    | Load
    | LandingModel Landing.Model

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Empty
    | LandingMsg Landing.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (LandingMsg subMsg, LandingModel landing)  ->
            Landing.update subMsg landing 
                |> updateWith LandingModel LandingMsg model

        (LinkClicked _, _) ->
            (model, Cmd.none)

        (UrlChanged _, _) ->
            (model, Cmd.none)
        
        (_, _) ->
            (model, Cmd.none)

changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of        
        Just Route.Landing ->
            Landing.init 
                |> updateWith LandingModel LandingMsg model
        
        Nothing -> 
            (NotFound, Cmd.none)

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
                
        NotFound -> 
            viewPage Page.Other (\_ -> Empty) NtFound.view 
        
        Load -> 
            viewPage Page.Other (\_ -> Empty) emptyview
            

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

