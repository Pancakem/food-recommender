module Page.Settings exposing (Model, Msg(..), Settings, init, toSession, view, update, subscriptions)

import Array as Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Decode
import Route
import Session exposing (Session)
import RemoteData exposing (..)
import Http


type alias Model =
    { session : Session
    , problem : List Problem
    , form : Form
    }


type Problem
    = ServerError String


type alias Form =
    { email : String
    , profileName : String
    , password : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        cmd =
            case Session.cred session of
                Just cred ->
                    -- Cmd.batch [makeAccountQuery cred, makeSettingsQuery cred]
                    Cmd.none -- get current settings

                Nothing ->
                    Route.replaceUrl (Session.navKey session) Route.Login
    in
    ( { session = session
      , form =
            { email = ""
            , profileName = ""
            , password = ""            
            }
      , problem = []
      }
    , cmd
    )



-- UPDATE

type Msg
    = GotAccountInfo (RemoteData Http.Error Account) 
    | GotSettingsInfo (RemoteData Http.Error Settings)
    | SubmitAccount
    | SetField Field String


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
    div [ class "settings-container" ]
        [ ul [] (List.map (\str -> viewError str) model.problem)
        ]

viewError : Problem -> Html msg
viewError (ServerError str) = 
    div[ style "color" "red" ]
        [ text str ]

type Field
    = Email
    | Password
    | ProfileName

inputField : Field -> Model -> String -> String -> String -> Html Msg
inputField field model plceholder lbel taype =
    let
        val = 
            case field of
                Email -> 
                    model.form.email
                
                ProfileName ->
                    model.form.profileName
                
                _ -> 
                    ""
    in
    div [ class "" ]
        [ span [] [label [class "task-form-input-title"] [ text lbel ], img [class "task-form-edit-icon", src "../images/edit.png"] []]
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
