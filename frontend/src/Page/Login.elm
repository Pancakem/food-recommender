module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Navigation as Navigation exposing (load)
import Helper exposing (Response, decodeResponse, endPoint, informHttpError)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Url.Builder as Builder
import User exposing (Profile)


init : Session -> ( Model, Cmd Msg )
init session =
    let
        cmd =
            case Session.cred session of
                Just cred ->
                    Route.pushUrl (Session.navKey session) Route.Home

                Nothing ->
                    Cmd.none
    in
    ( { session = session
      , problems = []
      , form =
            { email = ""
            , password = ""
            }
      }
    , cmd
    )


type alias Model =
    { session : Session
    , form : Form
    , problems : List Problem
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Eat Right - Login"
    , content = loginView model
    }


type alias Form =
    { email : String
    , password : String
    }


loginView : Model -> Html Msg
loginView model =
    div [ class "container" ]
        [ div [ class "card .login-card" ]
            [ div [ class "card-body" ]
                [ p [ class "validation-problem" ]
                    (List.map (\str -> viewServerError str) model.problems)
                , loginForm model
                ]
            ]
        ]


loginForm : Model -> Html Msg
loginForm model =
    div [ class "login-form" ]
        [ h3 [ class "card-title" ] [ text "Login" ]
        , div []
            [ div [ class "form-group" ]
                [ div [] [ viewInput model Email "Email" "text" "johndoe@example.com" ]
                , div [] [ viewInput model Password "Password" "password" "*******" ]
                , div []
                    [ button [ class "btn btn-primary", onClick SubmittedDetails ] [ text "LOGIN" ] ]
                ]
            ]
        ]


viewServerError : Problem -> Html Msg
viewServerError problem =
    case problem of
        ServerError str ->
            text str

        _ ->
            text ""


viewProblem : Model -> ValidatedField -> Problem -> Html msg
viewProblem model formfield problem =
    let
        errorMsg =
            case problem of
                InvalidEntry f str ->
                    if f == formfield then
                        str

                    else
                        ""

                ServerError _ ->
                    ""
    in
    if String.length errorMsg > 1 then
        p [ class "validation-problem" ] [ text errorMsg ]

    else
        text ""


setErrors : List Problem -> Model -> Model
setErrors problems model =
    { model | problems = problems }


setField : ValidatedField -> String -> Model -> Model
setField field value model =
    case field of
        Email ->
            updateForm (\form -> { form | email = value }) model

        Password ->
            updateForm (\form -> { form | password = value }) model


viewInput : Model -> ValidatedField -> String -> String -> String -> Html Msg
viewInput model formField labelName inputType inputName =
    let
        what =
            case formField of
                Email ->
                    model.form.email

                Password ->
                    model.form.password

        lis =
            List.map (\err -> viewProblem model formField err) model.problems
    in
    label
        [ class "" ]
        [ text labelName
        , input
            [ type_ inputType
            , placeholder inputName
            , onInput <| SetField formField
            , value what
            , class "form-control"
            ]
            []
        , ul [] lis
        ]


type Msg
    = SubmittedDetails
    | SetField ValidatedField String
    | GotResponse (Result Http.Error Response)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        problemList =
            validate model.form
    in
    case msg of
        SubmittedDetails ->
            case problemList of
                [] ->
                    -- send data to the server
                    -- receive server response
                    -- trigger command to handle or consume response
                    ( { model | problems = [] }
                    , login model
                    )

                problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        SetField field value ->
            ( model
                |> setField field value
                |> setErrors problemList
            , Cmd.none
            )

        GotResponse resp ->
            case resp of
                Ok successData ->
                    ( model
                    , Cmd.batch [ Session.login successData, Route.pushUrl (Session.navKey model.session) Route.Home ]
                    )

                Err err ->
                    let
                        errorMsg =
                            informHttpError err
                    in
                    ( { model | problems = [ ServerError errorMsg ] }
                    , Cmd.none
                    )

        GotSession session ->
            ( { model | session = session }
            , Navigation.load "/home"
            )



-- tiny helper function to update the form in the session


updateForm : (Form -> Form) -> Model -> Model
updateForm transform model =
    { model | form = transform model.form }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- Handle the Form


type ValidatedField
    = Email
    | Password



-- To ensure we only trim the form when the user is done


type TrimmedForm
    = Trimmed Form


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]



-- trim and validate form


validate : Form -> List Problem
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            []

        problems ->
            problems



-- validateField


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else if String.length form.password < 8 then
                    [ "password should be at least 8 characters long." ]

                else
                    []



-- trimFields on demand


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }



-- Exported Session Builder


toSession : Model -> Session
toSession model =
    model.session



-- http


login : Model -> Cmd Msg
login model =
    Http.request
        { url = endPoint [ "auth", "login" ]
        , headers = [ Http.header "Origin" "http://localhost:5000" ]
        , body = Http.jsonBody (encodeLogin model)
        , expect = Http.expectJson GotResponse decodeResponse
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        }


encodeLogin : Model -> Encode.Value
encodeLogin { form } =
    Encode.object
        [ ( "email", Encode.string form.email )
        , ( "password", Encode.string form.password )
        ]
