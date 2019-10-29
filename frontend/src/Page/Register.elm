module Page.Register exposing (Model, Msg, init, toSession, update, view)

import Browser.Navigation as Navigation exposing (load)
import Helper exposing (Response, decodeResponse, endPoint, informHttpError)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Regex exposing (..)
import Route exposing (Route)
import Session exposing (..)
import User exposing (Profile)


init : Session -> ( Model, Cmd msg )
init session =
    let
        cmd =
            case Session.cred session of
                Just cred ->
                    Route.replaceUrl (Session.navKey session) Route.Home

                Nothing ->
                    Cmd.none
    in
    ( { session = session
      , problems = []
      , form =
            { email = ""
            , username = ""
            , age = 0
            , gender = 0
            , preference = ""
            , password = ""
            , passwordAgain = ""
            }
      , showPassword = False
      , showErrors = False
      }
    , cmd
    )



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    , showPassword : Bool
    , showErrors : Bool
    }


type alias Form =
    { email : String
    , username : String
    , age : Int
    , gender : Int
    , preference : String
    , password : String
    , passwordAgain : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content = div [class "register-page"] 
                    [registerView model]
    }


registerView : Model -> Html Msg
registerView model =
    div [ class "register-form" ]
        [ p [ class "validation-problem" ] (List.map (\str -> viewServerError str) model.problems)
        , viewForm model
        ]


viewServerError : Problem -> Html Msg
viewServerError problem =
    case problem of
        ServerError str ->
            text str

        _ ->
            text ""


toGender : String -> Int
toGender str =
    case str of
        "Male" ->
            1

        "Female" ->
            2

        "Prefer not to say" ->
            3

        _ ->
            0


viewForm : Model -> Html Msg
viewForm model =
    div [ class "form-control" ]
        [ p [ class "" ] [ text "Eat Healthy." ]
        , p [ class "field-set-label" ] [ text "Register Account" ]
        , viewInput model Email "Email Address" "text" "johndoe@example.com"
        , viewInput model Username "Full Name" "text" "John Doe"
        , viewInput model Age "Age" "text" "18"
        , div []
            [ label [] [text "Pick Gender"] 
            ,div [ class "form-check-inline" ]
                [ input
                    [ type_ "radio"
                    , name "gender-choice"
                    , class "form-check-input"
                    , onCheck <| SetChecked "Male" Gender
                    ]
                    [] 
                ,  text "Male"
                ]
            , div [ class "form-check-inline" ]
                [ input
                    [ type_ "radio"
                    , name "gender-choice"
                    , class "form-check-input"
                    , onCheck <| SetChecked "Female" Gender
                    ]
                    []
                , text "Female"
                ]
            , div [ class "form-check-inline" ]
                [ input
                    [ type_ "radio"
                    , name "gender-choice"
                    , onCheck <| SetChecked "Prefer Not To Say" Gender
                    , class "form-check-input"
                    ]
                    [ ]
                , text "Prefer Not To Say"
                ]
            ]
        , div []
            [ label [] [text "Pick Dietary Preference"] 
            ,div [ class "form-check-inline" ]
                [ input
                    [ type_ "radio"
                    , name "deitary-choice"
                    , class "form-check-input"
                    , onCheck <| SetChecked "vegan" Preference
                    ]
                    []
                , text "Vegan"
                ]
            , div [ class "form-check-inline" ]
                [ input
                    [ type_ "radio"
                    , name "deitary-choice"
                    , class "form-check-input"
                    , onCheck <| SetChecked "vegetarian" Preference
                    ]
                    []
                , text "Vegetarian"
                ]
            , div [ class "form-check-inline" ]
                [ input
                    [ type_ "radio"
                    , name "deitary-choice"
                    , onCheck <| SetChecked "mixed_food" Preference
                    , class "form-check-input"
                    ]
                    []
                , text "Mixed Food"
                ]
            ]
        , viewInput model Password "Password" "password" "********"
        , viewInput model PasswordAgain "Confirm Password" "password" "********"
        , div [ class "login-button-row" ]
            [ button [ class "btn btn-primary", onClick SubmittedForm ] [ text "Join" ] ]
        ]


viewInput : Model -> ValidatedField -> String -> String -> String -> Html Msg
viewInput model formField labelName inputType inputName =
    let
        what =
            case formField of
                Username ->
                    model.form.username

                Email ->
                    model.form.email

                Password ->
                    model.form.password

                PasswordAgain ->
                    model.form.passwordAgain

                Age ->
                    String.fromInt model.form.age

        lis =
            List.map (\err -> viewProblem model formField err) model.problems
    in
    div []
        [ label
            [ class "" ]
            [ text labelName
            , input
                [ if formField == Password && model.showPassword then
                    type_ "text"

                  else
                    type_ inputType
                , placeholder inputName
                , onInput <| SetField formField
                , value what
                , class "form-control"
                ]
                []
            , ul [] lis
            ]
        ]


viewProblem : Model -> ValidatedField -> Problem -> Html msg
viewProblem model formfield problem =
    if model.showErrors then
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
            p [ style "color" "red" ] [ text errorMsg ]

        else
            text ""

    else
        ul [ class "formErrors" ] []


type Msg
    = SubmittedForm
    | SetField ValidatedField String
    | ToggleShowPassword
    | GotResponse (Result Http.Error Response)
    | SetChecked String CheckedField Bool

type CheckedField =
    Preference
    | Gender

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        problemList =
            validate model

        result =
            doRegister model
    in
    case msg of
        SetChecked str f bool->
            let
                val = if bool then str else "" 

                newModel =
                    case f of
                        Gender ->
                            updateForm (\form -> {form | gender = toGender val}) model
                    
                        Preference ->
                            updateForm (\form -> {form | preference = val}) model
                            
            in
            (newModel, Cmd.none)

        SubmittedForm ->
            case validate model of
                [] ->
                    ( { model | problems = [] }
                    , result
                    )

                problems ->
                    ( { model | problems = problems, showErrors = True }
                    , Cmd.none
                    )

        SetField field value ->
            ( model
                |> setField field value
                |> setErrors problemList
            , Cmd.none
            )

        ToggleShowPassword ->
            ( { model | showPassword = not model.showPassword }, Cmd.none )

        GotResponse signUpResp ->
            case signUpResp of
                Ok successData ->
                    ( model
                    , Cmd.batch [ Session.login successData, Route.loadPage Route.Home ]
                    )

                Err err ->
                    let
                        errorMsg =
                            informHttpError err
                    in
                    ( { model | problems = [ ServerError errorMsg ] }
                    , Cmd.none
                    )


updateForm : (Form -> Form) -> Model -> Model
updateForm transform model =
    { model | form = transform model.form }


setErrors : List Problem -> Model -> Model
setErrors problems model =
    { model | problems = problems }


type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Username
    | Email
    | Age
    | Password
    | PasswordAgain


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    ]


validate : Model -> List Problem
validate model =
    let
        trimmedForm =
            trimFields model.form
    in
    case List.concatMap (validatedField trimmedForm) fieldsToValidate of
        [] ->
            []

        problems ->
            problems


setField : ValidatedField -> String -> Model -> Model
setField field value model =
    case field of
        Username ->
            updateForm (\form -> { form | username = value }) model

        Email ->
            updateForm (\form -> { form | email = value }) model

        Password ->
            updateForm (\form -> { form | password = value }) model

        PasswordAgain ->
            updateForm (\form -> { form | passwordAgain = value }) model

        Age ->
            updateForm (\form -> { form | age = String.toInt value |> Maybe.withDefault 0 }) model


validatedField : TrimmedForm -> ValidatedField -> List Problem
validatedField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.username then
                    [ "name can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else if String.contains "@" form.email |> not then
                    [ "please enter a valid email." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else if String.length form.password < 8 then
                    [ "password must be at least 8 characters." ]

                else if not (form.passwordAgain == form.password) then
                    [ "passwords must match." ]

                else if not <| checkSymbol form.password then
                    [ "password should have at least one symbol." ]

                else
                    []

            PasswordAgain ->
                if String.isEmpty form.passwordAgain then
                    [ "confirm password can't be empty." ]

                else
                    []

            _ ->
                []


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , email = String.trim form.email
        , age = form.age
        , gender = form.gender
        , preference = form.preference
        , password = String.trim form.password
        , passwordAgain = String.trim form.passwordAgain
        }


toSession : Model -> Session
toSession model =
    model.session


checkSymbol : String -> Bool
checkSymbol password =
    let
        reg =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-zA-Z0-9 ]*$"

        listmatch =
            Regex.find reg password
    in
    case listmatch of
        [] ->
            False

        matches ->
            True



-- http


doRegister : Model -> Cmd Msg
doRegister model =
    Http.request
        { url = endPoint [ "auth", "register" ]
        , body = Http.jsonBody (encodeRegister model)
        , expect = Http.expectJson GotResponse decodeResponse
        , headers = []
        , method = "POST"
        , tracker = Nothing
        , timeout = Nothing
        }


encodeRegister : Model -> Encode.Value
encodeRegister { form } =
    Encode.object
        [ ( "email", Encode.string form.email )
        , ( "fullname", Encode.string form.username )
        , ( "age", Encode.int form.age )
        , ( "gender", Encode.int form.gender )
        , ( "preference", Encode.string form.preference)
        , ( "password", Encode.string form.password )
        ]
