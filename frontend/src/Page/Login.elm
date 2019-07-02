module Page.Login exposing (Model, Msg, init, toSession, view, update, subscriptions) 

import Profile exposing (..)
import Player exposing (..)
import Session exposing (Session)
import Html exposing (..)
import Html.Events exposing(onClick, onInput)
import Html.Attributes exposing (..)
import Route exposing (Route)
import RemoteData exposing (RemoteData)
import Api exposing (..)
import Browser.Navigation as Navigation exposing (load)

type alias Model = 
    { session : Session
    , form : Form
    , problems : List Problem
    , err : RemoteData (Graphql.Http.Error SignIn) SignIn
    }

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String



-- VIEW 
view : Model -> { title : String, content : Html Msg}
view model = 
    { title = "Eat Your Day - Login"
    , content = (loginView model)
    }

init : Session -> (Model, Cmd Msg)
init session =
    ({ session = session
     , problems  = [] 
     , form = 
           { email = ""
           , password = ""
           }
    , err = RemoteData.Loading
    }
    , Cmd.none
    )


type alias Form =
    { email : String
    , password : String
    }


loginView : Model -> Html Msg
loginView model = 
    div [ class "login-page"]
        [ ul []
          (List.map (\str -> viewServerError str) model.problems)
        ,(loginForm model) 
        ]


loginForm : Model -> Html Msg
loginForm model = 
    div [ class "vert-form" ]
        [ viewInput model Email "Email" "text" "forestsquirrel@eyd.com"
        , viewInput model Password "Password" "password" "*******"
        , div [ class "login-button-row"]
              [ button [ class "brown-button button", onClick SubmittedDetails ] [ text "LOGIN" ] ]
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
            li [style "color" "red"] [ text errorMsg ]
 
        else
            text ""


setErrors : List Problem -> Model -> Model
setErrors problems model =
    { model | problems = problems }  

setField : ValidatedField-> String -> Model-> Model
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
        [ class "form-label" ]
        [ text labelName
        , input
            [ type_ inputType
            , placeholder inputName
            , onInput <| SetField formField
            , value what
            ]
            []
 
            , ul [] lis
    ]

type Msg 
    = SubmittedDetails
    | SetField ValidatedField String
    | GotResponse (RemoteData (Graphql.Http.Error SignIn) SignIn)
    | GotSession Session


update : Msg -> Model -> (Model, Cmd Msg)
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
                    ({ model | problems = [] }
                    , (doSignIn model)
                    )
                
                problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        SetField field value->
            ( model
                |> setField field value
                |> setErrors problemList
            , Cmd.none)  
        
        GotResponse resp ->
            let 
                newModel = 
                    { model | err = resp }
            in
            responseHandling newModel

        GotSession session ->
            ( { model | session = session }
            , Navigation.load "/dashboard" 
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
        case List.concatMap(validateField trimmedForm) fieldsToValidate of
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
                if String.isEmpty form.password  then 
                    ["password can't be blank." ]

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


-- GraphQL 
profile : SelectionSet Profile EatYourDay.Object.User 
profile = 
    SelectionSet.succeed Profile
        |> with User.username 
        |> with User.email 
        |> with User.id
    
type alias SignIn =
    { user : Profile 
    , token : String
    }

signin : SelectionSet SignIn EatYourDay.Object.SignIn
signin =
    SelectionSet.succeed SignIn 
        |> with (SignIn.user profile)
        |> with SignIn.token


signInMutation : Model -> SelectionSet SignIn RootMutation 
signInMutation { form } = 
   Mutation.signIn { input = { signInParam = form.email, password = form.password }} signin


doSignIn : Model -> Cmd Msg
doSignIn model =
    (signInMutation model)
    |> Graphql.Http.mutationRequest Api.endPoint
    |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


responseHandling : Model -> (Model, Cmd Msg) 
responseHandling model = 
    case model.err of
        RemoteData.Failure err ->
            let 
                error = 
                    case handleError err of 
                        Just errorMsg ->
                            errorMsg
                        
                        Nothing ->
                            ErrorMessage ""
            in
            ( {model | problems = [ServerError <| asString error] }
            , Cmd.none
            )

        RemoteData.Success successData ->
            (model
            , Session.login successData
            )
        
        _ ->
            (model, Cmd.none)

