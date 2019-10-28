module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Navbar as Navbar
import Helper exposing (endPoint, informHttpError, prepareAuthHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Route
import Session exposing (Session, cred)


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( navstate, navCmd ) =
            Navbar.initialState NavbarMsg

        dropdownstate =
            Dropdown.initialState

        model =
            { session = session
            , navbarState = navstate
            , recommendation = 
                { food = ""
                , protein = 0.0
                , energy = 0.0
                , calories = 0.0
                , carb = 0.0
                , vitamin = 0.0
                }
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
    ( model, cmd )


type alias Model =
    { session : Session
    , recommendation : Recommendation
    , navbarState : Navbar.State
    , dropdownState : Dropdown.State
    , problem : List Problem
    }


type Problem
    = ServerError String


type Msg
    = NavbarMsg Navbar.State
    | GetRecommendation
    | GotRecommendation (Result Http.Error Recommendation)
    | LoggedOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        GetRecommendation ->
            ( model, getRecommendation model.session )

        GotRecommendation resp ->
            let
                newModel =
                    case resp of
                        Ok data ->
                            { model | recommendation = data }

                        Err er ->
                            { model | problem = [ ServerError <| informHttpError er ] }
            in
            ( newModel, Cmd.none )

        LoggedOut ->
            ( model, Cmd.batch [ Session.logout, Route.pushUrl (Session.navKey model.session) Route.Login ] )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div []
            [ viewNavbar model
            , p [ class "validation-problem" ]
                (List.map (\str -> viewServerError str) model.problem)
            , h3 [ style "text-align" "center" ] [ text "Did You Know!" ]
            , viewDidyouknow
            , br [] []
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
            , Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", onClick LoggedOut ] [ text "Logout" ]
            ]
        |> Navbar.view model.navbarState


viewRecommendation : Model -> Html Msg
viewRecommendation model =
    div [ style "text-align" "center" ]
        [
        div [style "color" "green"] 
            [ p [] [text model.recommendation.food]
            , ul []
                [
                    li [] [text <| "Carbohydrate: " ++ (String.fromFloat model.recommendation.carb)]
                    , li [] [text <| "Protein: " ++ (String.fromFloat model.recommendation.protein)]
                    , li [] [text <| "Vitamin: " ++ (String.fromFloat model.recommendation.vitamin)]
                    , li [] [text <| "Calories: " ++ (String.fromFloat model.recommendation.calories)]
                    , li [] [text <| "Energy: " ++ (String.fromFloat model.recommendation.energy)]
                ] 
            ]
        , br [] []
        , div []
            [ button [ class "recommend-button", onClick GetRecommendation ] [ text "Get Recommendation" ] ]
        ]


viewDidyouknow =
    div [ style "display" "block" ]
        [ p [] [ text (List.head tips |> Maybe.withDefault "") ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession { session } =
    session


getRecommendation : Session -> Cmd Msg
getRecommendation session =
    Http.request
        { headers = [ prepareAuthHeader session, Http.header "Origin" "http://localhost:5000" ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson GotRecommendation decodeRecommendation
        , url = endPoint [ "recommend" ]
        , body = Http.emptyBody
        }


type alias Recommendation =
    { food : String
    , protein : Float
    , carb : Float
    , vitamin : Float
    , energy: Float
    , calories: Float
    }


decodeRecommendation : Decode.Decoder Recommendation
decodeRecommendation = 
    Decode.map6 Recommendation
        (Decode.field "food" Decode.string)
        (Decode.field "protein" Decode.float)
        (Decode.field "carb" Decode.float)
        (Decode.field "vitamin" Decode.float)
        (Decode.field "energy" Decode.float)
        (Decode.field "calories" Decode.float)


tips : List String
tips =
    [ """Don’t drink sugar calories. Sugary drinks are among the most fattening items you can put into your body. 
    This is because your brain doesn’t measure calories from liquid sugar the same way it does for solid food.
    Therefore, when you drink soda, you end up eating more total calories."""
    , """Eat nuts. Despite being high in fat, nuts are incredibly nutritious and healthy. 
        They’re loaded with magnesium, vitamin E, fiber, and various other nutrients.
        Studies demonstrate that nuts can help you lose weight and may help fight type 2 diabetes and heart disease. 
        Additionally, your body doesn’t absorb 10–15% of the calories in nuts. 
        Some evidence also suggests that this food can boost metabolism. 
        In one study, almonds were shown to increase weight loss by 62%, compared with complex carbs."""
    , """Avoid processed junk food (eat real food instead). Processed junk food is incredibly unhealthy.
        These foods have been engineered to trigger your pleasure centers, so they trick your brain into overeating — even promoting food addiction in some people.
        They’re usually low in fiber, protein, and micronutrients but high in unhealthy ingredients like added sugar and refined grains. 
        Thus, they provide mostly empty calories."""
    ]
