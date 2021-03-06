module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Helper exposing (endPoint, informHttpError, prepareAuthHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Route
import Session exposing (Session, cred)
import Random


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , recommendation = 
                { food = ""
                , protein = 0.0
                , energy = 0.0
                , calories = 0.0
                , carb = 0.0
                , fat = 0.0
                , water = 0.0
                , fibre = 0.0
                }
            , problem = []
            , randNo = 0
            }

        cmd =
            case Session.cred session of
                Just cred ->
                    newNumber

                Nothing ->
                    Route.replaceUrl (Session.navKey session) Route.Login
    in
    ( model, cmd )

getRandomInt : Random.Generator Int
getRandomInt = 
    Random.int 1 3

newNumber : Cmd Msg
newNumber =
  Random.generate NewNumber getRandomInt

type alias Model =
    { session : Session
    , recommendation : Recommendation
    , problem : List Problem
    , randNo : Int
    }


type Problem
    = ServerError String


type Msg
    = GetRecommendation
    | GotRecommendation (Result Http.Error Recommendation)
    | LoggedOut
    | NewNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewNumber x ->
            ({model | randNo = x}, Cmd.none)


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
            ( model, Cmd.batch [ Session.logout, Route.loadPage Route.Login ] )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div [ class "home" ]
            [ 
                viewNavbar model
                ,p [ class "validation-problem" ]
                    (List.map (\str -> viewServerError str) model.problem)
                ,div [class "row"] 
                [
                    div [class "col-lg-6 col-md-12 col-xs-12"]
                    [
                        viewDidyouknow model
                    ]
                    ,div [class "col-lg-6 col-md-12 col-xs-12"]
                    [
                        viewRecommendation model
                    ]
                ]
            ]
    }


viewServerError : Problem -> Html Msg
viewServerError (ServerError str) =
    text str


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav [class "navbar navbar-fixed"]
        [div [class "nav-header"]
            [ a [ class "navbar-toggle"] 
                [span [class "fa fa-bars"] []
                ]
            , a [class "header", href "/"] [text "EatRight"]
            ]
        , div [class "nav", id "nav"]
            [ ul [id "nav-collapse"] [
                li [] [a [href "/settings"] [text "Settings"]]
                , li [] [a [onClick LoggedOut] [text "Logout"]]
            ]
            ]
        ]


viewRecommendation : Model -> Html Msg
viewRecommendation model =
        div [ class "dyk", style "color" "green"] 
            [ 
                h3 [ style "text-align" "center"] [ text "Recommendations"]  
                ,div [ class "content"] 
                [
                    p [] [text model.recommendation.food]
                    , ul []
                        [
                            li [] [text <| "Carbohydrate: " ++ (String.fromFloat model.recommendation.carb ++ " (g)")]
                            , li [] [text <| "Protein: " ++ (String.fromFloat model.recommendation.protein ++ " (g)")]
                            , li [] [text <| "Fat: " ++ (String.fromFloat model.recommendation.fat ++ " (g)")]
                            , li [] [text <| "Calories: " ++ (String.fromFloat model.recommendation.calories ++ " (kCal)")]
                            , li [] [text <| "Energy: " ++ (String.fromFloat model.recommendation.energy ++ " (kJ)")]
                            , li [] [text <| "Water: " ++ (String.fromFloat model.recommendation.water ++ " (g)")]
                            , li [] [text <| "Fibre: " ++ (String.fromFloat model.recommendation.fibre ++ " (g)")]
                        ] 
                    ,button [ class "recommend-button", onClick GetRecommendation ] [ text "Get Recommendation" ] 
                ]
            ]

viewDidyouknow : Model -> Html Msg
viewDidyouknow model =
    div [ class "dyk" , style "display" "block" ]
        [ 
            h3 [ style "text-align" "center" ] [ text "Did You Know!" ]
            ,div [ class "content" ] 
            [
                p [] [ text (List.take model.randNo tips |> List.head |> Maybe.withDefault "") ]
            ]
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
        { headers = [ prepareAuthHeader session ]
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
    , fat : Float
    , energy: Float
    , calories: Float
    , water : Float
    , fibre : Float
    }


decodeRecommendation : Decode.Decoder Recommendation
decodeRecommendation = 
    Decode.map8 Recommendation
        (Decode.field "food" Decode.string)
        (Decode.field "protein" Decode.float)
        (Decode.field "carb" Decode.float)
        (Decode.field "fat" Decode.float)
        (Decode.field "energy" Decode.float)
        (Decode.field "calories" Decode.float)
        (Decode.field "water" Decode.float)
        (Decode.field "fibre" Decode.float)
        


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
