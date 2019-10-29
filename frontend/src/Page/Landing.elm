module Page.Landing exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session exposing (..)


init : Session -> ( Model, Cmd Msg )
init session =
    let
        cmd =
            case Session.cred session of
                Just cred ->
                    Cmd.batch 
                        [ Route.loadPage Route.Home]

                Nothing ->
                    Cmd.none
    in
    ( { session = session }, cmd )


type alias Model =
    { session : Session
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Landing Page"
    , content =
        div []
            [ viewNavbar model
            , viewColumn model
            , section [ class "deal" ]
                [ div [] [ h3 [ class "title-h" ] [ text "What's Our Deal?" ] ]
                , viewPitch
                ]
            , section [ class "diet" ]
                [ div [] [ h3 [ class "title-h" ] [ text "My Diet, My Locality" ] ]
                , viewMyDiet
                ]
            , section [ class "footer-sect" ]
                [ viewFooter
                ]
            ]
    }


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav [class "navbar navbar-fixed"]
        [div [class "nav-header"]
            [ 
                img [src "src/images/icon.png"] []
                ,a [class "header", href "/"] [text "EatRight"]
            ]
        , div [class "nav", id "nav"]
            [ ul [id "nav-collapse"] [
                li [] [a [href "/login"] [text "Login"]]
                , li [] [a [href "/register"] [text "Register"]]
            ]
            ]
        ]


viewColumn : Model -> Html Msg
viewColumn model =
    div [class "landing"]
    [
        div [ class "overlay"] 
        [
            h1 [] [ text "EAT RIGHT" ]
            ,p [] [ text """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla aliquet finibus sagittis. Quisque id
                    eros erat. Sed ac maximus est."""]
        ]
        ,div [ class "row images" ]
            [ 
                div [ class "col-lg-6 col-md-6 col-xs-12" ]
                    [ img [ src "src/images/vegetables-752153_960_720.jpg" ] [] ]
                , div [ class "col-lg-6 col-md-6 col-xs-12 togo" ]
                    [ img [ src "src/images/olive-oil-968657__340.jpg" ] [] ]
            ]
    ]



viewPitch : Html msg
viewPitch =
    p [ class "" ]
        [ text pitch ]


viewMyDiet : Html msg
viewMyDiet =
    p [ class "" ]
        [ text mydiet ]


viewFooter : Html msg
viewFooter =
    footer [ class "cp-text" ]
        [ h3 [] [ text "EATRIGHT" ]
        , text "What is EatRight"
        , br [] []
        , text "Fruits"
        , br [] []
        , text "Protein Food"
        , br [] []
        , text "Dairy"
        , br [] []
        , text "Oils"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession model =
    model.session


pitch =
    """The EatRight platform shows you food groups targets-what and how much to each within your calorie allowance. 
            Your food plan is personalized, based on your age, sex, height, and physical activity level. 
            Use EatRight now."""


mydiet =
    """What foods, flavors and recipes is your town/city known for? 
            Do they meet your dietary requirements? The new EatRight Platform is full
            of nutritious food groups to help you meet your dietary requirements, 
            customized to your area and the foods available within your reach."""
