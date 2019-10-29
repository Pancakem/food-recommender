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
                    Route.pushUrl (Session.navKey session) Route.Home

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
    nav [ class "navbar navbar-default navbar-fixed-top" ]
        [ div [ class "container-fluid" ]
            [ div [ class "navbar-header" ]
                [ a [ class "navbar-brand", href "/" ]
                    [ text "EatRight" ]
                ]
            , ul [ class "nav navbar-nav navbar-right" ]
                [ li [] [ a [ href "/login" ] [ text "Sign In" ] ]
                , li [] [ a [ href "/register" ] [ text "Sign Up" ] ]
                ]
            ]
        ]


viewColumn : Model -> Html Msg
viewColumn model =
    div [ class "row" ]
        [ div [ class "column" ]
            [ img [ src "src/images/olive-oil-968657__340.jpg" ] [] ]
        , div [ class "column" ]
            [ img [ src "src/images/vegetables-752153_960_720.jpg" ] [] ]
        , div []
            [ img [ src "src/images/grapes-690230_960_720.jpg" ] [] ]
        , div []
            [ img [ src "src/images/grapes-690230_960_720.jpg" ] [] ]
        ]


viewPitch : Html msg
viewPitch =
    div [ class "container" ]
        [ text pitch ]


viewMyDiet : Html msg
viewMyDiet =
    div [ class "container" ]
        [ text mydiet ]


viewFooter : Html msg
viewFooter =
    footer [ class "cp-text" ]
        [ h6 [] [ text "EATRIGHT" ]
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
