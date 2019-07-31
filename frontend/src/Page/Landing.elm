module Page.Landing exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Session exposing (..)
import Route
import Bootstrap.Navbar as Navbar
import Bootstrap.Carousel as Carousel
import Bootstrap.Button as Button
import Bootstrap.Carousel.Slide as Slide

init : Session -> ( Model, Cmd Msg )
init session =
    let
        (navstate, navCmd) = Navbar.initialState NavbarMsg

        carouselstate = Carousel.initialStateWithOptions Carousel.defaultStateOptions
        model = {
            session = session
            , navbarState = navstate
            , carouselState = carouselstate
            }

        cmd =
            case Session.cred session of
                Just cred ->
                    Route.pushUrl (Session.navKey session) Route.Home

                Nothing ->
                    navCmd
    in
    ( model , cmd )

type alias Model = 
    {session : Session
    , navbarState : Navbar.State
    , carouselState : Carousel.State
    }

type Msg
    = NavbarMsg Navbar.State
    | CarouselMsg Carousel.Msg
    | KeyPress Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of        
        NavbarMsg state ->
            ({model | navbarState = state}, Cmd.none)

        CarouselMsg submsg ->
            ( { model | carouselState = Carousel.update submsg model.carouselState }
            , Cmd.none
            )
        
        KeyPress keycode ->
            if keycode == 39 then -- right arrow
                ({ model | carouselState = Carousel.next model.carouselState }
                , Cmd.none
                )

            else if keycode == 37 then -- left arrow
                ({ model | carouselState = Carousel.prev model.carouselState }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

view : Model -> { title : String, content  : Html Msg  }
view model =
    { title = "Landing Page"
    , content = 
        div []
            [ 
            viewNavbar model 
            , viewCarousel model
            , section [class "deal"] [
                div[] [h3 [class "title-h"] [ text "What's Our Deal?"] ]
                , viewPitch
                ]
            , section [ class "diet"] [
                div [] [h3 [class "title-h"] [text "My Diet, My Locality"]]
                , viewMyDiet
            ]
            , section [class "footer-sect"] [
                viewFooter
            ]
            ]
    }

viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ class "nav-menu-logo", href "/" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/login" ] [ text "Sign In" ]
            , Navbar.itemLink [ class "navbar-dropdown nav-links nav-menu", href "/register" ] [ text "Sign Up" ]
            ]
        |> Navbar.view model.navbarState

viewCarousel : Model -> Html Msg
viewCarousel model = 
    Carousel.config CarouselMsg []
    |> Carousel.slides
        [ Slide.config [] (Slide.image [] "src/images/olive-oil-968657__340.jpg")
        , Slide.config [] (Slide.image [] "src/images/vegetables-752153_960_720.jpg")
        , Slide.config [] (Slide.image [] "src/images/grapes-690230_960_720.jpg")
        ]
    |> Carousel.view model.carouselState

viewPitch : Html msg
viewPitch = 
    div []
        [ text pitch ]

viewMyDiet : Html msg
viewMyDiet = 
    div []
        [ text mydiet ]

viewFooter : Html msg
viewFooter = 
    footer [ class "cp-text" ]
        [  h6 []  [ text "EATRIGHT" ],
            text "What is EatRight"
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
    Sub.batch [Carousel.subscriptions model.carouselState CarouselMsg]

toSession : Model -> Session
toSession model = 
    model.session


pitch = """The EatRight platform shows you food groups targets-what and how much to each within your calorie allowance. Your food plan is 
            personalized, based on your age, sex, height, and physical activity level. Use EatRight now."""

mydiet = """What foods, flavors and recipes is your town/city known for? Do they meet your dietary requirements? The new EatRight Platform is full
        of nutritious food groups to help you meet your dietary requirements, customized to your area and the foods available within your reach."""