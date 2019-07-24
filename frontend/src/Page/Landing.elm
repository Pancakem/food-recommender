module Page.Landing exposing (Model, Msg, update, view, subscriptions, init, toSession)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Session exposing (..)
import Route
import Bootstrap.Navbar as Navbar
import Bootstrap.Carousel as Carousel
import Bootstrap.Button as Button

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
            , br [] []
            ]
    }

viewNavbar : Model -> Html Msg
viewNavbar model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.brand [ href "/" ] [ text "EatRight" ]
        |> Navbar.items
            [ Navbar.itemLink [ class "navbar-dropdown", href "/login" ] [ text "Sign In" ]
            , Navbar.itemLink [ class "navbar-dropdown", href "/register" ] [ text "Sign Up" ]
            ]
        |> Navbar.view model.navbarState

viewCarousel : Model -> Html Msg
viewCarousel model = 
    Carousel.config CarouselMsg []
        |> Carousel.withIndicators
        |> Carousel.slides
            [] -- put slides in here
        |> Carousel.view model.carouselState

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [Carousel.subscriptions model.carouselState CarouselMsg]

toSession : Model -> Session
toSession model = 
    model.session
