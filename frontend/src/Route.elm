module Route exposing (Route(..), fromUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Landing
    | Home
    | Register
    | Login
    | Settings


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map Register (s "register")
        , Parser.map Login (s "login")
        , Parser.map Home (s "home")
        , Parser.map Settings (s "settings")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toPath route)


toPath : Route -> String
toPath route =
    let
        path =
            case route of
                Landing ->
                    [ "" ]

                Home ->
                    [ "home" ]

                Login ->
                    [ "login" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]
    in
    String.join "/" path
