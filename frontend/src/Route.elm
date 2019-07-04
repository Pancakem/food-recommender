module Route exposing (Route(..), fromUrl, replaceUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)

type Route = 
    Landing
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


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route = 
    Nav.replaceUrl key (toPath route)

toPath : Route -> String
toPath route = 
    let
        path = 
            case route of 
                Landing ->
                    [""]

                Home ->
                    ["home"]
                
                Login -> 
                    ["login"]
                
                Register -> 
                    ["register"]
                
                Settings ->
                    ["settings"]
    in
    String.join "/" path