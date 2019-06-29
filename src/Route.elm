module Route exposing (Route(..), fromUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)

type Route = 
    Landing

parser : Parser (Route -> a) a
parser = 
    oneOf
        [ Parser.map Landing Parser.top
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
                    [""]
    in
    String.join "/" path