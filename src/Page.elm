module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (..)

type Page = 
    Home
    | Blog
    | Other
    | SearchResult

view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title
    , body = content :: []
    }