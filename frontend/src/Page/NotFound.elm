module Page.NotFound exposing (view)

import Html exposing (Html, div, main_, text)



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content = Html.text "404: Oops Page Not Found"
    }
