module Page.NotFound exposing (view)

import Html exposing (Html, div, text, main_)

-- VIEW 

view : { title : String, content  : Html msg  }
view = 
    { title = "Page Not Found"
    , content = Html.text "Oops Page Not Found"
    }
        