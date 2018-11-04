import Browser
import Html exposing (Html, text)

main =
  Browser.sandbox
  { init = init
  , update = update
  , view = view
  }


type alias Model = ()

init : Model
init =
  ()


type alias Msg = ()


update : Msg -> Model -> Model
update msg model =
  ()


view : Model -> Html Msg
view model =
  text "Hello, world!"




