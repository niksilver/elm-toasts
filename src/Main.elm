import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)


main =
  Browser.sandbox
  { init = init
  , update = update
  , view = view
  }


type alias Model =
  { left : Bool
  , right : Bool
  }


init : Model
init =
  Model False True


type alias Msg = ()


update : Msg -> Model -> Model
update msg model =
  model


view : Model -> Html Msg
view model =
  div [ style "padding" "10px" ]
    [ viewContainer model.left
    , viewContainer model.right
    ]


viewContainer : Bool -> Html Msg
viewContainer show =
  case show of
    True ->
      span [ style "padding" "10px" ]
        [ text "Here I am!"
        ]
    False ->
      span [ style "padding" "10px" ]
        [ text "Nothing to see"
        ]



