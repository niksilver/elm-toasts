import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Browser.Events exposing (onKeyPress)
import Json.Decode as Decode exposing (Decoder)


main =
  Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


type alias Model =
  { left : Bool
  , right : Bool
  }


init : () -> (Model, Cmd Msg)
init flags =
  (Model False True, Cmd.none)


type Msg =
  Left | Right | Ignore


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Left ->
      ({ model | left = not model.left}, Cmd.none)

    Right ->
      ({ model | right = not model.right}, Cmd.none)

    _ -> (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map keyStringToMsg (onKeyPress containerDecoder)


containerDecoder : Decoder String
containerDecoder =
  Decode.field "key" Decode.string


keyStringToMsg : String -> Msg
keyStringToMsg keyString =
  case keyString of
    "q" -> Left
    "w" -> Right
    _ -> Ignore


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



