import Browser
import Html exposing (Html, div, span, ul, li, text)
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


type alias Toast = String

type alias Model =
  { left : List Toast
  , right : List Toast
  }


init : () -> (Model, Cmd Msg)
init flags =
  (Model [] [], Cmd.none)


type Msg
  = DisposeLeft
  | DisposeRight
  | AddLeft
  | AddRight
  | Ignore


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DisposeLeft ->
      ({ model | left = [] }, Cmd.none)

    DisposeRight ->
      ({ model | right = [] }, Cmd.none)

    AddLeft ->
      ({ model | left = List.append model.left ["Left"] }, Cmd.none)

    AddRight ->
      ({ model | right = List.append model.right ["Right"] }, Cmd.none)
 
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
    "q" -> DisposeLeft
    "w" -> DisposeRight
    "z" -> AddLeft
    "x" -> AddRight
    _ -> Ignore


view : Model -> Html Msg
view model =
  div [ style "padding" "10px" ]
    [ viewContainer model.left
    , viewContainer model.right
    ]


viewContainer : List Toast -> Html Msg
viewContainer toasts =
  ul [style "display" "inline-block", style "vertical-align" "top"]
    (List.map (\toast -> li [style "display" "list-item"] [text toast]) toasts)


