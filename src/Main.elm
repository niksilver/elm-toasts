import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html)
import Element exposing
  ( Element
  , el, row, column, text
  , layout
  , width, padding, spacing
  , px, rgb
  , centerX, alignTop)
import Element.Background as Background
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
  [ viewContainer model.left
  , viewContainer model.right
  ]
    |> row []
    |> el []
    |> layout []


viewContainer : List Toast -> Element Msg
viewContainer toasts =
  List.map text toasts
    |> List.map (el [padding 30, Background.color (rgb 0.8 0.8 0.8), centerX])
    |> column [width (px 300), padding 30, spacing 20]
    |> el [alignTop]


