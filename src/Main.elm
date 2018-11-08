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
import Animation


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Toast = String


type alias Column =
    { toasts : List Toast
    , style : Animation.State
    }


type alias Model =
  { left : Column
  , right : Column
  }


initialColumn : Column
initialColumn =
  { toasts = []
  , style =
      Animation.style
      [ Animation.marginTop (Animation.px 0)
      , Animation.opacity 1.0
      ]
  }


init : () -> (Model, Cmd Msg)
init flags =
  ( Model initialColumn initialColumn
  , Cmd.none
  )


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
      ({ model | left = initialColumn }, Cmd.none)

    DisposeRight ->
      ({ model | right = initialColumn }, Cmd.none)

    AddLeft ->
      let
          left = model.left
      in
        ({ model | left = { left | toasts = List.append left.toasts ["Left"] }}, Cmd.none)

    AddRight ->
      let
          right = model.right
      in
        ({ model | right = { right | toasts = List.append right.toasts ["Right"] }}, Cmd.none)
 
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
  [ viewColumn model.left
  , viewColumn model.right
  ]
    |> row []
    |> el []
    |> layout []


viewColumn : Column -> Element Msg
viewColumn col =
  List.map text col.toasts
    |> List.map (el [padding 30, Background.color (rgb 0.8 0.8 0.8), centerX])
    |> column [width (px 300), padding 30, spacing 20]
    |> el [alignTop]


