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
  | IgnoreKey
  | AnimateLeft Animation.Msg
  | AnimateRight Animation.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DisposeLeft ->
      ({ model
       | left =
         { toasts = model.left.toasts
         , style = Animation.interrupt [Animation.to [Animation.marginTop (Animation.px -300), Animation.opacity 0]] model.left.style
         }
       }
      , Cmd.none
      )

    DisposeRight ->
      ({ model
       | right =
         { toasts = model.right.toasts
         , style = Animation.interrupt [Animation.to [Animation.marginTop (Animation.px -300), Animation.opacity 0]] model.right.style
         }
       }
      , Cmd.none
      )

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
 
    IgnoreKey -> (model, Cmd.none)

    AnimateLeft anim ->
      let
          left = model.left
      in
        ({ model
         | left =
           { left | style = Animation.update anim left.style
           }
         }
        , Cmd.none
        )

    AnimateRight anim ->
      let
          right = model.right
      in
        ({ model
         | right =
           { right | style = Animation.update anim right.style
           }
         }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ Sub.map keyStringToMsg (onKeyPress containerDecoder)
  , Animation.subscription AnimateLeft [ model.left.style ]
  , Animation.subscription AnimateRight [ model.right.style ]
  ]


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
    _ -> IgnoreKey


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
    |> column
      (List.append
        [width (px 300), padding 30, spacing 20]
        (List.map Element.htmlAttribute (Animation.render col.style))
      )
    |> el [alignTop]


