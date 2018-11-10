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
import Animation.Messenger


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
    , style : Animation.Messenger.State Msg
    }


type alias Model =
  { left : Column
  , leftExiting : Maybe Column
  , right : Column
  , rightExiting : Maybe Column
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
  ( Model initialColumn Nothing initialColumn Nothing
  , Cmd.none
  )


type Msg
  = DisposeLeft
  | DisposeRight
  | AddLeft
  | AddRight
  | IgnoreKey
  | AnimateLeftExiting Animation.Msg
  | AnimateRightExiting Animation.Msg
  | DoneLeftExiting
  | DoneRightExiting


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "Message" msg of
    DisposeLeft ->
      ({ model
       | left = initialColumn
       , leftExiting =
         Just
           { toasts = model.left.toasts
           , style = Animation.interrupt
              [ Animation.to [ Animation.marginTop (Animation.px -300), Animation.opacity 0 ]
              , Animation.Messenger.send DoneLeftExiting
              ]
              model.left.style
           }
       }
      , Cmd.none
      )

    DisposeRight ->
      ({ model
       | right = initialColumn
       , rightExiting =
         Just
           { toasts = model.right.toasts
           , style = Animation.interrupt
              [ Animation.to [ Animation.marginTop (Animation.px -300), Animation.opacity 0 ]
              , Animation.Messenger.send DoneRightExiting
              ]
              model.right.style
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

    AnimateLeftExiting anim ->
      case model.leftExiting of
        Just col ->
          let
              (newStyle, cmds) = Animation.Messenger.update anim col.style
          in
            ({ model
             | leftExiting =
               Just { col | style = newStyle }
             }
            , cmds
            )
        Nothing ->
          (model, Cmd.none)

    AnimateRightExiting anim ->
      let
          applyAnim col = { col | style = Animation.update anim col.style }
      in
        ({ model
         | rightExiting =
           Maybe.map applyAnim model.rightExiting
         }
        , Cmd.none
        )

    DoneLeftExiting ->
        ({ model | leftExiting = Nothing }
        , Cmd.none
        )

    DoneRightExiting ->
        ({ model | rightExiting = Nothing }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
  let
      subscribe fn =
        .style >> List.singleton >> Animation.subscription fn >> List.singleton
  in
    [ [ Sub.map keyStringToMsg (onKeyPress containerDecoder) ]
    , Maybe.map (subscribe AnimateLeftExiting) model.leftExiting |> Maybe.withDefault []
    , Maybe.map (subscribe AnimateRightExiting) model.rightExiting |> Maybe.withDefault []
    ]
    |> List.concat
    |> Sub.batch


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
  [ viewColumn (Maybe.withDefault model.left model.leftExiting)
  , viewColumn (Maybe.withDefault model.right model.rightExiting)
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


