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


type Position = Left | Right


type Msg
  = Dispose Position
  | Add Position
  | IgnoreKey
  | AnimateExiting Position Animation.Msg
  | DoneLeftExiting
  | DoneRightExiting


mainColumn : Model -> Position -> Column
mainColumn model pos =
  case pos of
    Left -> model.left
    Right -> model.right


doneMsg : Position -> Msg
doneMsg pos =
  case pos of
    Left -> DoneLeftExiting
    Right -> DoneRightExiting


setMainColumn : Position -> Column -> Model -> Model
setMainColumn pos col model =
  case pos of
    Left -> { model | left = col }
    Right -> { model | right = col }


setExitingColumn : Position -> Maybe Column -> Model -> Model
setExitingColumn pos maybeCol model =
  case pos of
    Left -> { model | leftExiting = maybeCol }
    Right -> { model | rightExiting = maybeCol }


addCmds : Cmd Msg -> Model -> (Model, Cmd Msg)
addCmds cmds model =
  (model, cmds)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Dispose pos ->
      let
          mainCol = mainColumn model pos
      in
          model
          |> setMainColumn pos initialColumn
          |> setExitingColumn pos
           (Just
             { toasts = mainCol.toasts
             , style = Animation.interrupt
                [ Animation.to [ Animation.marginTop (Animation.px -300), Animation.opacity 0 ]
                , Animation.Messenger.send (doneMsg pos)
                ]
                mainCol.style
             }
           )
         |> addCmds Cmd.none

    Add pos ->
      let
          mainCol = mainColumn model pos
          content = if pos == Left then "Left" else "Right"
      in
          model
          |> setMainColumn pos { mainCol | toasts = List.append mainCol.toasts [content] }
          |> addCmds Cmd.none

    IgnoreKey -> (model, Cmd.none)

    AnimateExiting Left anim ->
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

    AnimateExiting Right anim ->
      case model.rightExiting of
        Just col ->
          let
              (newStyle, cmds) = Animation.Messenger.update anim col.style
          in
            ({ model
             | rightExiting =
               Just { col | style = newStyle }
             }
            , cmds
            )
        Nothing ->
          (model, Cmd.none)

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
    , Maybe.map (subscribe (AnimateExiting Left)) model.leftExiting |> Maybe.withDefault []
    , Maybe.map (subscribe (AnimateExiting Right)) model.rightExiting |> Maybe.withDefault []
    ]
    |> List.concat
    |> Sub.batch


containerDecoder : Decoder String
containerDecoder =
  Decode.field "key" Decode.string


keyStringToMsg : String -> Msg
keyStringToMsg keyString =
  case keyString of
    "q" -> Dispose Left
    "w" -> Dispose Right
    "z" -> Add Left
    "x" -> Add Right
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


