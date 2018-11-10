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


exitingColumn : Model -> Position -> Maybe Column
exitingColumn model pos =
  case pos of
    Left -> model.leftExiting
    Right -> model.rightExiting


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

    AnimateExiting pos anim ->
      let
          exCol = exitingColumn model pos
      in
          case exCol of
            Just col ->
              let
                  (newStyle, cmds) = Animation.Messenger.update anim col.style
              in
                  model
                  |> setExitingColumn pos (Just { col | style = newStyle })
                  |> addCmds cmds
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
      -- Given a position,
      -- produce a function that takes that exiting column and produces an animation subscription.
      -- That subscription is put in a list for later concatenation.

      subscribe : Position -> (Column -> List (Sub Msg))
      subscribe pos =
        .style >> List.singleton >> Animation.subscription (AnimateExiting pos) >> List.singleton

      -- The left or right exiting column.

      exitingCol : Position -> Maybe Column
      exitingCol pos = exitingColumn model pos

      -- Turn a Nothing into [] and a Just x into [x]

      maybeToList m = Maybe.withDefault [] m

      -- Given a position, produce a list of animation subscriptions for it

      subscriptionsToExiting : Position -> List (Sub Msg)
      subscriptionsToExiting pos =
        Maybe.map (subscribe pos) (exitingCol pos) |> maybeToList
  in
    [ [ Sub.map keyStringToMsg (onKeyPress containerDecoder) ]
    , subscriptionsToExiting Left
    , subscriptionsToExiting Right
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


