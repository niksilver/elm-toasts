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
import Element.Border as Border
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


type alias Toast =
  { message : String
  , style : Animation.Messenger.State Msg
  }


type alias Column =
    { toasts : List Toast
    , style : Animation.Messenger.State Msg
    }


type alias Model =
  { left : Column
  , leftExiting : Maybe Column
  , right : Column
  , rightExiting : Maybe Column
  , toastCount : Int
  }


emptyColumn : Column
emptyColumn =
  { toasts = []
  , style =
      Animation.style
      [ Animation.marginTop (Animation.px 0)
      , Animation.opacity 1.0
      ]
  }


init : () -> (Model, Cmd Msg)
init flags =
  ( { left = emptyColumn
    , leftExiting = Nothing
    , right = emptyColumn
    , rightExiting = Nothing
    , toastCount = 0
    }
  , Cmd.none
  )


type Position = Left | Right


type Msg
  = Dispose Position
  | Add Position
  | IgnoreKey
  | AnimateExiting Position Animation.Msg
  | DoneExiting Position


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


incrementToastCount : Model -> Model
incrementToastCount model =
  { model
  | toastCount = model.toastCount + 1
  }

setMainColumn : Position -> Column -> Model -> Model
setMainColumn pos col model =
  case pos of
    Left -> { model | left = col }
    Right -> { model | right = col }


newToast : String -> Toast
newToast message =
  { message = message
  , style = Animation.style []
  }

appendToast : Position -> Model -> Model
appendToast pos model =
  let
      mainCol = mainColumn model pos
      message =
        if pos == Left then
          String.append "Left " (String.fromInt model.toastCount)
        else
          String.append "Right " (String.fromInt model.toastCount)
      toast = newToast message
  in
      model
      |> setMainColumn pos { mainCol | toasts = List.append mainCol.toasts [ toast ] }


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
          |> setMainColumn pos emptyColumn
          |> setExitingColumn pos
           (Just
             { toasts = mainCol.toasts
             , style = Animation.interrupt
                [ Animation.to
                  [ Animation.marginTop (Animation.px -300)
                  , Animation.opacity 0
                  ]
                , Animation.Messenger.send (DoneExiting pos)
                ]
                mainCol.style
             }
           )
         |> addCmds Cmd.none

    Add pos ->
          model
          |> incrementToastCount
          |> appendToast pos
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

    DoneExiting pos ->
      model
      |> setExitingColumn pos Nothing
      |> addCmds Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
  let
      subsToExiting pos =
        case exitingColumn model pos of
          Just col ->
            [ Animation.subscription (AnimateExiting pos) [ col.style ] ]
          Nothing ->
            []
  in
    [ [ Sub.map keyStringToMsg (onKeyPress containerDecoder) ]
    , subsToExiting Left
    , subsToExiting Right
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
  [ viewOverlaidColumns model.left model.leftExiting
  , viewOverlaidColumns model.right model.rightExiting
  ]
    |> row []
    |> layout []


viewOverlaidColumns : Column -> Maybe Column -> Element Msg
viewOverlaidColumns col maybeExitingCol =
  let
      topEl =
        case maybeExitingCol of
          Just exitingCol ->
            viewColumn exitingCol

          Nothing ->
            Element.none
  in
    column [Element.inFront topEl, alignTop] [viewColumn col]


viewColumn : Column -> Element Msg
viewColumn col =
  col.toasts
    |> List.map .message
    |> List.map text
    |> List.map (el [padding 30, Background.color (rgb 0.8 0.8 0.8), centerX])
    |> column
      (List.append
        [ width (px 300)
        , padding 30, spacing 20
        , Border.width 3, Border.color (rgb 0 0 0)
        ]
        (List.map Element.htmlAttribute (Animation.render col.style))
      )


{- ---------------------------------------------------------------------
   Debug utilities
   -----------------------------------------------------------------------}


modelToString : Model -> String
modelToString model =
  String.concat
    [ " left = "
    , columnToString model.left
    , ", leftExiting = "
    , exitingColumnToString model.leftExiting
    , ", right = "
    , columnToString model.right
    , ", rightExiting = "
    , exitingColumnToString model.rightExiting
    , "}"
    ]


columnToString : Column -> String
columnToString col =
  String.concat
    [ "{ toasts = "
    , toastListToString col.toasts
    , ", style = ... }"
    ]

toastListToString : List Toast -> String
toastListToString toasts =
  String.concat
    [ "["
    , List.map toastToString toasts |> String.join ", "
    , "]"
    ]


toastToString : Toast -> String
toastToString toast =
  String.concat
    [ "{...\""
    , toast.message
    , "\"...}"
    ]


exitingColumnToString : Maybe Column -> String
exitingColumnToString maybeCol =
  case maybeCol of
    Just col ->
      String.append "Just " (columnToString col)
    Nothing ->
      "Nothing"


