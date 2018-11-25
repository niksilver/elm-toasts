import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html)
import Element exposing
  ( Element
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
  { id : Int
  , message : String
  , style : Animation.State
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
  = DisposeOfColumn Position
  | AddToast Position
  | IgnoreKey
  | AnimateExitingColumn Position Animation.Msg
  | DoneExitingColumn Position
  | AnimateEnteringToast Position Int Animation.Msg


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


mapToasts : (Toast -> Toast) -> Position -> Model -> Model
mapToasts tMapper pos model =
  let
      col = mainColumn model pos
      newCol = { col | toasts = List.map tMapper col.toasts }
  in
      setMainColumn pos newCol model


getToasts : Position -> Model -> List Toast
getToasts pos model =
  mainColumn model pos
  |> .toasts

newToast : Int -> String -> Toast
newToast id message =
  { id = id
  , message = message
  , style = Animation.style
    [ Animation.marginTop (Animation.px 320)
    , Animation.opacity 0.0
    ]
    |> Animation.interrupt
      [ Animation.to
        [ Animation.marginTop (Animation.px 20)
        , Animation.opacity 1.0
        ]
      ]
  }


getToast : Position -> Int -> Model -> Maybe Toast
getToast pos id model =
  let
      toasts = mainColumn model pos |> .toasts
  in
      toasts
          |> List.filter (\t -> t.id == id)
          |> List.head


-- replaceToastInColumn : Toast -> Column -> Column
-- replaceToastInColumn toast col =
--   let
--       updater t =
--         if t.id = toast.id then
--           toast
--         else
--           t
--   in
--       List.map updater col.toasts


applyToastStyle : Position -> Int -> Animation.Msg -> Model -> Model
applyToastStyle pos id anim model =
  let
      tMapper t =
        if t.id == id then
          { t | style = Animation.update anim t.style }
        else
          t
  in
      model
      |> mapToasts tMapper pos


appendToast : Position -> Model -> Model
appendToast pos model =
  let
      mainCol = mainColumn model pos
      newId = model.toastCount
      message =
        if pos == Left then
          String.append "Left " (String.fromInt newId)
        else
          String.append "Right " (String.fromInt newId)
      toast = newToast newId message
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
    DisposeOfColumn pos ->
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
                , Animation.Messenger.send (DoneExitingColumn pos)
                ]
                mainCol.style
             }
           )
         |> addCmds Cmd.none

    AddToast pos ->
          model
          |> incrementToastCount
          |> appendToast pos
          |> addCmds Cmd.none

    IgnoreKey -> (model, Cmd.none)

    AnimateExitingColumn pos anim ->
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

    DoneExitingColumn pos ->
      model
      |> setExitingColumn pos Nothing
      |> addCmds Cmd.none

    AnimateEnteringToast pos id anim ->
      model
      |> applyToastStyle pos id anim
      |> addCmds Cmd.none

subscriptions : Model -> Sub Msg
subscriptions model =
  let
      subsToExiting pos =
        case exitingColumn model pos of
          Just col ->
            [ Animation.subscription (AnimateExitingColumn pos) [ col.style ] ]
          Nothing ->
            []
      subsToEntering pos =
        getToasts pos model
        |> List.map (\t -> Animation.subscription (AnimateEnteringToast pos t.id) [ t.style ])
  in
    [ [ Sub.map keyStringToMsg (onKeyPress containerDecoder) ]
    , subsToExiting Left
    , subsToExiting Right
    , subsToEntering Left
    , subsToEntering Right
    ]
    |> List.concat
    |> Sub.batch


containerDecoder : Decoder String
containerDecoder =
  Decode.field "key" Decode.string


keyStringToMsg : String -> Msg
keyStringToMsg keyString =
  case keyString of
    "q" -> DisposeOfColumn Left
    "w" -> DisposeOfColumn Right
    "z" -> AddToast Left
    "x" -> AddToast Right
    _ -> IgnoreKey


view : Model -> Html Msg
view model =
  [ viewOverlaidColumns model.left model.leftExiting
  , viewOverlaidColumns model.right model.rightExiting
  ]
    |> Element.row []
    |> Element.layout []


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
    Element.column [Element.inFront topEl, alignTop] [viewColumn col]


viewColumn : Column -> Element Msg
viewColumn col =
  col.toasts
    |> List.map viewToast
    |> Element.column
      (List.append
        [ width (px 300)
        , padding 30
        ]
        (List.map Element.htmlAttribute (Animation.render col.style))
      )


viewToast : Toast -> Element Msg
viewToast toast =
  toast.message
    |> Element.text
    |> Element.el
      (List.append
        [ padding 30, Background.color (rgb 0.8 0.8 0.8), centerX ]
        (List.map Element.htmlAttribute (Animation.render toast.style))
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


