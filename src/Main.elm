import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html)
import Element exposing
  ( Element
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


type alias Toast =
  { id : Int
  , message : String
  , style : Animation.State
  }


type alias Column =
    { toasts : List Toast
    }


type alias Model =
  { left : Column
  , right : Column
  , toastCount : Int
  }


emptyColumn : Column
emptyColumn =
  { toasts = []
  }


init : () -> (Model, Cmd Msg)
init flags =
  ( { left = emptyColumn
    , right = emptyColumn
    , toastCount = 0
    }
  , Cmd.none
  )


type Position = Left | Right


type Msg
  = DisposeOfColumn Position
  | AddToast Position
  | IgnoreKey
  | AnimateEnteringToast Position Int Animation.Msg


mainColumn : Model -> Position -> Column
mainColumn model pos =
  case pos of
    Left -> model.left
    Right -> model.right


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
          |> addCmds Cmd.none

    AddToast pos ->
          model
          |> incrementToastCount
          |> appendToast pos
          |> addCmds Cmd.none

    IgnoreKey -> (model, Cmd.none)

    AnimateEnteringToast pos id anim ->
      model
      |> applyToastStyle pos id anim
      |> addCmds Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
  let
      subsToEntering pos =
        getToasts pos model
        |> List.map (\t -> Animation.subscription (AnimateEnteringToast pos t.id) [ t.style ])
  in
    [ [ Sub.map keyStringToMsg (onKeyPress containerDecoder) ]
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
  Element.column []
    [ viewInstructions
    , viewMainModel model
    ]
    |> Element.layout []


viewInstructions : Element Msg
viewInstructions =
  Element.el [Element.padding 30]
    (Element.text "Use 'z' and x' to add a message, 'q' and 'w' to dispose of a column of messages")


viewMainModel : Model -> Element Msg
viewMainModel model =
  [ viewColumn model.left
  , viewColumn model.right
  ]
    |> Element.row []



viewColumn : Column -> Element Msg
viewColumn col =
  col.toasts
    |> List.map viewToast
    |> Element.column [ width (px 300), padding 30, alignTop ]
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



