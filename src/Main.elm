-- Main.elm
-- Imports
import Browser
import Animation
import Html exposing (Html)
import Element exposing
  ( Element
  , width, padding, spacing
  , px, rgb
  , centerX, alignTop)
import Element.Background as Background
import Json.Decode as Decode
import Browser.Events



-- Type definitions
type alias Model =
  { left : Column
  , leftExiting : Column
  , right : Column
  , rightExiting : Column
  , toastCount : Int
  }


type alias Column =
    { toasts : List Toast
    , style : Animation.State
    }


type alias Toast =
  { id : Int
  , message : String
  , style : Animation.State
  }


type Msg
  = DisposeOfColumn Position
  | AddToast Position
  | IgnoreKey
  | AnimateExitingColumn Position Animation.Msg
  | AnimateEnteringToast Position Int Animation.Msg


type Position = Left | Right



-- Functions
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : () -> (Model, Cmd Msg)
init flags =
  ( { left = emptyColumn
    , leftExiting = emptyColumn
    , right = emptyColumn
    , rightExiting = emptyColumn
    , toastCount = 0
    }
  , Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
-- Update case clauses for key press update messages
    DisposeOfColumn pos ->
      let
          col = column model pos
      in
          model
          |> setColumn pos emptyColumn
          |> setExitingColumn pos
               { toasts = col.toasts
               , style = Animation.interrupt
                  [ Animation.to
                    [ Animation.marginTop (Animation.px -300)
                    , Animation.opacity 0
                    ]
                  ]
                  col.style
               }
         |> addNoCmd

    AddToast pos ->
          model
          |> incrementToastCount
          |> appendToast pos
          |> addNoCmd

    IgnoreKey -> (model, Cmd.none)

-- Update case clauses for animation update messages
    AnimateExitingColumn pos anim ->
      let
          col = exitingColumn model pos
          newStyle = Animation.update anim col.style
      in
          model
          |> setExitingColumn pos { col | style = newStyle }
          |> addNoCmd

    AnimateEnteringToast pos id anim ->
      model
      |> applyToastStyle pos id anim
      |> addNoCmd


addNoCmd : Model -> (Model, Cmd Msg)
addNoCmd model =
  (model, Cmd.none)


emptyColumn : Column
emptyColumn =
  { toasts = []
  , style =
      Animation.style
        [ Animation.marginTop (Animation.px 0)
        , Animation.opacity 1.0
        ]
  }


subscriptions : Model -> Sub Msg
subscriptions model =
  let
      subsToExiting pos =
        let
            col = exitingColumn model pos
        in
            [ Animation.subscription (AnimateExitingColumn pos) [ col.style ] ]
      subsToEntering pos =
        getToasts pos model
        |> List.map (\t -> Animation.subscription (AnimateEnteringToast pos t.id) [ t.style ])
  in
    [
      -- List of subscriptions for key presses
          [ Browser.Events.onKeyPress keyDecoder |> Sub.map keyStringToMsg ]

    , subsToExiting Left
    , subsToExiting Right
    , subsToEntering Left
    , subsToEntering Right
    ]
    |> List.concat
    |> Sub.batch


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


mapToasts : (Toast -> Toast) -> Position -> Model -> Model
mapToasts tMapper pos model =
  let
      col = column model pos
      newCol = { col | toasts = List.map tMapper col.toasts }
  in
      setColumn pos newCol model


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
  [ viewOverlaidColumns model.left model.leftExiting
  , viewOverlaidColumns model.right model.rightExiting
  ]
    |> Element.row []


viewOverlaidColumns : Column -> Column -> Element Msg
viewOverlaidColumns col exitingCol =
  let
      topEl = viewColumn exitingCol
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


appendToast : Position -> Model -> Model
appendToast pos model =
  let
      col = column model pos
      newId = model.toastCount
      message =
        if pos == Left then
          String.append "Left " (String.fromInt newId)
        else
          String.append "Right " (String.fromInt newId)
      toast = newToast newId message
  in
      model
      |> setColumn pos { col | toasts = List.append col.toasts [ toast ] }


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


keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string


keyStringToMsg : String -> Msg
keyStringToMsg keyString =
  case keyString of
    "q" -> DisposeOfColumn Left
    "w" -> DisposeOfColumn Right
    "z" -> AddToast Left
    "x" -> AddToast Right
    _ -> IgnoreKey


column : Model -> Position -> Column
column model pos =
  case pos of
    Left -> model.left
    Right -> model.right


setColumn : Position -> Column -> Model -> Model
setColumn pos col model =
  case pos of
    Left -> { model | left = col }
    Right -> { model | right = col }


exitingColumn : Model -> Position -> Column
exitingColumn model pos =
  case pos of
    Left -> model.leftExiting
    Right -> model.rightExiting


setExitingColumn : Position -> Column -> Model -> Model
setExitingColumn pos col model =
  case pos of
    Left -> { model | leftExiting = col }
    Right -> { model | rightExiting = col }


incrementToastCount : Model -> Model
incrementToastCount model =
  { model
  | toastCount = model.toastCount + 1
  }


getToasts : Position -> Model -> List Toast
getToasts pos model =
  column model pos
  |> .toasts




