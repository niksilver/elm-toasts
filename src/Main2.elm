import Browser
import Json.Decode as Decode
import Browser.Events
import Html exposing (Html)
import Element
import Element exposing
  ( Element
  , width, padding, spacing
  , px, rgb
  , centerX, alignTop)
import Element.Background as Background



type alias Toast =
  { message : String
  }

type alias Column =
  { toasts : List Toast
  }

type alias Model =
  { left : Column
  , right : Column
  , toastCount : Int
  }


type Position = Left | Right


type Msg =
  DisposeOfColumn Position
  | AddToast Position
  | IgnoreKey




main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



init : () -> (Model, Cmd Msg)
init flags =
  { left = emptyColumn
  , right = emptyColumn
  , toastCount = 0
  }
  |> addNoCmd


emptyColumn : Column
emptyColumn =
  { toasts = []
  }


addNoCmd : Model -> (Model, Cmd Msg)
addNoCmd model =
  (model, Cmd.none)
  


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DisposeOfColumn pos ->
      let
          col = column pos model
      in
          model
          |> setColumn pos emptyColumn
          |> addNoCmd

    AddToast pos ->
      let
          col = column pos model
          toast = makeToast pos model.toastCount
          newCol = addToast toast col
      in
          model
          |> setColumn pos newCol
          |> incrementCounter
          |> addNoCmd

    IgnoreKey ->
      (model, Cmd.none)


column : Position -> Model -> Column
column pos model =
  case pos of
    Left -> model.left

    Right -> model.right


setColumn : Position -> Column -> Model -> Model
setColumn pos col model =
  case pos of
    Left ->
      { model | left = col }

    Right ->
      { model | right = col }


makeToast : Position -> Int -> Toast
makeToast pos count =
  case pos of
    Left ->
      { message = "Left " ++ (String.fromInt count) }

    Right ->
      { message = "Right " ++ (String.fromInt count) }


addToast : Toast -> Column -> Column
addToast toast col =
  { toasts = List.append col.toasts [ toast ] }


incrementCounter : Model -> Model
incrementCounter model =
  { model
  | toastCount = model.toastCount + 1
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



subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onKeyPress keyDecoder
  |> Sub.map keyStringToMsg



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
  Element.row []
    [ viewColumn model.left
    , viewColumn model.right
    ]


viewColumn : Column -> Element Msg
viewColumn col =
  Element.column [ width (px 300), padding 30, spacing 20, alignTop ]
    (toastViews col)


toastViews : Column -> List (Element Msg)
toastViews col =
  List.map viewToast col.toasts


viewToast : Toast -> Element Msg
viewToast toast =
  Element.el [ padding 30, Background.color (rgb 0.8 0.8 0.8), centerX ]
    (Element.text toast.message)




