-- Main2.elm
-- Imports
-- litline 78
import Browser
-- litline 219
import Json.Decode as Decode
-- litline 249
import Browser.Events
-- litline 342
import Html exposing (Html)
-- litline 343
import Element
-- litline 344
import Element exposing
-- litline 345
  ( Element
-- litline 346
  , width, padding, spacing
-- litline 347
  , px, rgb
-- litline 348
  , centerX, alignTop)
-- litline 349
import Element.Background as Background
-- litline 365
import Animation

-- litline 25

-- Type definitions
-- litline 40

-- litline 41
type alias Toast =
-- litline 42
  { message : String
  -- More Toast type fields
-- litline 358
    , id : Int
-- litline 387
    , style : Animation.State

-- litline 44
  }
-- litline 45

-- litline 46
type alias Column =
-- litline 47
  { toasts : List Toast
-- litline 48
  }
-- litline 49

-- litline 50
type alias Model =
-- litline 51
  { left : Column
-- litline 52
  , right : Column
-- litline 53
  , toastCount : Int
-- litline 54
  }
-- litline 55

-- litline 115

-- litline 116
type Position = Left | Right
-- litline 117

-- litline 118

-- litline 119
type Msg =
-- litline 120
  DisposeOfColumn Position
-- litline 121
  | AddToast Position
-- litline 122
  | IgnoreKey
  -- More Msgs
-- litline 401
    | AnimateEnteringToast Position Int Animation.Msg

-- litline 124


-- litline 27

-- Functions
-- litline 64
main =
-- litline 65
  Browser.element
-- litline 66
    { init = init
-- litline 67
    , update = update
-- litline 68
    , subscriptions = subscriptions
-- litline 69
    , view = view
-- litline 70
    }
-- litline 71

-- litline 72

-- litline 86
init : () -> (Model, Cmd Msg)
-- litline 87
init flags =
-- litline 88
  { left = emptyColumn
-- litline 89
  , right = emptyColumn
-- litline 90
  , toastCount = 0
-- litline 91
  }
-- litline 92
  |> addNoCmd
-- litline 93

-- litline 94

-- litline 95
emptyColumn : Column
-- litline 96
emptyColumn =
-- litline 97
  { toasts = []
-- litline 98
  }
-- litline 99

-- litline 100

-- litline 101
addNoCmd : Model -> (Model, Cmd Msg)
-- litline 102
addNoCmd model =
-- litline 103
  (model, Cmd.none)
-- litline 104
  
-- litline 105

-- litline 128
update : Msg -> Model -> (Model, Cmd Msg)
-- litline 129
update msg model =
-- litline 130
  case msg of
-- litline 131
    DisposeOfColumn pos ->
-- litline 132
      let
-- litline 133
          col = column pos model
-- litline 134
      in
-- litline 135
          model
-- litline 136
          |> setColumn pos emptyColumn
-- litline 137
          |> addNoCmd
-- litline 138

-- litline 139
    AddToast pos ->
-- litline 140
      let
-- litline 141
          col = column pos model
-- litline 142
          toast = makeToast pos model.toastCount
-- litline 143
          newCol = addToast toast col
-- litline 144
      in
-- litline 145
          model
-- litline 146
          |> setColumn pos newCol
-- litline 147
          |> incrementCounter
-- litline 148
          |> addNoCmd
-- litline 149

-- litline 150
    IgnoreKey ->
-- litline 151
      (model, Cmd.none)
-- litline 152

    -- More model updates
-- litline 439
    AnimateEnteringToast pos id anim ->
-- litline 440
      model
-- litline 441
      |> applyToastStyle pos id anim
-- litline 442
      |> addNoCmd
-- litline 443
    

-- litline 154

-- litline 155

-- litline 161
column : Position -> Model -> Column
-- litline 162
column pos model =
-- litline 163
  case pos of
-- litline 164
    Left -> model.left
-- litline 165

-- litline 166
    Right -> model.right
-- litline 167

-- litline 168

-- litline 169
setColumn : Position -> Column -> Model -> Model
-- litline 170
setColumn pos col model =
-- litline 171
  case pos of
-- litline 172
    Left ->
-- litline 173
      { model | left = col }
-- litline 174

-- litline 175
    Right ->
-- litline 176
      { model | right = col }
-- litline 177

-- litline 178

-- litline 179
makeToast : Position -> Int -> Toast
-- litline 180
makeToast pos count =
-- litline 181
  let
-- litline 182
      message =
-- litline 183
        case pos of
-- litline 184
          Left -> "Left " ++ (String.fromInt count)
-- litline 185
          Right -> "Right " ++ (String.fromInt count)
-- litline 186
  in
-- litline 187
    { message = message
    -- More Toast creation fields
-- litline 496
      , id = count
-- litline 497
      , style = Animation.style
-- litline 498
        [ Animation.marginTop (Animation.px 320)
-- litline 499
        , Animation.opacity 0.0
-- litline 500
        ]
-- litline 501
        |> Animation.interrupt
-- litline 502
          [ Animation.to
-- litline 503
            [ Animation.marginTop (Animation.px 20)
-- litline 504
            , Animation.opacity 1.0
-- litline 505
            ]
-- litline 506
          ]

-- litline 189
    }
-- litline 190

-- litline 191

-- litline 192
addToast : Toast -> Column -> Column
-- litline 193
addToast toast col =
-- litline 194
  { toasts = List.append col.toasts [ toast ] }
-- litline 195

-- litline 196

-- litline 197
incrementCounter : Model -> Model
-- litline 198
incrementCounter model =
-- litline 199
  { model
-- litline 200
  | toastCount = model.toastCount + 1
-- litline 201
  }
-- litline 202

-- litline 203

-- litline 223
keyDecoder : Decode.Decoder String
-- litline 224
keyDecoder =
-- litline 225
  Decode.field "key" Decode.string
-- litline 226

-- litline 227

-- litline 233
keyStringToMsg : String -> Msg
-- litline 234
keyStringToMsg keyString =
-- litline 235
  case keyString of
-- litline 236
    "q" -> DisposeOfColumn Left
-- litline 237
    "w" -> DisposeOfColumn Right
-- litline 238
    "z" -> AddToast Left
-- litline 239
    "x" -> AddToast Right
-- litline 240
    _ -> IgnoreKey
-- litline 241

-- litline 242

-- litline 253
subscriptions : Model -> Sub Msg
-- litline 254
subscriptions model =
-- litline 255
  let
      -- subscriptions assignments
-- litline 410
            subsToEntering pos =
-- litline 411
              getToasts pos model
-- litline 412
              |> List.map (\t -> Animation.subscription (AnimateEnteringToast pos t.id) [ t.style ])

-- litline 257
  in
-- litline 258
    [ [ Browser.Events.onKeyPress keyDecoder |> Sub.map keyStringToMsg ]
    -- More subscription lists
-- litline 429
        , subsToEntering Left
-- litline 430
        , subsToEntering Right

-- litline 260
    ]
-- litline 261
    |> List.concat
-- litline 262
    |> Sub.batch
-- litline 263

-- litline 264

-- litline 291
view : Model -> Html Msg
-- litline 292
view model =
-- litline 293
  Element.column []
-- litline 294
    [ viewInstructions
-- litline 295
    , viewMainModel model
-- litline 296
    ]
-- litline 297
    |> Element.layout []
-- litline 298

-- litline 299
viewInstructions : Element Msg
-- litline 300
viewInstructions =
-- litline 301
  Element.el [Element.padding 30]
-- litline 302
    (Element.text "Use 'z' and x' to add a message, 'q' and 'w' to dispose of a column of messages")
-- litline 303

-- litline 304

-- litline 305
viewMainModel : Model -> Element Msg
-- litline 306
viewMainModel model =
-- litline 307
  Element.row []
-- litline 308
    [ viewColumn model.left
-- litline 309
    , viewColumn model.right
-- litline 310
    ]
-- litline 311

-- litline 312

-- litline 313
viewColumn : Column -> Element Msg
-- litline 314
viewColumn col =
-- litline 315
  Element.column [ width (px 300), padding 30, spacing 20, alignTop ]
-- litline 316
    (toastViews col)
-- litline 317

-- litline 318

-- litline 319
toastViews : Column -> List (Element Msg)
-- litline 320
toastViews col =
-- litline 321
  List.map viewToast col.toasts
-- litline 322

-- litline 323

-- litline 324
viewToast : Toast -> Element Msg
-- litline 325
viewToast toast =
-- litline 326
  toast.message
-- litline 327
  |> Element.text
-- litline 328
  |> Element.el
-- litline 329
    (List.concat
-- litline 330
      [ [padding 30]
-- litline 331
      , [Background.color (rgb 0.8 0.8 0.8)]
-- litline 332
      , [centerX]
      -- More Toast stylings
-- litline 484
      , (List.map Element.htmlAttribute (Animation.render toast.style))

-- litline 334
      ]
-- litline 335
    )
-- litline 336

-- litline 337

-- litline 418
getToasts : Position -> Model -> List Toast
-- litline 419
getToasts pos model =
-- litline 420
  column pos model
-- litline 421
  |> .toasts
-- litline 422

-- litline 423

-- litline 452
applyToastStyle : Position -> Int -> Animation.Msg -> Model -> Model
-- litline 453
applyToastStyle pos id anim model =
-- litline 454
  let
-- litline 455
      tMapper t =
-- litline 456
        if t.id == id then
-- litline 457
          { t | style = Animation.update anim t.style }
-- litline 458
        else
-- litline 459
          t
-- litline 460
  in
-- litline 461
      model
-- litline 462
      |> mapToasts tMapper pos
-- litline 463

-- litline 464

-- litline 465
mapToasts : (Toast -> Toast) -> Position -> Model -> Model
-- litline 466
mapToasts tMapper pos model =
-- litline 467
  let
-- litline 468
      col = column pos model
-- litline 469
      newCol = { col | toasts = List.map tMapper col.toasts }
-- litline 470
  in
-- litline 471
      setColumn pos newCol model
-- litline 472

-- litline 473



