-- Main2.elm
-- Imports
-- litline 80
import Browser
-- litline 227
import Json.Decode as Decode
-- litline 257
import Browser.Events
-- litline 363
import Html exposing (Html)
-- litline 364
import Element
-- litline 365
import Element exposing
-- litline 366
  ( Element
-- litline 367
  , width, padding, spacing
-- litline 368
  , px, rgb
-- litline 369
  , centerX, alignTop)
-- litline 370
import Element.Background as Background
-- litline 386
import Animation

-- litline 25

-- Type definitions
-- litline 40

-- litline 41
type alias Toast =
-- litline 42
  { message : String
  -- More Toast type fields
-- litline 379
    , id : Int
-- litline 408
    , style : Animation.State

-- litline 44
  }
-- litline 45

-- litline 46
type alias Column =
-- litline 47
  { toasts : List Toast
  -- More Column type fields
-- litline 589
      , style : Animation.State

-- litline 49
  }
-- litline 50

-- litline 51
type alias Model =
-- litline 52
  { left : Column
-- litline 53
  , right : Column
-- litline 54
  , toastCount : Int
  -- More Model fields
-- litline 546
    , leftExiting : Column
-- litline 547
    , rightExiting : Column

-- litline 56
  }
-- litline 57

-- litline 119

-- litline 120
type Position = Left | Right
-- litline 121

-- litline 122

-- litline 123
type Msg =
-- litline 124
  DisposeOfColumn Position
-- litline 125
  | AddToast Position
-- litline 126
  | IgnoreKey
  -- More Msgs
-- litline 422
    | AnimateEnteringToast Position Int Animation.Msg
-- litline 607
    | AnimateExitingColumn Position Animation.Msg

-- litline 128


-- litline 27

-- Functions
-- litline 66
main =
-- litline 67
  Browser.element
-- litline 68
    { init = init
-- litline 69
    , update = update
-- litline 70
    , subscriptions = subscriptions
-- litline 71
    , view = view
-- litline 72
    }
-- litline 73

-- litline 74

-- litline 88
init : () -> (Model, Cmd Msg)
-- litline 89
init flags =
-- litline 90
  { left = emptyColumn
-- litline 91
  , right = emptyColumn
-- litline 92
  , toastCount = 0
  -- More Model field initialisation
-- litline 551
      , leftExiting = emptyColumn
-- litline 552
      , rightExiting = emptyColumn

-- litline 94
  }
-- litline 95
  |> addNoCmd
-- litline 96

-- litline 97

-- litline 98
emptyColumn : Column
-- litline 99
emptyColumn =
-- litline 100
  { toasts = []
  -- More empty Column fields
-- litline 593
    , style =
-- litline 594
        Animation.style
-- litline 595
          [ Animation.marginTop (Animation.px 0)
-- litline 596
          , Animation.opacity 1.0
-- litline 597
          ]

-- litline 102
  }
-- litline 103

-- litline 104

-- litline 105
addNoCmd : Model -> (Model, Cmd Msg)
-- litline 106
addNoCmd model =
-- litline 107
  (model, Cmd.none)
-- litline 108
  
-- litline 109

-- litline 132
update : Msg -> Model -> (Model, Cmd Msg)
-- litline 133
update msg model =
-- litline 134
  case msg of
-- litline 135
    DisposeOfColumn pos ->
-- litline 136
      let
-- litline 137
          col = column pos model
-- litline 138
      in
-- litline 139
          model
-- litline 140
          |> setColumn pos emptyColumn
          -- More column disposal operations
-- litline 682
                    |> setExitingColumn pos
-- litline 683
                         { toasts = col.toasts
-- litline 684
                         , style = Animation.interrupt
-- litline 685
                            [ Animation.to
-- litline 686
                              [ Animation.marginTop (Animation.px -300)
-- litline 687
                              , Animation.opacity 0
-- litline 688
                              ]
-- litline 689
                            ]
-- litline 690
                            col.style
-- litline 691
                         }

-- litline 142
          |> addNoCmd
-- litline 143

-- litline 144
    AddToast pos ->
-- litline 145
      let
-- litline 146
          col = column pos model
-- litline 147
          toast = makeToast pos model.toastCount
-- litline 148
          newCol = addToast toast col
-- litline 149
      in
-- litline 150
          model
-- litline 151
          |> setColumn pos newCol
-- litline 152
          |> incrementCounter
-- litline 153
          |> addNoCmd
-- litline 154

-- litline 155
    IgnoreKey ->
-- litline 156
      (model, Cmd.none)
-- litline 157

    -- More Model updates
-- litline 460
    AnimateEnteringToast pos id anim ->
-- litline 461
      model
-- litline 462
      |> applyToastStyle pos id anim
-- litline 463
      |> addNoCmd
-- litline 464
    
-- litline 628
    AnimateExitingColumn pos anim ->
-- litline 629
      let
-- litline 630
          col = exitingColumn model pos
-- litline 631
          newStyle = Animation.update anim col.style
-- litline 632
      in
-- litline 633
          model
-- litline 634
          |> setExitingColumn pos { col | style = newStyle }
-- litline 635
          |> addNoCmd
-- litline 636
    

-- litline 159

-- litline 160

-- litline 169
column : Position -> Model -> Column
-- litline 170
column pos model =
-- litline 171
  case pos of
-- litline 172
    Left -> model.left
-- litline 173

-- litline 174
    Right -> model.right
-- litline 175

-- litline 176

-- litline 177
setColumn : Position -> Column -> Model -> Model
-- litline 178
setColumn pos col model =
-- litline 179
  case pos of
-- litline 180
    Left ->
-- litline 181
      { model | left = col }
-- litline 182

-- litline 183
    Right ->
-- litline 184
      { model | right = col }
-- litline 185

-- litline 186

-- litline 187
makeToast : Position -> Int -> Toast
-- litline 188
makeToast pos count =
-- litline 189
  let
-- litline 190
      message =
-- litline 191
        case pos of
-- litline 192
          Left -> "Left " ++ (String.fromInt count)
-- litline 193
          Right -> "Right " ++ (String.fromInt count)
-- litline 194
  in
-- litline 195
    { message = message
    -- More Toast creation fields
-- litline 517
      , id = count
-- litline 518
      , style = Animation.style
-- litline 519
        [ Animation.marginTop (Animation.px 320)
-- litline 520
        , Animation.opacity 0.0
-- litline 521
        ]
-- litline 522
        |> Animation.interrupt
-- litline 523
          [ Animation.to
-- litline 524
            [ Animation.marginTop (Animation.px 20)
-- litline 525
            , Animation.opacity 1.0
-- litline 526
            ]
-- litline 527
          ]

-- litline 197
    }
-- litline 198

-- litline 199

-- litline 200
addToast : Toast -> Column -> Column
-- litline 201
addToast toast col =
-- litline 202
  { col | toasts = List.append col.toasts [ toast ] }
-- litline 203

-- litline 204

-- litline 205
incrementCounter : Model -> Model
-- litline 206
incrementCounter model =
-- litline 207
  { model
-- litline 208
  | toastCount = model.toastCount + 1
-- litline 209
  }
-- litline 210

-- litline 211

-- litline 231
keyDecoder : Decode.Decoder String
-- litline 232
keyDecoder =
-- litline 233
  Decode.field "key" Decode.string
-- litline 234

-- litline 235

-- litline 241
keyStringToMsg : String -> Msg
-- litline 242
keyStringToMsg keyString =
-- litline 243
  case keyString of
-- litline 244
    "q" -> DisposeOfColumn Left
-- litline 245
    "w" -> DisposeOfColumn Right
-- litline 246
    "z" -> AddToast Left
-- litline 247
    "x" -> AddToast Right
-- litline 248
    _ -> IgnoreKey
-- litline 249

-- litline 250

-- litline 261
subscriptions : Model -> Sub Msg
-- litline 262
subscriptions model =
-- litline 263
  let
      -- subscriptions assignments
-- litline 431
            subsToEntering pos =
-- litline 432
              getToasts pos model
-- litline 433
              |> List.map (\t -> Animation.subscription (AnimateEnteringToast pos t.id) [ t.style ])
-- litline 611
            subsToExiting pos =
-- litline 612
              let
-- litline 613
                  col = exitingColumn model pos
-- litline 614
              in
-- litline 615
                  [ Animation.subscription (AnimateExitingColumn pos) [ col.style ] ]

-- litline 265
  in
-- litline 266
    [ [ Browser.Events.onKeyPress keyDecoder |> Sub.map keyStringToMsg ]
    -- More subscription lists
-- litline 450
        , subsToEntering Left
-- litline 451
        , subsToEntering Right
-- litline 619
        , subsToExiting Left
-- litline 620
        , subsToExiting Right

-- litline 268
    ]
-- litline 269
    |> List.concat
-- litline 270
    |> Sub.batch
-- litline 271

-- litline 272

-- litline 299
view : Model -> Html Msg
-- litline 300
view model =
-- litline 301
  Element.column []
-- litline 302
    [ viewInstructions
-- litline 303
    , viewMainModel model
-- litline 304
    ]
-- litline 305
    |> Element.layout []
-- litline 306

-- litline 307
viewInstructions : Element Msg
-- litline 308
viewInstructions =
-- litline 309
  Element.el [Element.padding 30]
-- litline 310
    (Element.text "Use 'z' and x' to add a message, 'q' and 'w' to dispose of a column of messages")
-- litline 311

-- View main model function
-- litline 647
viewMainModel : Model -> Element Msg
-- litline 648
viewMainModel model =
-- litline 649
  [ viewOverlaidColumns model.left model.leftExiting
-- litline 650
  , viewOverlaidColumns model.right model.rightExiting
-- litline 651
  ]
-- litline 652
    |> Element.row []
-- litline 653

-- litline 654


-- View columns and toasts functions
-- litline 328
viewColumn : Column -> Element Msg
-- litline 329
viewColumn col =
-- litline 330
  toastViews col
-- litline 331
  |> Element.column
-- litline 332
    (List.concat
-- litline 333
      [ [width (px 300)]
-- litline 334
      , [padding 30]
      -- More Column stylings
-- litline 673
            , (List.map Element.htmlAttribute (Animation.render col.style))

-- litline 336
      ]
-- litline 337
    )
-- litline 338

-- litline 339

-- litline 340
toastViews : Column -> List (Element Msg)
-- litline 341
toastViews col =
-- litline 342
  List.map viewToast col.toasts
-- litline 343

-- litline 344

-- litline 345
viewToast : Toast -> Element Msg
-- litline 346
viewToast toast =
-- litline 347
  toast.message
-- litline 348
  |> Element.text
-- litline 349
  |> Element.el
-- litline 350
    (List.concat
-- litline 351
      [ [padding 30]
-- litline 352
      , [Background.color (rgb 0.8 0.8 0.8)]
-- litline 353
      , [centerX]
      -- More Toast stylings
-- litline 505
      , (List.map Element.htmlAttribute (Animation.render toast.style))

-- litline 355
      ]
-- litline 356
    )
-- litline 357

-- litline 358

-- litline 660
viewOverlaidColumns : Column -> Column -> Element Msg
-- litline 661
viewOverlaidColumns col exitingCol =
-- litline 662
  let
-- litline 663
      topEl = viewColumn exitingCol
-- litline 664
  in
-- litline 665
      Element.column [Element.inFront topEl, alignTop] [viewColumn col]
-- litline 666

-- litline 667


-- litline 439
getToasts : Position -> Model -> List Toast
-- litline 440
getToasts pos model =
-- litline 441
  column pos model
-- litline 442
  |> .toasts
-- litline 443

-- litline 444

-- litline 473
applyToastStyle : Position -> Int -> Animation.Msg -> Model -> Model
-- litline 474
applyToastStyle pos id anim model =
-- litline 475
  let
-- litline 476
      tMapper t =
-- litline 477
        if t.id == id then
-- litline 478
          { t | style = Animation.update anim t.style }
-- litline 479
        else
-- litline 480
          t
-- litline 481
  in
-- litline 482
      model
-- litline 483
      |> mapToasts tMapper pos
-- litline 484

-- litline 485

-- litline 486
mapToasts : (Toast -> Toast) -> Position -> Model -> Model
-- litline 487
mapToasts tMapper pos model =
-- litline 488
  let
-- litline 489
      col = column pos model
-- litline 490
      newCol = { col | toasts = List.map tMapper col.toasts }
-- litline 491
  in
-- litline 492
      setColumn pos newCol model
-- litline 493

-- litline 494

-- litline 559
exitingColumn : Model -> Position -> Column
-- litline 560
exitingColumn model pos =
-- litline 561
  case pos of
-- litline 562
    Left -> model.leftExiting
-- litline 563
    Right -> model.rightExiting
-- litline 564

-- litline 565

-- litline 566
setExitingColumn : Position -> Column -> Model -> Model
-- litline 567
setExitingColumn pos col model =
-- litline 568
  case pos of
-- litline 569
    Left -> { model | leftExiting = col }
-- litline 570
    Right -> { model | rightExiting = col }
-- litline 571

-- litline 572



