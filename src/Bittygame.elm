module Bittygame where

import Html exposing (Html, Attribute)
import Html.Events as Events
import Html.Attributes as Attr
import Signal
import Mouse
import StartApp
import Effects exposing (Effects, Never)
import Task
import BittygameClient exposing (beginGame)
import BittygameClient.Types exposing (..)


-- wiring
app = StartApp.start 
  { 
    init = init, 
    view = view, 
    update = update, 
    inputs = [] 
  }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

-- MODEL
type alias Model = 
  {
    displayText : String,
    currentMove : String,
    state : State,
    gameName : String
  }

init: (Model, Effects Action)
init = 
  let
    initialDisplayText = "Hello there"
    initialState = { inventory = [] }
    gameName = "hungover"
  in
  (
    {
      displayText = initialDisplayText,
      currentMove = "",
      state = initialState,
      gameName = gameName
    },
    beginGame takeTurn handleError gameName
  )

takeTurn: Turn -> Action
takeTurn turn =
  let
    doOneThing instr =
      case instr of
        ExitGame -> Nothing  -- Not implemented
        Print stuff -> Just (StuffToPrint stuff)
  in
    turn.instructions 
    |> List.filterMap doOneThing
    |> ManyActions

handleError e = StuffToPrint ("crap!! " ++ (toString e))

-- update
type Action = 
    SitAround
  | ManyActions (List Action)
  | Think
  | StuffToPrint String
  | Input String
  | MakeMove

update: Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Think -> (model, BittygameClient.think doWithThoughts handleError model.gameName model.state)
    SitAround -> (model, Effects.none)
    ManyActions allOfThese ->
      updateAll allOfThese model -- mutually recursive
    Input move -> 
      ( 
        { model |
          currentMove <- move
        },
        Effects.none
      )
    MakeMove -> 
      (
        { model |
          displayText <- "I am gonna " ++ model.currentMove
        },
        BittygameClient.turn takeTurn handleError 
          model.gameName 
          { 
            state = model.state, 
            playerMove = model.currentMove 
          }
      )
    StuffToPrint text -> 
      (
        { model |
          displayText <- text
        },
        Effects.none
      )

updateAll : List Action -> Model -> (Model, Effects Action)
updateAll actions model =
  let
    keepUpdating: Action -> (Model, Effects Action) -> (Model, Effects Action)
    keepUpdating action (model, effectSoFar) =
      let
        (newModel, moreEffects) = update action model
      in
        (newModel, Effects.batch [effectSoFar, moreEffects])
  in
    List.foldl keepUpdating (model, Effects.none) actions

doWithThoughts : Thoughts -> Action
doWithThoughts thoughts =
  StuffToPrint ("Well, I could " ++ (joinTheFuckingList " or " thoughts))


joinTheFuckingList: String -> List String -> String
joinTheFuckingList joinString = 
  let
    whyCantIMakeAnAnonymousFunctionWithTwoArguments e accum =
      accum ++ joinString ++ e
  in
    List.foldl whyCantIMakeAnAnonymousFunctionWithTwoArguments ""
-- view

view : Signal.Address Action -> Model -> Html
view a model = 
  Html.div 
    [] 
    [
      Html.text model.displayText,
      Html.button [Events.onClick a Think] [Html.text "Think"],
      Html.input [onInput a Input, onEnter a SitAround MakeMove, Attr.value model.currentMove] []
    ]

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput addr contentToValue =
    Events.on "input" Events.targetValue (\str -> Signal.message addr (contentToValue str))

onEnter : Signal.Address a -> a -> a -> Attribute
onEnter address doNothing action =
  let
    respondTo code =
      case code of
        13 -> action
        _  -> doNothing
    in
    Events.on "keyup" Events.keyCode (\code -> Signal.message address (respondTo code))
