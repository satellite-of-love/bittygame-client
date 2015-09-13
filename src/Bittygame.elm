module Bittygame where

import Html exposing (Html)
import Html.Events as Events
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
    |> List.head
    |> Maybe.withDefault SitAround

handleError e = StuffToPrint ("crap!! " ++ (toString e))

-- update
type Action = 
    SitAround
  | Think
  | StuffToPrint String

update: Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Think -> (model, BittygameClient.think doWithThoughts handleError model.gameName model.state)
    SitAround -> (model, Effects.none)
    StuffToPrint text -> 
      (
        { model |
          displayText <- text
        },
        Effects.none
      )

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
      Html.button [Events.onClick a Think] [Html.text "Think"]
    ]
