module Bittygame where

import Html exposing (Html)
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
    displayText : String
  }

init: (Model, Effects Action)
init = 
  let
    initialDisplayText = "Hello there"
  in
  (
    {
      displayText = initialDisplayText
    },
    beginGame takeTurn handleError "hungover"
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
  | StuffToPrint String

update: Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    StuffToPrint text -> 
      (
        { model |
          displayText <- text
        },
        Effects.none
      )

-- view

view : Signal.Address Action -> Model -> Html
view a model = Html.text model.displayText
