module Bittygame where

import Html exposing (Html)
import Signal
import Mouse
import StartApp
import Effects exposing (Effects, Never)
import Task


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
    Effects.none
  )

-- update
type Action = StuffToPrint String

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
