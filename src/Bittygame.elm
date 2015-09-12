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
    inputs = [whereIsThePointer] 
  }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

whereIsThePointer: Signal Action
whereIsThePointer = Signal.map PointerMoved Mouse.position

-- MODEL
type alias Model = (Int, Int)

init: (Model, Effects Action)
init = ((0,0), Effects.none)

-- update
type Action = PointerMoved (Int, Int)

update: Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    PointerMoved (x, y) -> ((x, y), Effects.none)

-- view

view : Signal.Address Action -> Model -> Html
view a model = pointerView model

pointerView: (Int, Int) -> Html
pointerView (x, y) = Html.text ("you are pointing at " ++ (toString x) ++ ", " ++ (toString y))
