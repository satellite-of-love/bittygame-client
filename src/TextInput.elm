module TextInput(TextInput, new) where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Effects exposing (Effects)
import Signal
import Task


type alias TextInput yourAction = {
  init : Model,
  view : Signal.Address Action -> Model -> Html,
  update : Action -> Model -> (Model, Effects yourAction)
}

new : (String -> yourAction) 
               -> TextInput yourAction
new reportOnEnter =
  let 
    report = makeReportFunction reportOnEnter
  in
    {
      init = init,
      update = update report,
      view = view
    } 

makeReportFunction : (String -> yourAction) -> String -> Effects yourAction
makeReportFunction reportOnEnter contents =
  reportOnEnter contents
    |> Task.succeed
    |> Effects.task 

-- MODEL
type alias Model = {
  contents : String
}
init : Model
init = { contents = "" }

-- UPDATE
type Action = 
  Enter
  | Contents String 
  | UninterestingKeyPressed

update : (String -> Effects yourAction) -> Action -> Model -> (Model, Effects yourAction)
update report action model =
  case action of
    UninterestingKeyPressed -> (model, Effects.none)
    Contents str -> 
      (
        newContents model str,
        Effects.none
      )
    Enter -> 
      (
        newContents model "",
        report model.contents
      )

newContents : Model -> String -> Model
newContents model c =
  { model |
    contents <- c
  }

-- VIEW
view : Signal.Address Action -> Model -> Html
view myAddr model =
  Html.input 
    [
      Attr.type' "text", 
      Attr.value model.contents,
      onInput myAddr Contents,
      onEnter myAddr UninterestingKeyPressed Enter
    ] []

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput addr contentToValue =
    Events.on "input" Events.targetValue (\str -> Signal.message addr (contentToValue str))

onEnter : Signal.Address a -> a -> a -> Attribute
onEnter address doNothing noticeEnter =
  let
    respondTo code =
      case code of
        13 -> noticeEnter
        _  -> doNothing
  in
    Events.on "keyup" Events.keyCode (\code -> Signal.message address (respondTo code))
