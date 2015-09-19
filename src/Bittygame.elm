module Bittygame where

import Html exposing (Html, Attribute)
import Html.Events as Events
import Html.Attributes as Attr
import Signal
import Mouse
import StartApp
import Dict exposing (Dict)
import Effects exposing (Effects, Never)
import Task
import String
import BittygameClient
import BittygameClient.Types exposing (..)
import UrlParameterParser exposing (ParseResult(..), parseSearchString)


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

port locationSearch : String

parameters : Dict String String
parameters = 
  case (parseSearchString locationSearch) of
    Error _ -> Dict.empty
    UrlParams dict -> dict

server : String
server = 
  let 
    v = Dict.get "server" parameters
        |> Maybe.withDefault "http://localhost:8080"
  in
    if String.endsWith "/" v then
      String.slice 0 -1 v
    else
      v


-- MODEL
type alias Model = 
  {
    displayText : String,
    currentMove : String,
    state : State,
    gameName : String,
    inGame : Bool,
    won : Bool
  }

init: (Model, Effects Action)
init = 
  let
    initialDisplayText = "Hello there"
    initialState = { inventory = [] }
    gameName = 
      Dict.get "game" parameters
      |> Maybe.withDefault "hungover"
  in
  (
    {
      displayText = initialDisplayText,
      currentMove = "",
      state = initialState,
      gameName = gameName,
      inGame = True,
      won = False
    },
    BittygameClient.beginGame server takeTurn handleError gameName
  )

-- update
takeTurn: Turn -> Action
takeTurn turn =
  let
    doOneThing instr =
      case instr of
        ExitGame -> Just Exit  
        Print stuff -> Just (StuffToPrint stuff)
        Win -> Just YouWin
  in
    turn.instructions 
    |> List.filterMap doOneThing
    |> ManyActions

handleError e = StuffToPrint ("crap!! " ++ (toString e))


type Action = 
    SitAround
  | ManyActions (List Action)
  | Think
  | StuffToPrint String
  | Input String
  | MakeMove
  | Exit
  | YouWin

update: Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Think -> (model, respondToThink model)
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
      if model.currentMove == "think" then
        ( 
          { model 
            | currentMove <- ""
          },
          respondToThink model
        )
      else
        (
          { model |
            displayText <- "I am gonna " ++ model.currentMove,
            currentMove <- ""
          },
          respondToMakeMove model
        )
    StuffToPrint text -> 
      (
        { model |
          displayText <- text
        },
        Effects.none
      )
    Exit -> 
      (
        { model |
          inGame <- False
        },
        Effects.none
      )
    YouWin -> 
      (
        { model |
          won <- True
        },
        Effects.none
      )

respondToThink : Model -> Effects Action
respondToThink model =       
  BittygameClient.think server doWithThoughts handleError model.gameName model.state

respondToMakeMove : Model -> Effects Action  
respondToMakeMove model =
  BittygameClient.turn server takeTurn handleError 
    model.gameName 
    { 
      state = model.state, 
      playerMove = model.currentMove 
    }
  

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


joinTheList: String -> List String -> String
joinTheList joinString these = 
  case these of
    [] -> ""
    one :: [] -> one 
    start :: rest ->
      let   
        whyCantIMakeAnAnonymousFunctionWithTwoArguments e accum =
          accum ++ joinString ++ e
      in
        List.foldl whyCantIMakeAnAnonymousFunctionWithTwoArguments start rest

-- view

view : Signal.Address Action -> Model -> Html
view a model = 
  let 
    interactions =
      if model.inGame then
        [
          Html.input [onInput a Input, onEnter a SitAround MakeMove, Attr.value model.currentMove] [],
          Html.button [Events.onClick a Think] [Html.text "Think"]
        ]
      else
        [ Html.a [Attr.href ""] [Html.text "Start Over"] ]
  in
  Html.div 
    [] 
    (
      [
        header model,
        Html.text model.displayText,
        Html.div [] interactions
      ]
    )

header : Model -> Html
header model = 
  let
    announcement = 
      if model.won then
        "You have won "
      else
        "You are playing "
  in
  Html.div [] 
    [
      Html.h1 [] [Html.text (announcement ++ model.gameName)]
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
