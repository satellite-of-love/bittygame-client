module BittygameClient(beginGame, think, turn) where

import Http
import Task
import Effects exposing (Effects)
import BittygameClient.Types exposing (..)
import BittygameClient.Serialization as Ser

beginGameUrl baseUrl name = baseUrl ++ "/scenario/" ++ (Http.uriEncode name) ++ "/begin"

thinkUrl baseUrl game = baseUrl ++ "/game/" ++ game ++ "/think"

turnUrl baseUrl game action = baseUrl ++ "/game/" ++ game ++ "/turn/" ++ (Http.uriEncode action)

beginGame: String -> (Turn -> action) -> (Http.Error -> action) -> ScenarioName -> Effects action
beginGame baseUrl successAction failureAction name = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  Http.get Ser.turn (beginGameUrl baseUrl name)
  |> Task.toResult
  |> Task.map handler
  |> Effects.task

think: String -> (Thoughts -> action) -> (Http.Error -> action) -> GameID -> Effects action
think baseUrl successAction failureAction game = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  Http.get Ser.thoughts (thinkUrl baseUrl game)
  |> Task.toResult
  |> Task.map handler
  |> Effects.task

turn: String -> (Turn -> action) -> (Http.Error -> action) -> GameID -> GameAction -> Effects action
turn baseUrl successAction failureAction game action = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  Http.get Ser.turn (turnUrl baseUrl game action)
  |> Task.toResult
  |> Task.map handler
  |> Effects.task
