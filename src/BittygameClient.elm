module BittygameClient(beginGame, think, turn) where

import Http
import Task
import GetWithHeaders exposing (postJson)
import Effects exposing (Effects)
import BittygameClient.Types exposing (..)
import BittygameClient.Serialization as Ser

baseUrl = "http://localhost:8080"

beginGameUrl name = baseUrl ++ "/game/" ++ name ++ "/begin"

thinkUrl name = baseUrl ++ "/game/" ++ name ++ "/think"

turnUrl name = baseUrl ++ "/game/" ++ name ++ "/turn"

beginGame: (Turn -> action) -> (Http.Error -> action) -> String -> Effects action
beginGame successAction failureAction name = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  Http.get Ser.turn (beginGameUrl name)
  |> Task.toResult
  |> Task.map handler
  |> Effects.task

think: (Thoughts -> action) -> (Http.Error -> action) -> String -> State -> Effects action
think successAction failureAction name state = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  postJson Ser.thoughts [] (thinkUrl name) (Ser.encodeState state)
  |> Task.map snd -- ignore the headers in the response
  |> Task.toResult
  |> Task.map handler
  |> Effects.task

turn: (Turn -> action) -> (Http.Error -> action) -> String -> Act -> Effects action
turn successAction failureAction name act = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  postJson Ser.turn [] (turnUrl name) (Ser.encodeAct act)
  |> Task.map snd -- ignore the headers in the response
  |> Task.toResult
  |> Task.map handler
  |> Effects.task
