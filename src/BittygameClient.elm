module BittygameClient(beginGame, think, turn) where

import Http
import Task
import GetWithHeaders exposing (postJson)
import Effects exposing (Effects)
import BittygameClient.Types exposing (..)
import BittygameClient.Serialization as Ser

beginGameUrl baseUrl name = baseUrl ++ "/game/" ++ name ++ "/begin"

thinkUrl baseUrl name = baseUrl ++ "/game/" ++ name ++ "/think"

turnUrl baseUrl name = baseUrl ++ "/game/" ++ name ++ "/turn"

beginGame: String -> (Turn -> action) -> (Http.Error -> action) -> String -> Effects action
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

think: String -> (Thoughts -> action) -> (Http.Error -> action) -> String -> State -> Effects action
think baseUrl successAction failureAction name state = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  postJson Ser.thoughts [] (thinkUrl baseUrl name) (Ser.encodeState state)
  |> Task.map snd -- ignore the headers in the response
  |> Task.toResult
  |> Task.map handler
  |> Effects.task

turn: String -> (Turn -> action) -> (Http.Error -> action) -> String -> Act -> Effects action
turn baseUrl successAction failureAction name act = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  postJson Ser.turn [] (turnUrl baseUrl name) (Ser.encodeAct act)
  |> Task.map snd -- ignore the headers in the response
  |> Task.toResult
  |> Task.map handler
  |> Effects.task
