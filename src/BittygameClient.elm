module BittygameClient(beginGame) where

import Http
import Task
import Effects exposing (Effects)
import BittygameClient.Types exposing (..)
import BittygameClient.Serialization as Ser

baseUrl = "http://localhost:8080"

beginGameUrl name = baseUrl ++ "/game/" ++ name ++ "/begin"

thinkUrl name = baseUrl ++ "/game/" ++ name ++ "/turn"

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
  Http.post Ser.thoughts (thinkUrl name) (Http.string (Ser.encodeState state))
  |> Task.toResult
  |> Task.map handler
  |> Effects.task
