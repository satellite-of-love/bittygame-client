module BittygameClient(beginGame) where

import Http
import Task
import Effects exposing (Effects)
import BittygameClient.Types exposing (..)
import BittygameClient.Serialization as Decoder

baseUrl = "http://localhost:8080"

beginGameUrl name = baseUrl ++ "/game/" ++ name ++ "/begin"

beginGame: (Turn -> action) -> (Http.Error -> action) -> String -> Effects action
beginGame successAction failureAction name = 
  let
    handler result =
      case result of
        Ok  but -> successAction but
        Err poo -> failureAction poo
  in
  Http.get Decoder.turn (beginGameUrl name)
  |> Task.toResult
  |> Task.map handler
  |> Effects.task