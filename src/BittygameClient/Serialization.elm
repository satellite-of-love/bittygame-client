module BittygameClient.Serialization(turn) where

import BittygameClient.Types exposing(..)
import Json.Decode exposing (Decoder, list, string, object1, object2, andThen, (:=), succeed)

distinguishInstruction : String -> Decoder Instruction
distinguishInstruction t =
  case t of
    "exit" -> succeed ExitGame
    "print" -> object1 Print ("message" := string)

instruction: Decoder Instruction
instruction =
  ("type" := string) `andThen` distinguishInstruction

state: Decoder State
state = 
 object1 State 
    ("inventory" := (list string))

turn: Decoder Turn
turn = object2 Turn
  ("state" := state)
  ("instructions" := (list instruction))