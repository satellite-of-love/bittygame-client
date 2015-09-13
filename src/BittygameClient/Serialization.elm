module BittygameClient.Serialization(turn, thoughts, encodeState) where

import BittygameClient.Types exposing(..)
import Json.Decode as D exposing (Decoder, object1, object2, andThen, (:=), succeed)
import Json.Encode as E exposing (Value)

-- DERODERS
distinguishInstruction : String -> Decoder Instruction
distinguishInstruction t =
  case t of
    "exit" -> succeed ExitGame
    "print" -> object1 Print ("message" := D.string)

instruction: Decoder Instruction
instruction =
  ("type" := D.string) `andThen` distinguishInstruction

state: Decoder State
state = 
 object1 State 
    ("inventory" := (D.list D.string))

turn: Decoder Turn
turn = object2 Turn
  ("state" := state)
  ("instructions" := (D.list instruction))

thoughts: Decoder Thoughts
thoughts = D.list D.string

--- ENCODERS
encodeState: State -> Value
encodeState state = 
  E.object
    [
      ("inventory", E.list (List.map E.string state.inventory))
    ]