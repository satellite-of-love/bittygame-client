module BittygameClient.Serialization(turn, thoughts) where

import BittygameClient.Types exposing(..)
import Json.Decode as D exposing (Decoder, object1, object2, andThen, (:=), succeed)

-- DERODERS
distinguishInstruction : String -> Decoder Instruction
distinguishInstruction t =
  case t of
    "exit"  -> succeed ExitGame
    "print" -> object1 Print ("message" := D.string)
    "win"   -> succeed Win

instruction: Decoder Instruction
instruction =
  ("type" := D.string) `andThen` distinguishInstruction

turn: Decoder Turn
turn = object2 Turn
  ("game" := D.string)
  ("instructions" := (D.list instruction))

thoughts: Decoder Thoughts
thoughts = D.list D.string
