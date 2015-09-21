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
    "unknown" -> object1 IDontKnowHowTo ("what" := D.string)
    "denied"  -> object1 CantDoThat ("why" := D.string)
    "acquire" -> object1 Acquire ("item" := item)
    "increase" -> object1 IncreaseStat ("stat" := D.string)

instruction: Decoder Instruction
instruction =
  ("type" := D.string) `andThen` distinguishInstruction

turn: Decoder Turn
turn = object2 Turn
  ("gameID" := D.string)
  ("instructions" := (D.list instruction))
 
item : Decoder Item
item = object1 Item
  ("name" := D.string)

thoughts: Decoder Thoughts
thoughts = D.list D.string
