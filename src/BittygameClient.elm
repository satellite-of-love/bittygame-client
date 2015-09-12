module BittyGame where

import Http
import Json.Decode as Json

type Instruction = 
    ExitGame
  | Print String

type alias Turn =
  {
    state: 
      {
        inventory: List String
      },
    instructions: List Instruction
  }

decodeInstruction: Json.Decoder Instruction
decodeInstruction =

beginGame: String -> Effects 
beginGame name =