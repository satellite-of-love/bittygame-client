module BittygameClient.Types where

type Instruction = 
    ExitGame
  | Print String

type alias State = 
  {
    inventory: List String
  }

type alias Turn =
  {
    state: State,
    instructions: List Instruction
  }