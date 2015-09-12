module BittygameClient.Types where

type alias Thoughts = List String

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