module BittygameClient.Types where

type alias ScenarioName = String

type alias Thoughts = List String

type alias GameID = String

type Instruction = 
    ExitGame
  | Print String
  | Win

type alias Turn =
  {
    game: GameID,
    instructions: List Instruction
  }

type alias GameAction = String