module BittygameClient.Types where

type alias ScenarioName = String

type alias Thoughts = List String

type alias GameID = String

type alias Stat = String

type Instruction = 
    ExitGame
  | Print String
  | Win
  | IDontKnowHowTo String
  | CantDoThat String
  | Acquire Item
  | IncreaseStat Stat

type alias Item =
  {
    name: String
  }

type alias Turn =
  {
    gameID: GameID,
    instructions: List Instruction
  }

type alias GameAction = String