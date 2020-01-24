
// Setup FsCheck
#I "C:/Users/Stefan/.nuget/packages/fscheck/2.14.0/lib/netstandard2.0"
#r "FsCheck.dll"
open FsCheck

// Load in GameState
#load "GameState.fs"
open GameState

// Generator
let gsGen = Arb.generate<int list * bool * bool>

let gameStateGen: Gen<GameState> =
  gen {
    return! gsGen
      |> Gen.filter (fun (l, _, _) -> l.Length > 0)
      |> Gen.map (fun (l, t, b) -> (List.map abs l, t, b))
      |> Gen.filter (fun (l, _, _) -> not (List.forall (fun x -> x = 0) l) )
      |> Gen.map (fun (l, t, e) -> GameState(l, t, e))
  }

type GameStateGenerator =
  static member GameState() = Arb.fromGen gameStateGen

Arb.register<GameStateGenerator>()

let config = {
  Config.Quick with
    MaxTest = 1000
    EndSize = 10000
}

let moveWasLegal (gs: GameState) = gs.IsLegalMove (gs.ComputerDecision)
Check.One(config, moveWasLegal)

let hardModeStrategy (gs: GameState) =
  not gs.IsEasyMode ==> 
    let board = gs.Data
    let newBoard = gs.ComputerTurn
    match List.fold (^^^) 0 board with
    // If m = 0 then remove 1 match from biggest heap
    | 0 -> (List.fold (^^^) 0 newBoard.Data) <> 0 
    // if m <> 0 then move to obtain m = 0
    | m -> (List.fold (^^^) 0 newBoard.Data) = 0 
Check.One(config, hardModeStrategy)