
module GameState
    open System

    type GameSettings = 
        { 
            mutable buckets: int
            mutable matches : int
            mutable easyMode: bool
        }
    
    let defaultSettings = { buckets = 5; matches = 5; easyMode = false }

    type GameState(gs: int list, hasTaunted': bool, easyMode: bool) =
        let mutable hasTaunted:bool = hasTaunted'
        
        let computerTurnHard(): GameState =
            let rec zero max = function
                | [] -> []
                | h :: t when h = max -> h - 1 :: t
                | h :: t -> h :: zero max t
            and aboveZero m = function
                | [] -> []
                | h :: t when h > h ^^^ m -> (h ^^^ m) :: t
                | h :: t -> h :: aboveZero m t
            match List.fold (^^^) 0 gs with
            | 0 -> GameState(zero (List.max gs) gs, hasTaunted, easyMode)
            | m -> GameState(aboveZero m gs, hasTaunted, easyMode)

        let isLegalMove (n:int) (index:int):bool = (0 <= index && index < gs.Length) && (0 < n && n <= gs.Item index)
        let computerTurnEasy(): GameState =
            let rnd = Random()

            let nonEmptyBuckets = List.filter (fun (_, x) -> x > 0) (List.mapi (fun i e -> (i, e)) gs)
            let (bckt, maxAmnt) = nonEmptyBuckets.Item (rnd.Next nonEmptyBuckets.Length)
            let amnt = 1 + rnd.Next maxAmnt

            // printf "move was legal: %b (%d, %d)" (isLegalMove amnt bckt) bckt amnt
            GameState(List.mapi (fun i e -> if i = bckt then e - amnt else e) gs, hasTaunted, easyMode)

        static member CreateGame (settings:GameSettings) : GameState =
            GameState(Array.create settings.buckets settings.matches |> List.ofArray, false, settings.easyMode)

        member x.CanTaunt = (not hasTaunted) && x.WillWin
        member _.Taunted = hasTaunted <- true
        member _.IsEasyMode = easyMode
        member _.Data = gs
        member _.IsGameOver:bool = List.forall (fun e -> e = 0) gs
        member _.IsLegalMove (n:int) (index:int):bool = isLegalMove n index
        member _.PlayerTurn (n:int) (index:int):GameState = GameState(List.mapi (fun i e -> if i = index then e - n else e) gs, hasTaunted, easyMode)
        member _.ComputerTurn: GameState = if easyMode then computerTurnEasy() else computerTurnHard()
         
        member this.WillWin : bool =
            let rec inner (gs:GameState) = function
                | i when i % 2 = 0 -> 
                    if gs.IsGameOver then true
                    else inner gs.ComputerTurn (i + 1)
                | i -> 
                    if gs.IsGameOver then false
                    else inner gs.ComputerTurn (i + 1)
            inner this 1