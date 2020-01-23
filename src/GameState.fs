
module GameState
    open System

    type GameSettings = 
        { 
            mutable buckets: int
            mutable matches : int
            mutable easyMode: bool
        }
    
    let defaultSettings = { buckets = 5; matches = 5; easyMode = false }

    type Turn = (int * int)
    type GameState(gs: int list, hasTaunted': bool, easyMode: bool) =
        let mutable hasTaunted:bool = hasTaunted'

        let computerTurnHard(): Turn =
            let rec zero max i = function
                | [] -> failwith "zero fail"
                | h :: t when h = max -> (1, i)
                | h :: t -> zero max (i + 1) t
            and aboveZero m i = function
                | [] -> failwith "aboveZero fail"
                | h :: t when h > h ^^^ m -> (h - (h ^^^ m), i)
                | h :: t -> aboveZero m (i + 1) t
            match List.fold (^^^) 0 gs with
            | 0 -> zero (List.max gs) 0 gs
            | m -> aboveZero m 0 gs

        let isLegalMove ((n, index): Turn): bool = (0 <= index && index < gs.Length) && (0 < n && n <= gs.Item index)
        let computerTurnEasy(): Turn =
            let rnd = Random()

            let nonEmptyBuckets = List.filter (fun (_, x) -> x > 0) (List.mapi (fun i e -> (i, e)) gs)
            let (bucket, maxAmount) = nonEmptyBuckets.Item (rnd.Next nonEmptyBuckets.Length)
            let amount = 1 + rnd.Next maxAmount
            (amount, bucket)

        static member CreateGame (settings:GameSettings) : GameState =
            GameState(Array.create settings.buckets settings.matches |> List.ofArray, false, settings.easyMode)

        member x.CanTaunt = (not hasTaunted) && x.WillWin
        member _.Taunted = hasTaunted <- true
        member _.IsEasyMode = easyMode
        member _.Data = gs
        member _.IsGameOver: bool = List.forall (fun e -> e = 0) gs
        member _.IsLegalMove (turn: Turn): bool = isLegalMove turn
        member _.PlayerTurn ((n, index): Turn): GameState = GameState(List.mapi (fun i e -> if i = index then e - n else e) gs, hasTaunted, easyMode)
        member _.ComputerDecision = if easyMode then computerTurnEasy() else computerTurnHard()
        member x.ComputerTurn: GameState = 
            let turn = x.ComputerDecision
            x.PlayerTurn turn
         
        member this.WillWin : bool =
            let rec inner (gs:GameState) = function
                | i when i % 2 = 0 -> 
                    if gs.IsGameOver then true
                    else inner gs.ComputerTurn (i + 1)
                | i -> 
                    if gs.IsGameOver then false
                    else inner gs.ComputerTurn (i + 1)
            inner this 1

        override this.ToString() =
            sprintf "Board: %A, hasTaunted: %b, easyMode: %b" gs hasTaunted easyMode 



            
// module GameState
//     open System

//     type GameSettings = 
//         { 
//             mutable buckets: int
//             mutable matches : int
//             mutable easyMode: bool
//         }
    
//     let defaultSettings = { buckets = 5; matches = 5; easyMode = false }

//     type Turn = (int * int)
//     type GameState(gs: int list, hasTaunted': bool, easyMode: bool) =
//         let mutable hasTaunted:bool = hasTaunted'
        
//         let computerTurnHard(): Turn =
//             //let rec zero max = function
//             //    | [] -> []
//             //    | h :: t when h = max -> h - 1 :: t
//             //    | h :: t -> h :: zero max t
//             let rec zeroTurn max acc = function
//                 | [] -> failwith "zeroTurn: should not occur..."
//                 | h :: _ when h = max -> (1, acc)
//                 | _ :: t -> zeroTurn max (acc + 1) t
//             //and aboveZero m = function
//             //    | [] -> []
//             //    | h :: t when h > h ^^^ m -> (h ^^^ m) :: t
//             //    | h :: t -> h :: aboveZero m t
//             and aboveZeroTurn m acc = function
//                 | [] -> failwith "aboveZeroTurn: should not occur..."
//                 | h :: _ when h > h ^^^ m -> (h ^^^ m, acc)
//                 | _ :: t -> aboveZeroTurn m (acc + 1) t
//             match List.fold (^^^) 0 gs with
//             | 0 -> zeroTurn (List.max gs) 0 gs
//             | m -> aboveZeroTurn m 0 gs
//             //| 0 -> GameState(zero (List.max gs) gs, hasTaunted, easyMode)
//             //| m -> GameState(aboveZero m gs, hasTaunted, easyMode)

//         let turn ((n, index): Turn): GameState = GameState(List.mapi (fun i e -> if i = index then e - n else e) gs, hasTaunted, easyMode)
//         let isLegalMove ((n, index): Turn): bool = (0 <= index && index < gs.Length) && (0 < n && n <= gs.Item index)
//         let computerTurnEasy(): Turn =
//             let rnd = Random()

//             let nonEmptyBuckets = List.filter (fun (_, x) -> x > 0) (List.mapi (fun i e -> (i, e)) gs)
//             let (bucket, maxAmnt) = nonEmptyBuckets.Item (rnd.Next nonEmptyBuckets.Length)
//             let amount = 1 + rnd.Next maxAmnt
//             (amount, bucket)

//             // printf "move was legal: %b (%d, %d)" (isLegalMove amnt bckt) bckt amnt
//             // GameState(List.mapi (fun i e -> if i = bckt then e - amnt else e) gs, hasTaunted, easyMode)

//         let computerDecision(): Turn =
//             match easyMode with
//             | true -> computerTurnEasy()
//             | false -> computerTurnHard()

//         static member CreateGame (settings:GameSettings) : GameState =
//             GameState(Array.create settings.buckets settings.matches |> List.ofArray, false, settings.easyMode)

//         member x.CanTaunt = (not hasTaunted) && x.WillWin
//         member _.Taunted = hasTaunted <- true
//         member _.IsEasyMode = easyMode
//         member _.Data = gs
//         member _.IsGameOver:bool = List.forall (fun e -> e = 0) gs
//         member _.IsLegalMove (turn: Turn): bool = isLegalMove turn
//         member _.Turn (move: Turn): GameState = turn move
//         member _.ComputerDecision: Turn = computerDecision()
//         member _.ComputerTurn: GameState = 
//             let move = computerDecision()
//             turn move
//         //if easyMode then computerTurnEasy() else computerTurnHard()
         
//         member this.WillWin : bool =
//             let rec inner (gs:GameState) = function
//                 | i when i % 2 = 0 -> 
//                     if gs.IsGameOver then true
//                     else inner gs.ComputerTurn (i + 1)
//                 | i -> 
//                     if gs.IsGameOver then false
//                     else inner gs.ComputerTurn (i + 1)
//             inner this 1