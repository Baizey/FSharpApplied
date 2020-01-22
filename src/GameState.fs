module GameState
    type GameState(gs: int list) =
        member _.Data = gs
        member _.IsGameOver:bool = List.forall (fun e -> e = 0) gs
        member _.IsLegalMove (n:int) (index:int):bool = (0 <= index && index < gs.Length) && (0 < n && n <= gs.Item index)
        member _.PlayerTurn (n:int) (index:int):GameState = GameState(List.mapi (fun i e -> if i = index then e - n else e) gs)
        member _.ComputerTurn:GameState =
            let rec zero max = function
                | [] -> []
                | h :: t when h = max -> h - 1 :: t
                | h :: t -> h :: zero max t
            and aboveZero m = function
                | [] -> []
                | h :: t when h > h ^^^ m -> (h ^^^ m) :: t
                | h :: t -> h :: aboveZero m t
            match List.fold (^^^) 0 gs with
            | 0 -> GameState(zero (List.max gs) gs)
            | m -> GameState(aboveZero m gs)
        member this.WillWin:bool =
            let rec inner (gs:GameState) = function
                | i when i % 2 = 0 -> 
                    if gs.IsGameOver then true
                    else inner gs.ComputerTurn (i + 1)
                | i -> 
                    if gs.IsGameOver then false
                    else inner gs.ComputerTurn (i + 1)
            inner this 1