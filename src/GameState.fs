module GameState
    type GameState(gs: int list) =
        member _.Data = gs
        member _.IsGameOver:bool = List.forall (fun e -> e = 0) gs
        member _.PlayerTurn (ii:int, n:int):GameState = GameState(List.mapi (fun i e -> if i = ii then e - n else e) gs)
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