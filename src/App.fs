module App

open AsyncEventQueue
open GameState
open Ui

let ev = AsyncEventQueue<Message>()
let ui = Ui(ev)

let settings = defaultSettings

let rec start() =
    async {
        ui.ShowStart
        let rec stateChange =
            async {
                let! msg = ev.Receive()
                match msg with
                | Load -> return! loading()
                | Opts -> return! options()
                | _ -> return! stateChange
            }
        return! stateChange
        ()
    }
and playingPlayer(game:GameState) =
    async {
        printf "StateMachine: playingPlayer %A" game
        ui.RenderGame game true
        if game.IsGameOver then ev.Post(GameOver(false))
        let rec stateChange =
            async {
                let! msg = ev.Receive()
                // printf "%A" msg
                match msg with
                | GameOver(false) -> return! finished("You lost!")
                | Turn(n, i) when (game.IsLegalMove (n, i)) -> return! playingComputer(game.PlayerTurn (n, i))
                | _ -> return! stateChange
            }
        return! stateChange
        ()
    }
and playingComputer(game:GameState) =
    async {
        printf "StateMachine: playingComputer %A" game
        ui.RenderGame game false
        if game.IsGameOver then return! finished("You won!")
        do! Async.Sleep(1000)
        printf "Doing AI move"
        return! playingPlayer(game.ComputerTurn)
        ()
    }
and finished(text:string) =
    async {
        ui.ShowFinish text
        let rec stateChange =
            async {
                let! msg = ev.Receive()
                match msg with
                | Clear -> return! start()
                | _ -> return! stateChange
            }
        return! stateChange
        ()
    }
and loading() =
    async{
        ui.ShowLoading
        use ts = new System.Threading.CancellationTokenSource()
        Async.StartWithContinuations
                 (async { do! Async.Sleep(2500) },
                  (fun _ -> ev.Post Start),
                  (fun _ -> ev.Post Error),
                  (fun _ -> ev.Post Cancelled),
                  ts.Token)
        let! msg = ev.Receive()
        match msg with
        | Error -> return! finished("Error loading game")
        | Cancel -> ts.Cancel()
                    return! cancelling()
        | Start -> 
            let game = GameState.CreateGame settings
            printf "%A" game
            return! playingPlayer(game)
        | _ -> failwith "Unexpected msg loading" 
    } 
and cancelling() =
    async{
        ()
        let! msg = ev.Receive()
        match msg with
        | Error | Cancelled | Start  -> return! finished("Cancelled loading game")
        | _ -> failwith "Unexpected msg cancelling"
    }       
and options() =
    async {
        ui.ShowSettings

        let! msg = ev.Receive()
        match msg with
        | Apply -> 
            settings.buckets <- ui.SettingsBuckets
            settings.matches <- ui.SettingsMatches
            settings.easyMode <- ui.SettingsEasyMode
        | Cancel -> 
            ui.SetBuckets settings.buckets
            ui.SetMatches settings.matches
            ui.SetEasyMode settings.easyMode
        | _ -> ()
        return! start()
    }

Async.StartImmediate(start())

// let sleepForSeconds secs =
//     async {
//         do! Async.Sleep(secs * 1000)
//         printfn "Slept for %d seconds" secs
//     }

// sleepForSeconds 5 |> Async.StartChild |> ignore