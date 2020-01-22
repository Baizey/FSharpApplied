module App

open GameState
open Fable.Core.JsInterop
open Fable.Import
open Browser.Dom
open Browser.Types

type Message =
    | Start
    | Turn of int * int
    | GameOver of bool
    | Clear
    | Opts
    | LoadGame of string
    | Cancel // Todo, where to use?
    | Error
    | Cancelled
    | Web of GameState

// An asynchronous event queue kindly provided by Don Syme 
type AsyncEventQueue<'T>() = 
    let mutable cont = None 
    let mutable queue = List.empty //Queue<'T>()
    let tryTrigger() = 
        match queue.Length, cont with 
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d -> 
            cont <- None
            // Hacky way to dequeue the list
            let reversed = List.rev queue
            queue <- List.rev queue.Tail
            d (reversed.Head)

    let tryListen(d) = 
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue <- msg :: queue; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)

let mutable StartDom : HTMLDivElement = unbox document.getElementById "start"
let mutable GameDom : HTMLDivElement = unbox document.getElementById "game"
let mutable FinishDom : HTMLDivElement = unbox document.getElementById "finished"

let mutable startBtn: HTMLButtonElement = unbox document.getElementById "start_start"
let mutable settingsBtn: HTMLButtonElement = unbox document.getElementById "start_settings"
let mutable loadingBtn: HTMLButtonElement = unbox document.getElementById "start_load"

let mutable gameBoard: HTMLDivElement = unbox document.getElementById "game_board"
let mutable gameText: HTMLParagraphElement = unbox document.getElementById "game_text"

let mutable finishedBtn: HTMLButtonElement = unbox document.getElementById "finished_start"
let mutable finishedText: HTMLParagraphElement = unbox document.getElementById "finished_text"

let showDom (dom: HTMLDivElement) =
    for a in [StartDom; GameDom; FinishDom] do
        a.classList.add "hidden"
    dom.classList.remove "hidden"

let renderPlayingDom (gs: GameState) =
    let elem i n = sprintf "<button id='%d-%d' class='mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect'>%d</button>" n i n
    let bucket i n = 
        if n = 0 then ""
        else
            let elems = [1..n] |> Seq.map (fun n -> elem i n) |> List.ofSeq |> List.fold (+) ""
            "<div class='bucket'>" + elems + "</div>"

    snd (List.fold (fun (i, acc) e -> (i + 1, acc + bucket i e)) (0, "") gs.Data)

let ev = AsyncEventQueue<Message>()

let makePlayingDomInteractive (gs:GameState) =
    [0..(gs.Data.Length - 1)]
    |> List.map (fun i -> [1..(gs.Data.Item i)] |> List.map (fun n -> (n, i)))
    |> List.reduce List.append
    |> List.fold (fun _ (n, i) -> 
        let mutable btn : HTMLButtonElement = unbox document.getElementById (sprintf "%d-%d" n i)
        btn.addEventListener("click", (fun _ -> ev.Post (Turn(n, i)))); ()) ()

let rec start() =
    async {
        printf "async start() called"
        showDom StartDom
        let rec stateChange =
            async {
                let! msg = ev.Receive()
                match msg with
                | Start -> return! playingPlayer(GameState([3;4;3;4;]))
                | LoadGame(url) -> return! loading(url)
                | _ -> return! stateChange
            }
        return! stateChange
        ()
    }
and playingPlayer(game:GameState) =
    async {
        printf "async playingPlayer() called"
        showDom GameDom
        gameText.innerText <- "Your turn..."
        gameBoard.innerHTML <- renderPlayingDom game
        makePlayingDomInteractive game |> ignore
        if game.IsGameOver then ev.Post(GameOver(false))
        let rec stateChange =
            async {
                let! msg = ev.Receive()
                match msg with
                | GameOver(false) -> return! finished("You lost!")
                | Turn(n, i) when (game.IsLegalMove n i) -> return! playingComputer(game.PlayerTurn n i)
                | _ -> return! stateChange
            }
        return! stateChange
        ()
    }
and playingComputer(game:GameState) =
    async {
        printf "async playingComputer() called"
        gameBoard.innerHTML <- renderPlayingDom game
        if game.IsGameOver then return! finished("You won!")
        gameText.innerText <- "Enemy thinking..."
        do! Async.Sleep(200)
        if game.WillWin then gameText.innerHTML <- "Enemy thinking...<br>Might as well give up now!"
        do! Async.Sleep(1000)
        return! playingPlayer(game.ComputerTurn)
        ()
    }
and finished(text:string) =
    async {
        printf "async finished() called"
        finishedText.innerText <- text
        showDom FinishDom
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
and loading(url) =
    async{
        
        ev.Post(Error)
        //Show some loading page with a cancel button and load a game
        let! msg = ev.Receive()
        match msg with
        | Error -> return! finished("Error loading game")
        | Cancel -> return! cancelling()
        | Web gs -> return! playingPlayer(gs)
        | _ -> failwith "Unexpected msg loading" 
    } 
and cancelling() =
    async{
        ()
        let! msg = ev.Receive()
        match msg with
        | Error | Cancelled | Web _  -> return! finished("Cancelled loading game")
        | _ -> failwith "Unexpected msg cancelling"
    }       
and options() =
    async {
        ()
    }

startBtn.addEventListener("click", (fun _ -> ev.Post Start))
finishedBtn.addEventListener("click", (fun _ -> ev.Post Clear))
loadingBtn.addEventListener("click", (fun _ -> ev.Post (LoadGame("webpage"))))


Async.StartImmediate(start())

// let sleepForSeconds secs =
//     async {
//         do! Async.Sleep(secs * 1000)
//         printfn "Slept for %d seconds" secs
//     }

// sleepForSeconds 5 |> Async.StartChild |> ignore