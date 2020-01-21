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
    | Web of string

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

let showDom (dom: HTMLDivElement) =
    for a in [StartDom; GameDom; FinishDom] do
        a.classList.add "hidden"
    dom.classList.remove "hidden"

let renderPlayingDom (gs: GameState) =
    
    let elem b n = sprintf "<button id='%d-%d' class='mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect'>-</button>" b n
    let bucket b n = 
        let elems = [0..n] |> Seq.map (fun n -> elem b n) |> List.ofSeq |> List.fold (+) ""
        "<div class='bucket'>" + elems + "<span>" + string n + "</span></div>"

    snd (List.fold (fun (i, acc) e -> (i + 1, acc + bucket i e)) (0, "") gs.Data)


let ev = AsyncEventQueue<Message>()

let rec start() =
    async {
        printf "async start() called"
        showDom StartDom
        let! msg = ev.Receive()
        match msg with
        | Start -> return! playingPlayer(GameState([20;3;5]))
        | LoadGame(url) -> return! loading(url)
        | _ -> failwith "unexpected msg"
        ()
    }
and playingPlayer(gs:GameState) =
    async {
        printf "async playingPlayer() called"
        showDom GameDom
        GameDom.innerHTML <- renderPlayingDom gs
        let! msg = ev.Receive()
        match msg with
        | Turn(n,i) -> return! playingComputer(gs.PlayerTurn(n, i))
        | GameOver(b) -> return! finished(b)
        | _ -> failwith "unexpected msg"
        ()
    }
and playingComputer(gs:GameState) =
    async {
        printf "async playingComputer() called"
        do! Async.Sleep(2 * 1000)
        return! playingPlayer(gs.ComputerTurn)
        ()
    }
and finished(playerWon:bool) =
    async {
        printf "async finished() called"
        showDom FinishDom
        // TODO: display who won
        let! msg = ev.Receive()
        match msg with
        | Clear -> return! start()
        | _ -> failwith "unexpected msg"
        ()
    }
and loading(url) =
    async{
        ()
    } 
and cancelling() =
    async{
        ()
    }       
and options() =
    async {
        ()
    }

startBtn.addEventListener("click", (fun _ -> ev.Post Start))


Async.StartImmediate(start())

// let sleepForSeconds secs =
//     async {
//         do! Async.Sleep(secs * 1000)
//         printfn "Slept for %d seconds" secs
//     }

// sleepForSeconds 5 |> Async.StartChild |> ignore