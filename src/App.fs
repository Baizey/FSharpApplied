module App

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
    | Cancel // Todo, where to use?

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

type GameState = int list

let turn (gs:GameState) (ii, n): GameState = List.mapi (fun i e -> if i = ii then e - n else e) gs

let isGameOver (gs:GameState) = List.forall (fun e -> e = 0) gs

let aiMove (gs:GameState): GameState =
    let rec zero max = function
        | [] -> []
        | h :: t when h = max -> h - 1 :: t
        | h :: t -> h :: zero max t
    and aboveZero m = function
        | [] -> []
        | h :: t when h > h ^^^ m -> (h ^^^ m) :: t
        | h :: t -> h :: aboveZero m t
    match List.fold (^^^) 0 gs with
    | 0 -> zero (List.max gs) gs
    | m -> aboveZero m gs



let ev = AsyncEventQueue<Message>()

let rec init() =
    async {
        printf "async init() started"
        showDom StartDom
        let! msg = ev.Receive()
        match msg with
        | Start -> return! start()
        | _ -> failwith "unexpected msg"
        ()
    }
and start() =
    async {
        printf "async start() called"
        showDom GameDom
        ()
    }
and playingPlayer() =
    async {
        ()
    }
and playingComputer() =
    async {
        ()
    }
and finished() =
    async {
        showDom FinishDom
        ()
    }
and options() =
    async {
        ()
    }

startBtn.addEventListener("click", (fun _ -> ev.Post Start))


Async.StartImmediate(init())

let sleepForSeconds secs =
    async {
        do! Async.Sleep(secs * 1000)
        printfn "Slept for %d seconds" secs
    }

sleepForSeconds 5 |> Async.StartChild |> ignore