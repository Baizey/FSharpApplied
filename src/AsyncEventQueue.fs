module AsyncEventQueue
    type Message =
    | Start
    | Turn of int * int
    | GameOver of bool
    | Clear
    | Opts
    | Load
    | Apply
    | Cancel
    | Error
    | Cancelled

    type AsyncEventQueue<'T>() = 
            let mutable cont = None 
            let mutable queue = List.empty
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
            member x.Receive():Async<'T> = 
                Async.FromContinuations (fun (cont,econt,ccont) -> 
                    tryListen cont)