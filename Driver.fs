namespace Project1

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

module Driver =
    // Taken from https://en.wikibooks.org/wiki/F_Sharp_Programming/Higher_Order_Functions#A_Timer_Function
    let duration f =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        returnValue
        
    let project2 =

        System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
        let ex1Tree = parseFromFile "Examples/Skip.gc"
        let _ = tcP ex1Tree

        // obtain symbolic code:
        //let ex1Code = CP ex1Tree

        // -- is executed with trace as follows:
        let stack = goTrace ex1Tree

        // -- is executed as follows (no trace):
        let sameStack = go ex1Tree

        // "All in one" parse from file, type check, compile and run
        //let _ = exec "Ex1.gc"
        0

    [<EntryPoint>]
    let main argv =
        project2 |> ignore
        0
