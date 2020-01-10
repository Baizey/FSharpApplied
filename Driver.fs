﻿namespace Project1

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Backend.CodeGeneration
open Machine

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
        (*
    let project2 =
        System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
        // The Ex0.gc example:
        let ex0Tree = parseFromFile "Ex0.gc"

        let _ = tcP ex0Tree

        let ex0Code = CP ex0Tree

        let _ = go ex0Tree

        let _ = goTrace ex0Tree


        // Parsing of Ex1.gc

        let ex1Tree = parseFromFile "Ex1.gc"

        // -- is typechecked as follows:

        let _ = tcP ex1Tree

        // obtain symbolic code:
        let ex1Code = CP ex1Tree

        // -- is executed with trace as follows:
        let stack = goTrace ex1Tree

        // -- is executed as follows (no trace):
        let sameStack = go ex1Tree

        // "All in one" parse from file, type check, compile and run

        let _ = exec "Ex1.gc"

        let _ = exec "Ex2.gc"

        // Test of programs covered by the fifth task using optimized compilation (Section 8.2):
        List.iter execOpt [ "Ex1.gc"; "Ex2.gc" ]

        // All programs relating to the basic version can be parsed:
        let pts = List.map parseFromFile [ "Ex1.gc"; "Ex2.gc"; "Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc" ]

        // The parse tree for Ex3.gc
        List.item
        (*
                // Test of programs covered by the first task (Section 3.7):
                List.iter exec ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;

                // Test of programs covered by the second task (Section 4.3):
                List.iter exec ["Ex7.gc"; "fact.gc"; "factRec.gc"; "factCBV.gc"];;

                // Test of programs covered by the fourth task (Section 5.4):
                List.iter exec ["A0.gc"; "A1.gc"; "A2.gc"; "A3.gc"];;

                // Test of programs covered by the fifth task (Section 6.1):
                List.iter exec ["A4.gc"; "Swap.gc"; "QuickSortV1.gc"];;

                // Test of programs covered by the fifth task (Section 7.4):
                List.iter exec ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

                // Test of programs covered by the fifth task using optimized compilation (Section 8.2):
                List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

        *) 2 pts
        0
*)
    [<EntryPoint>]
    let main argv =
        System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
        let tree = parseFromFile "mul.gc"
        let code = CP tree
        let stack = goTrace tree
        0
