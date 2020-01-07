namespace Project1

open System.IO
open AndrewKennedyTree
open PostScript
open TreeGenerator
open AST

module Driver =
    // Taken from https://en.wikibooks.org/wiki/F_Sharp_Programming/Higher_Order_Functions#A_Timer_Function
    let duration f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        returnValue

    [<EntryPoint>]
    let main argv =
        let postScriptWrapper (tree: 'a Tree) (filename: string) = 
            let (postree, extents) = designTree tree
            postScriptSaveResult postree extents filename

        let postScriptWrapperAst (ast: Program) (filename: string) =
            postScriptWrapper (convertToNodes ast) filename

        let testTree = Node("A", [
          Node("B", [Node("D", []); Node("E", []); Node("E", []); Node("E", []); Node("E", []); Node("E", []); Node("E", []); Node("E", [])]); 
          Node("F", [Node("F1", []); Node("F2", [])]); 
          Node("G", [Node("H", []); Node("I", []); Node("J", [])])])
        //postScriptWrapper (generate 10 10 1) "test"

        let testAst = P(
          [VarDec(ITyp, "x")],
          [
            Ass(AVar("x"), N(5));
            PrintLn(Access(AVar("x")))
          ]
        )
        //postScriptWrapperAst testAst "simpleAst"

        let sampleAst = P(
            [VarDec(ITyp, "y"); VarDec(ITyp, "x")],
            [Do(GC(
                [
                    Access(AVar("x")), [Ass(AVar("y"), Access(AVar("x"))); Ass(AVar("x"), N(5))];
                    Access(AVar("y")), [Ass(AVar("x"), Access(AVar("y"))); Ass(AVar("y"), N(10))]
                ]
            ))]
        )
        //postScriptWrapperAst sampleAst "GuardedCommandAst"

        let (postree, extents) = designTree (generate 200 200 10)

        // Warm up to ensure fairness in runs
        (postScriptStringBuilder postree extents) |> ignore

        printfn "Timing string builder"
        duration (fun() -> (postScriptStringBuilder postree extents) |> ignore)

        printfn "Timing string concat"
        duration (fun() -> (postScriptStringConcat postree extents) |> ignore)

        printfn "Timing string plus"
        duration (fun() -> (postScriptStringPlus postree extents) |> ignore)

        0