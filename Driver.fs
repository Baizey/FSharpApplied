namespace Project1

open System.IO
open AndrewKennedyTree
open PostScript
open TreeGenerator
open AST

module Driver =
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

        let (posTree, extents) = designTree (testTree)
        let postScript = (postScriptString posTree extents)
        //let posTree = designTree (generate 2 10)
        File.WriteAllText (@".\test.ps", postScript) |> ignore
        let testAst = P(
          [VarDec(ITyp, "x")],
          [
            Ass(AVar("x"), N(5));
            PrintLn(Access(AVar("x")))
          ]
        )
        postScriptWrapperAst testAst "simpleAst"

        let sampleAst = P(
            [VarDec(ITyp, "y"); VarDec(ITyp, "x")],
            [Do(GC(
                [
                    Access(AVar("x")), [Ass(AVar("y"), Access(AVar("x"))); Ass(AVar("x"), N(5))];
                    Access(AVar("y")), [Ass(AVar("x"), Access(AVar("y"))); Ass(AVar("y"), N(10))]
                ]
            ))]
        )
        postScriptWrapperAst sampleAst "GuardedCommandAst"

        0



(* Factorial
GC code:

y:=1;
do x>0 -> y:=x*y;
          x:=x-1
od
*)