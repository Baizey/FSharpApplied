namespace Project1

open System.IO
open AndrewKennedyTree
open PostScript
open TreeGenerator
open AST

module Driver =
    [<EntryPoint>]
    let main argv =
        let testTree = Node("A", [
          Node("B", [Node("D", []); Node("E", [])]); 
          Node("F", [Node("F1", []); Node("F2", [])]); 
          Node("G", [Node("H", []); Node("I", []); Node("J", [])])])
        let testAst = P(
          [VarDec(ITyp, "x")],
          [
            Ass(AVar("x"), N(5));
            PrintLn(Access(AVar("x")))
          ]
        )
        let nodes = convertToNodes testAst
        //let posTree = designTree (generate 2 10)
        let posTree = designTree nodes
        let postScript = (postScriptString posTree)
        File.WriteAllText (@".\test.ps", postScript) |> ignore
        0
