namespace Project1

open System.IO
open AndrewKennedyTree
open PostScript
open TreeGenerator

module Driver =
    [<EntryPoint>]
    let main argv =
        let testTree = Node("A", [
          Node("B", [Node("D", []); Node("E", [])]); 
          Node("F", [Node("F1", []); Node("F2", [])]); 
          Node("G", [Node("H", []); Node("I", []); Node("J", [])])])
        let posTree = designTree (generate 2 10)
        let postScript = (postScriptString posTree)
        File.WriteAllText (@".\test.ps", postScript) |> ignore
        0
