namespace Project1

open System.IO
open AndrewKennedyTree
open PostScript
open TreeGenerator

module Driver =
    [<EntryPoint>]
    let main argv =
        let testTree = Node("A", [
          Node("B", [Node("D", []); Node("E", []); Node("E", []); Node("E", []); Node("E", []); Node("E", []); Node("E", []); Node("E", [])]); 
          Node("F", [Node("F1", []); Node("F2", [])]); 
          Node("G", [Node("H", []); Node("I", []); Node("J", [])])])
        let (posTree, extents) = designTree (testTree)
        let postScript = (postScriptString posTree extents)
        File.WriteAllText (@".\test.ps", postScript) |> ignore
        0
