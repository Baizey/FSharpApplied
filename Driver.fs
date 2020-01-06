namespace Project1

open AndrewKennedyTree
open PostScript

module Driver =
    [<EntryPoint>]
    let main argv =
        let testTree = Node("A", [
          Node("B", [Node("D", []); Node("E", [])]); 
          Node("F", [Node("F1", []); Node("F2", [])]); 
          Node("G", [Node("H", []); Node("I", []); Node("J", [])])])
        let posTree = designTree testTree
        printfn "%A" (postScriptString posTree)
        0
