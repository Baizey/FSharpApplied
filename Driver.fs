namespace Project1

open AndrewKennedyTree

module Driver =
    [<EntryPoint>]
    let main argv =
        let testTree = Node("A", [
          Node("B", [Node("C", []); Node("D", []); Node("E", [])]); 
          Node("F", []); 
          Node("G", [Node("H", []); Node("I", []); Node("J", [])])])
        let posTree = designTree testTree
        printfn "%A" posTree
        0
