namespace Lasse

module SimpleTree = 
    type Node =
        { name: string
          children: List<Node> }

    let rec displayTree (node: Node) = 
        let rec displayNode (node: Node, depth: int) =
            let rec displayChildren (list: List<Node>, depth: int) =
                match list with
                | head :: tail ->
                    displayNode (head, depth)
                    displayChildren (tail, depth)
                | [] -> 0
            printfn "%s%s" (String.replicate depth "  ") node.name
            displayChildren (node.children, depth + 1) |> ignore
        displayNode (node, 0)
        
    let A =
        { name = "A"
          children = [] }

    let B =
        { name = "B"
          children = [] }

    let C =
        { name = "C"
          children = [ A ] }

    let D =
        { name = "D"
          children = [ B ] }

    let E =
        { name = "E"
          children = [ C; D ] }