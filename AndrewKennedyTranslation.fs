namespace Lasse

module AndrewKennedyTree =
    type Node = string * float

    type Tree =
        { node: Node
          children: Tree list }

    type Extent = (float * float) list

    let moveTree (tree: Tree, x: float): Tree =
        let (label, at) = tree.node

        let newTree =
            { node = Node(label, at + x)
              children = tree.children }
        newTree

    let moveExtent (extent: Extent, x: float): Extent =
        extent
        |> List.map (fun item ->
            match item with
            | (p, q) -> (p + x, q + x))

    let rec merge (a: Extent, b: Extent): Extent =
        match (a, b) with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge (ps, qs)

    let rmax (p: float, q: float): float =
        if p > q then p
        else q

    let mean (x: float, y: float): float = (x + y) / 2.0

    let rec fit (a: Extent, b: Extent): float =
        match (a, b) with
        | ((_, p) :: ps, (q, _) :: qs) -> rmax (fit (ps, qs), p - q + 1.0)
        | _, _ -> 0.0

    let flipExtent (extent: Extent): Extent =
        extent
        |> List.map (fun item ->
            match item with
            | (p, q) -> (-q, -p))

    let fitlistLeft (es: Extent list): float list =
        let rec fitlist (acc: Extent, es: Extent list) =
            match (acc, es) with
            | (_, []) -> []
            | (acc, e :: es) ->
                let x = fit (acc, e)
                x :: fitlist (merge (acc, moveExtent (e, x)), es)
        fitlist ([], es)

    let fitlistRight (es: Extent list): float list =
        let rec fitlist (acc: Extent, es: Extent list) =
            match (acc, es) with
            | (_, []) -> []
            | (acc, e :: es) ->
                let x = fit (acc, e)
                x :: fitlist (merge (moveExtent (e, x), acc), es)
        fitlist ([], es)

    let fitlist (es: Extent list): float list =
        let a = fitlistLeft es
        let b = fitlistRight es
        let zipped = List.zip a b
        List.map mean zipped

    let designTree (tree: Tree) =
        let rec innerDesign (tree: Tree) =
            let (label, _) = tree.node
            let (trees, extents) = List.unzip (List.map innerDesign tree.children)
            let positions = fitlist extents

            let zippedTrees = List.zip trees positions
            let ptrees = List.map moveTree zippedTrees

            let zippedExtents = List.zip extents positions
            let pextents = List.map moveExtent zippedExtents

            let flatExtents = List.fold (fun state item -> merge (state, item)) [] pextents

            let resultExtent = (0.0, 0.0) :: flatExtents

            let resulttree =
                { node = (label, 0.0)
                  children = ptrees }
            (resulttree, resultExtent)
        innerDesign (tree)
