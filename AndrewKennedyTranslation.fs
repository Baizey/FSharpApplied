namespace Project1

module AndrewKennedyTree =
    type 'a Tree = Node of 'a * ('a Tree list)
    and 'a PosTree = PosNode of ('a * float) * ('a PosTree list)
    and Extent = (float * float) list

    let moveTree (PosNode((label, pos), children): 'a PosTree, x: float) =
        PosNode((label, pos + x), children)

    let moveExtent (extent: Extent, x: float): Extent =
        extent
        |> List.map (fun item ->
            let (p, q) = item
            (p + x, q + x))

    let rec merge (a: Extent, b: Extent) =
        match (a, b) with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge (ps, qs)

    let mean (x: float, y: float): float = (x + y) / 2.0

    let rec fit (a: Extent, b: Extent) =
        match (a, b) with
        | ((_, p) :: ps, (q, _) :: qs) -> max (fit (ps, qs)) (p - q + 1.0)
        | _, _ -> 0.0

    let fitlistLeft (es: Extent list) =
        let rec fitlist (acc: Extent, es: Extent list) =
            match (acc, es) with
            | (_, []) -> []
            | (acc, e :: es) ->
                let x = fit (acc, e)
                x :: fitlist (merge (acc, moveExtent (e, x)), es)
        fitlist ([], es)
        
    let fitlistRight (es: Extent list) =
        let rec fitlist (acc: Extent, es: Extent list) =
            match (acc, es) with
            | (_, []) -> []
            | (acc, e :: es) ->
                let x = -fit (e, acc)
                x :: fitlist (merge (moveExtent (e, x), acc), es)
        List.rev (fitlist ([], List.rev es))

    let fitlist (es: Extent list) =
        List.map mean (List.zip (fitlistLeft es) (fitlistRight es))

    let designTree (tree: 'a Tree) =
        let rec innerDesign (Node(label, children): 'a Tree) =
            let (trees, extents) = List.unzip (List.map innerDesign children)
            let positions = fitlist extents
            let ptrees = List.map moveTree (List.zip trees positions)
            let pextents = List.map moveExtent (List.zip extents positions)
            let flatExtents = List.fold (fun state item -> merge (state, item)) [] pextents
            let resultExtent = (0.0, 0.0) :: flatExtents
            let resulttree = PosNode((label, 0.0), ptrees)
            (resulttree, resultExtent)
        innerDesign (tree)