namespace Project1

open AndrewKennedyTree

module TreeGenerator =


    let random (min: int) (max: int) =
        let r = System.Random()
        r.Next(min, max)

    let rec addEmpty (list: string Tree list list) (n: int) =
        match n with
        | 0 -> list
        | 1 -> [] :: list
        | _ -> [] :: (addEmpty list (n - 1))

    let rec generate (width: int) (height: int): string Tree =
        let rec generateInner (width: int) (height: int): string Tree list =
            match height with
            | 0 ->
                [ for _ in 0 .. width -> Node("A", []) ]
            | _ ->
                let chunks =
                    (generateInner width (height - 1)
                     |> Seq.chunkBySize 2
                     |> Seq.toList
                     |> List.map Array.toList)
                (addEmpty chunks (random 0 (width - chunks.Length))) |> List.map (fun item -> Node("A", item))
        generateInner width height
        |> List.rev
        |> List.head
