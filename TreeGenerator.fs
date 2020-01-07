﻿namespace Project1

open System
open AndrewKennedyTree

module TreeGenerator =

    let rng = new Random()

    let Shuffle(org: _ []) =
        let arr = Array.copy org
        let max = (arr.Length - 1)

        let randomSwap (arr: _ []) i =
            let pos = rng.Next(max)
            let tmp = arr.[pos]
            arr.[pos] <- arr.[i]
            arr.[i] <- tmp
            arr

        [| 0 .. max |] |> Array.fold randomSwap arr

    let random (min: int) (max: int) =
        if min >= max then 0
        else Random().Next(min, max)

    let rec addEmpty (list: string Tree list list) (n: int) =
        match n with
        | 0 -> list
        | 1 -> [] :: list
        | _ -> [] :: (addEmpty list (n - 1))

    let rec generate (width: int) (height: int) (maxDegrees: int): string Tree =
        let rec generateInner (width: int) (height: int): string Tree list =
            match height with
            | 0 ->
                [ for _ in 0 .. width -> Node("A", []) ]
            | _ ->
                let chunks =
                    (generateInner width (height - 1)
                     |> Seq.chunkBySize maxDegrees
                     |> Seq.toList
                     |> List.map Array.toList)
                (addEmpty chunks (random 0 (width - chunks.Length)))
                |> List.toArray
                |> Shuffle
                |> Array.toList
                |> List.map (fun item -> Node("A", item))
        Node("Root", generateInner width height)