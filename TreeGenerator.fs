namespace Project1

open AndrewKennedyTree

module TreeGenerator =


    let random (min: int) (max: int) =
        let r = System.Random()
        r.Next(min, max)

    let rec generate (maxDegrees: int) (height: int) =
        match height with
        | 0 -> Node("A", [])
        | _ ->
            let r = random 1 maxDegrees
            Node
                ("A",
                 [ for _ in 0 .. r -> generate maxDegrees (height - 1) ])
