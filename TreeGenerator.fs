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
            Node
                ("A",
                 [ for _ in 1 .. maxDegrees -> generate maxDegrees (height - 1) ])
