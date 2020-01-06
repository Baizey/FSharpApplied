namespace Lasse

module Driver =
    [<EntryPoint>]
    let main argv =
        let xs = [1;2;3]
        let ys = [1;0;1;1;2]
        let str = Exercise_1.display(Exercise_1.mul(xs, ys))
        printfn "%s" str
        0
