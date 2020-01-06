namespace Lasse

module Exercise_1 =

    let rec f n =
        if n = 0 then 0
        else n + f (n - 1)

    let rec sum (m, n) =
        if n = 0 then m
        else m + n + sum (m, n - 1)

    let rec bin (n, k) =
        if k = 0 || k = n then 1
        else bin (n - 1, k - 1) + bin (n - 1, k)

    let multiplicity (x, ys) =
        ys
        |> List.filter (fun y -> y = x)
        |> List.length

    let mulC (x: int, ys: int list) = ys |> List.map (fun y -> y * x)

    let rec mulX (xs: int list, x: int) =
        if x <= 0 then xs
        else mulX (0 :: xs, x - 1)

    let rec addE (xs: int list, ys: int list) =
        if xs.Length = ys.Length then List.zip xs ys |> List.map (fun (a, b) -> a + b)
        else if xs.Length > ys.Length then addE (xs, mulX (ys |> List.rev, xs.Length - ys.Length) |> List.rev)
        else addE (mulX (xs |> List.rev, ys.Length - xs.Length) |> List.rev, ys)

    let rec mul (xs: int list, ys: int list) =
        let rec mulInner (xs: int list, acc: int list, depth: int) =
            match xs with
            | [] -> acc
            | head :: tail -> mulInner (tail, addE (mulC (head, mulX (ys, depth)), acc), depth + 1)
        mulInner (xs, [], 0)

    let rec display (xs: int list) =
        let rec displayInner (xs: int list, acc: string list, depth: int) =
            match (xs, depth) with
            | ([], _) -> acc
            | (0 :: tail, _) ->
                displayInner (tail, acc, depth + 1)
            | (head :: tail, 0) ->
                displayInner (tail, head.ToString() :: acc, depth + 1)
            | (head :: tail, 1) ->
                displayInner (tail, head.ToString() + "x" :: acc, depth + 1)
            | (head :: tail, _) ->
                displayInner (tail, (head.ToString() + "x^" + depth.ToString()) :: acc, depth + 1)
        displayInner (xs, [], 0)
        |> List.rev
        |> String.concat " + "
