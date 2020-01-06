namespace Project1

open System.Text
open AndrewKennedyTree

module PostScript =
    // Change these to change size of prints
    let factor = 30.0
    let textSize = 10.0

    let postScriptString (tree: 'a PosTree) (extent: Extent) =
        let (f, t) =
            extent
            |> List.rev
            |> List.head

        let width = int (abs (t - f) * factor) + 30
        let height = extent.Length * int factor + 30
        let rootSpot = int ((float width / 2.0) - ((f + t) / 2.0) * factor)
        let halfFactor = factor / 2.0
        let halfTextSize = textSize / 2.0

        let size =
            sprintf "<</PageSize[%d %d]/ImagingBBox null>> setpagedevice\n" width height

        let output = StringBuilder(size)
        output.AppendLine("1 1 scale").AppendLine(sprintf "%d %d translate" rootSpot (height - 1)).AppendLine("newpath")
              .AppendLine(sprintf "/Times-Roman findfont %d scalefont setfont" (int textSize)) |> ignore

        let rec draw (PosNode((label, pos), children): 'a PosTree) (x: float) (y: float) (isRoot: bool): int =
            let rec drawInner (children: 'a PosTree list) (x: float) (y: float) =
                match children with
                | [] -> 0
                | PosNode((label, pos), children) :: tail ->
                    draw (PosNode((label, pos), children)) x y false |> ignore
                    drawInner tail x y
            match isRoot with
            | true ->
                output.AppendLine(sprintf "%d %d moveto" (int (x + pos * factor)) (int (y - factor)))
                      .AppendLine(sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show" (label.ToString()))
                |> ignore
            | false ->
                output.AppendLine(sprintf "%d %d moveto" (int x) (int (y - halfTextSize)))
                      .AppendLine(sprintf "%d %d lineto" (int x) (int (y - halfFactor)))
                      .AppendLine(sprintf "%d %d lineto" (int (x + pos * factor)) (int (y - halfFactor)))
                      .AppendLine(sprintf "%d %d lineto" (int (x + pos * factor)) (int (y - factor + textSize)))
                      .AppendLine(sprintf "%d %d moveto" (int (x + pos * factor)) (int (y - factor)))
                      .AppendLine(sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show" (label.ToString()))
                |> ignore
            match children with
            | [] -> 0
            | _ ->
                drawInner children (floor (x + pos * factor)) (floor (y - factor))
        draw tree 0.0 0.0 true |> ignore
        output.AppendLine("stroke").AppendLine("showpage").ToString()

//let postScriptFile =
