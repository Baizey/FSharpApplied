namespace Project1

open AndrewKennedyTree
open System.Text

module PostScript =

    let postScriptString (tree: 'a PosTree) =
        let output = StringBuilder("<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n")
        output.AppendLine("1 1 scale").AppendLine("700 999 translate").AppendLine("newpath")
              .AppendLine("/Times-Roman findfont 10 scalefont setfont") |> ignore


        let factor = 30.0
        let halfFactor = factor / 2.0
        let textSize = 10.0
        let halfTextSize = textSize / 2.0

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
