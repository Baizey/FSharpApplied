namespace Project1

open System.IO
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

    let postScriptStringPlus (tree: 'a PosTree) (extent: Extent) =
        let (f, t) =
            extent
            |> List.rev
            |> List.head
        let width = int (abs (t - f) * factor) + 30
        let height = extent.Length * int factor + 30
        let rootSpot = int ((float width / 2.0) - ((f + t) / 2.0) * factor)
        let halfFactor = factor / 2.0
        let halfTextSize = textSize / 2.0

        let output = sprintf "<</PageSize[%d %d]/ImagingBBox null>> setpagedevice\n" width height +
                     sprintf "1 1 scale\n" +
                     sprintf "%d %d translate\n" rootSpot (height - 1) +
                     sprintf "newpath\n" +
                     sprintf "/Times-Roman findfont %d scalefont setfont\n" (int textSize)

        let rec draw (PosNode((label, pos), children): 'a PosTree) (x: float) (y: float) (isRoot: bool): string =
            let rec drawInner (children: 'a PosTree list) (x: float) (y: float) =
                match children with
                | [] -> ""
                | PosNode((label, pos), children) :: tail ->
                    draw (PosNode((label, pos), children)) x y false + drawInner tail x y
            let t = match isRoot with
                    | true ->   sprintf "%d %d moveto\n" (int (x + pos * factor)) (int (y - factor)) +
                                sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (label.ToString())

                    | false ->  sprintf "%d %d moveto\n" (int x) (int (y - halfTextSize)) +
                                sprintf "%d %d lineto\n" (int x) (int (y - halfFactor)) +
                                sprintf "%d %d lineto\n" (int (x + pos * factor)) (int (y - halfFactor)) +
                                sprintf "%d %d lineto\n" (int (x + pos * factor)) (int (y - factor + textSize)) +
                                sprintf "%d %d moveto\n" (int (x + pos * factor)) (int (y - factor)) +
                                sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (label.ToString())

            match children with
            | [] -> t
            | _  -> t + drawInner children (floor (x + pos * factor)) (floor (y - factor))
        let stringTree = draw tree 0.0 0.0 true
        output + stringTree + "stroke\nshowpage"

    let postScriptStringConcat (tree: 'a PosTree) (extent: Extent) =
        let (f, t) =
            extent
            |> List.rev
            |> List.head
        let width = int (abs (t - f) * factor) + 30
        let height = extent.Length * int factor + 30
        let rootSpot = int ((float width / 2.0) - ((f + t) / 2.0) * factor)
        let halfFactor = factor / 2.0
        let halfTextSize = textSize / 2.0
        
        let output = seq{yield sprintf "<</PageSize[%d %d]/ImagingBBox null>> setpagedevice\n" width height
                         yield "1 1 scale\n"
                         yield sprintf "%d %d translate\n" rootSpot (height - 1)
                         yield "newpath\n"
                         yield sprintf "/Times-Roman findfont %d scalefont setfont\n" (int textSize)
                        }
              
        let rec draw (PosNode((label, pos), children): 'a PosTree) (x: float) (y: float) (isRoot: bool): seq<string> =
            let rec drawInner (children: 'a PosTree list) (x: float) (y: float) =
                match children with
                | [] -> Seq.empty
                | PosNode((label, pos), children) :: tail ->
                    seq
                        { yield! draw (PosNode((label, pos), children)) x y false
                          yield! drawInner tail x y
                        }
            let t = match isRoot with
                    | true ->   seq{yield sprintf "%d %d moveto\n" (int (x + pos * factor)) (int (y - factor))
                                    yield sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (label.ToString())}

                    | false ->  seq{yield sprintf "%d %d moveto\n" (int x) (int (y - halfTextSize))
                                    yield sprintf "%d %d lineto\n" (int x) (int (y - halfFactor))
                                    yield sprintf "%d %d lineto\n" (int (x + pos * factor)) (int (y - halfFactor))
                                    yield sprintf "%d %d lineto\n" (int (x + pos * factor)) (int (y - factor + textSize))
                                    yield sprintf "%d %d moveto\n" (int (x + pos * factor)) (int (y - factor))
                                    yield sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (label.ToString())}

            match children with
            | [] -> t
            | _  -> seq{ yield! t 
                         yield! drawInner children (floor (x + pos * factor)) (floor (y - factor))
                       }
        let stringTree = seq{yield! output
                             yield! draw tree 0.0 0.0 true
                             yield "stroke\nshowpage"
                            }
        String.concat "" stringTree


(*
    let postScriptStringConcat (tree: 'a PosTree) =
        let intro = "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n" + "1 1 scale\n" + "700 999 translate\n" + "newpath\n" + "/Times-Roman findfont 10 scalefont setfont\n"
        // TODO : Make more dynamic
        let moveTo x y = string (int (x*8.0)) + " " + string (int (y * 10.0)) + " moveto\n"
        let lineTo x y = string (int (x*8.0)) + " " + string (int (y * 10.0)) + " lineto\n"
        let name label = " (" + label.ToString() + ") dup stringwidth pop 2 div neg 0 rmoveto show\n"
        let getPos (PosNode((_,pos),_)) = pos

        let rec drawList l x y =
            match l with
            | [] -> ""
            | e::rest -> drawElement e x y + (drawList rest x y)
        and drawElement node x y = 
            match node with
            | PosNode((label,pos),[]) -> (moveTo (x+pos) (y-1.0)) + (name label) + (moveTo (x+pos) (y-2.0))
            | PosNode((label,pos),children) -> (moveTo (x+pos) (y-1.0)) + (name label) + (moveTo (x+pos) (y-1.3)) + (lineTo (x + pos) (y-2.3)) + (drawBranch children (x+pos) (y-2.3)) + (drawList children (x+pos) (y-3.3))
        and drawBranch l x y =
            match l with
            | [] -> ""
            | h::t -> let posBeg = getPos h
                      let posEnd = getPos (List.last t)
                      moveTo (x+posBeg) (y) + lineTo (x+posEnd) (y) + drawBranchDown l x y
        and drawBranchDown l x y =
            match l with
            | [] -> ""
            | h::t -> let pos = getPos h
                      moveTo (x+pos) (y) + lineTo (x+pos) (y-1.0) + drawBranchDown t x y

        intro + drawElement tree 0.0 (-1.0) + "stroke\nshowpage"
*)

    let postScriptSaveResult (tree: 'a PosTree) (extent: Extent) (filepath: string) =
        let result = postScriptStringPlus tree extent
        let abspath = Path.Combine(__SOURCE_DIRECTORY__,filepath)
        File.WriteAllText(abspath + ".ps", result)

