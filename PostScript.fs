namespace Project1

open System.IO
open System.Text
open AndrewKennedyTree

module PostScript =
    // Change these to change size of prints
    //let widthFactor = 30.0
    let heightFactor = 30.0
    let textSize = 10.0

    let rec range (mi: float) (ma: float) (ex: Extent): float * float =
        match ex with
        | [] -> (mi, ma)
        | (l, r) :: tail ->
            let a = (min l mi)
            let b = (max r ma)
            range a b tail

    let postScript (tree: 'a PosTree) (extent: Extent) (strFunc: string -> unit): unit =

        let rec findLongestLabel (tree: 'a PosTree) =
            let rec findLongestLabelInChildren (trees: 'a PosTree list) =
                match trees with
                | [] -> 0
                | [ h ] -> findLongestLabel h
                | h :: t -> max (findLongestLabel h) (findLongestLabelInChildren t)
            match tree with
            | PosNode((label, _), []) ->
                label.ToString().Length
            | PosNode((label, _), children) ->
                max (label.ToString().Length) (findLongestLabelInChildren children)

        let (f, t) = range 0.0 0.0 extent
        (*
            extent
            |> List.rev
            |> List.head
            *)
        let widthFactor = float (findLongestLabel tree) * 7.0
        let width = int (abs (t - f) * widthFactor) + int widthFactor
        let height = extent.Length * int heightFactor + int heightFactor
        let rootSpot = int ((float width / 2.0) - ((f + t) / 2.0) * widthFactor)
        let halfTextSize = textSize / 2.0

        strFunc (sprintf "<</PageSize[%d %d]/ImagingBBox null>> setpagedevice\n" width height)
        strFunc
            ("1 1 scale\n" + sprintf "%d %d translate\n" rootSpot (height - 1)
             + "newpath\n")
        strFunc (sprintf "/Times-Roman findfont %d scalefont setfont\n" (int textSize))

        let rec draw (PosNode((label, pos), children): 'a PosTree) (x: float) (y: float) (isRoot: bool): unit =
            let rec drawInner (children: 'a PosTree list) (x: float) (y: float): unit =
                match children with
                | PosNode((label, pos), children) :: tail ->
                    draw (PosNode((label, pos), children)) x y false
                    drawInner tail x y
                | _ -> ()
            match isRoot with
            | true ->
                strFunc (sprintf "%d %d moveto\n" (int (x + pos * widthFactor)) (int (y - heightFactor)))
                strFunc (sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (label.ToString()))
            | false ->
                strFunc (sprintf "%d %d moveto\n" (int x) (int (y - halfTextSize)))
                strFunc (sprintf "%d %d lineto\n" (int x) (int (y - (heightFactor / 2.0))))
                strFunc (sprintf "%d %d lineto\n" (int (x + pos * widthFactor)) (int (y - (heightFactor / 2.0))))
                strFunc (sprintf "%d %d lineto\n" (int (x + pos * widthFactor)) (int (y - heightFactor + textSize)))
                strFunc (sprintf "%d %d moveto\n" (int (x + pos * widthFactor)) (int (y - heightFactor)))
                strFunc (sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (label.ToString()))
            if children.Length > 0 then
                drawInner children (floor (x + pos * widthFactor)) (floor (y - heightFactor))
                
        draw tree 0.0 0.0 true
        strFunc "stroke\nshowpage"

    let postScriptStringBuilder (tree: 'a PosTree) (extent: Extent): string =
        let strBuilder = StringBuilder()
        let strFunc (str: string) = strBuilder.Append(str) |> ignore
        postScript tree extent strFunc
        strBuilder.ToString()

    let postScriptStringPlus (tree: 'a PosTree) (extent: Extent) =
        let mutable output = ""
        let strFunc (str: string) = output <- output + str
        postScript tree extent strFunc
        output

    let postScriptStringConcat (tree: 'a PosTree) (extent: Extent) =
        let mutable strList: string list = List.empty
        let strFunc (str: string) = strList <- str :: strList
        postScript tree extent strFunc
        String.concat "" (List.rev strList)


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

    let postScriptSaveResultString (psString: string) (filepath: string) =
        let abspath = Path.Combine(__SOURCE_DIRECTORY__, filepath)
        File.WriteAllText(abspath + ".ps", psString)

    let postScriptSaveResult (tree: 'a PosTree) (extent: Extent) (filepath: string) =
        let result = postScriptStringConcat tree extent
        postScriptSaveResultString result filepath
