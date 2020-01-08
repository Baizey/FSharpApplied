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
            if children.Length > 0 then drawInner children (floor (x + pos * widthFactor)) (floor (y - heightFactor))

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
        
    let postScriptSaveResultString (psString: string) (filepath: string) =
        
        let postscriptPath = Path.Combine(__SOURCE_DIRECTORY__, "postscript")
        Directory.CreateDirectory(postscriptPath) |> ignore
        let abspath = Path.Combine(postscriptPath, filepath)
        File.WriteAllText(abspath + ".ps", psString)

    let postScriptSaveResult (tree: 'a PosTree) (extent: Extent) (filepath: string) =
        let result = postScriptStringConcat tree extent
        postScriptSaveResultString result filepath
