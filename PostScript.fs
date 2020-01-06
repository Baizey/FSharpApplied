namespace Project1

open AndrewKennedyTree
open System.Text

module PostScript = 

  let postScriptString (tree: 'a PosTree) =
    // Todo: Normalize

    let output = new StringBuilder("<</PageSize[1400, 1000]/ImagingBBox null>> setpagedevice\n")
    output.AppendLine("1 1 scale")
          .AppendLine("700 999 translate")
          .AppendLine("newpath")
          .AppendLine("/Times-Romand findfont 10 scalefont setfont") |> ignore

    
    let draw (root: 'a PosTree) =
      let rec drawInner (PosNode((label, pos), children): 'a PosTree) (x:float) (y:float) =
        // Draw current node
        
        // Draw children
        for c in children do
          
        // Moveto current node again

        //match children with
        //| h :: hs -> drawInner h x y + 1
          
      drawInner tree 0.0 0.0

    0

  //let postScriptFile = 