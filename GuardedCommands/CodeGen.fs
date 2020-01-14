namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open Machine

open GuardedCommands.Frontend.AST

module CodeGeneration =
    // This string is used to add a known temporary string
    // to the function environment, so that the number of arguments
    // can be fetched in the Return statement.
    // Note: This must be a keyword, otherwise we risk it overwriting
    // another function
    let TMP_FUNCTION_STR = "function"

    (* A global variable has an absolute address, a local one has an offset: *)
    type Var =
        | GloVar of int (* absolute address in stack           *)
        | LocVar of int (* address relative to bottom of frame *)

    (* The variable environment keeps track of global and local variables, and
   keeps track of next available offset for local variables *)

    type varEnv = Map<string, Var * Typ> * int

    (* The function environment maps function name to label and parameter decs *)

    type ParamDecs = (Typ * string) list

    type funEnv = Map<string, label * Typ option * ParamDecs>

    /// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
    let rec CompExpr (varEnv: varEnv) (funEnv: funEnv) (exp: Exp): instr list =
        match exp with
        | N n -> [ CSTI n ]
        | B b ->
            [ CSTI
                (if b then 1
                 else 0) ]
        | Access acc -> CompAccess varEnv funEnv acc @ [ LDI ]

        | Apply("-", [ e ]) ->
            CompExpr varEnv funEnv e @ [CSTI 0
                                        SWAP
                                        SUB ]
        | Apply("!", [ e ]) ->
            CompExpr varEnv funEnv e @ [ NOT ]

        | Apply("&&", [ b1; b2 ]) ->
            let labend = newLabel()
            let labfalse = newLabel()
            CompExpr varEnv funEnv b1 @ [ IFZERO labfalse ] @ CompExpr varEnv funEnv b2 @ [ 
                                                                                          GOTO labend
                                                                                          Label labfalse
                                                                                          CSTI 0
                                                                                          Label labend ]

        | Apply("||", [ b1; b2 ]) ->
            let labend = newLabel()
            let labtrue = newLabel()
            CompExpr varEnv funEnv b1 @ [ IFNZRO labtrue ] @ CompExpr varEnv funEnv b2 @ [ 
                                                                                          GOTO labend
                                                                                          Label labtrue
                                                                                          CSTI 1
                                                                                          Label labend ]


        | Apply(o, [ e1; e2 ]) when List.exists (fun x -> o = x) [ "+"; "*"; "="; "-"; "<"; "<>"; ">" ; "<="] ->
            let ins =
                match o with
                | "+" -> [ ADD ]
                | "*" -> [ MUL ]
                | "=" -> [ EQ ]
                | "-" -> [ SUB ]
                | "<" -> [ LT ]
                | "<>" -> [ EQ ; NOT ]
                | ">" -> [ SWAP ; LT ]
                | "<=" -> [SWAP ; LT ; NOT]
                | _ -> failwith "CE: this case is not possible"
            CompExpr varEnv funEnv e1 @ CompExpr varEnv funEnv e2 @ ins
        | Func(f,es) -> let (flabel,_,_) = Map.find f funEnv
                        (List.fold (fun s v -> s @ CompExpr varEnv funEnv v) [] es) @
                        [CALL (List.length es, flabel)]
                        //failwith "function call not implemented yet"
        | Addr acc -> CompAccess varEnv funEnv acc
        | _ -> failwith "CE: not supported yet"


    /// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
    and CompAccess (varEnv: varEnv) (funEnv: funEnv) (access: Access): instr list =
        match access with
        | AVar x ->
            match Map.find x (fst varEnv) with
            | (GloVar addr, _) ->
                [ CSTI addr ]
            | (LocVar offset, _) ->
                [ GETBP; CSTI offset; ADD ]
        | AIndex(AIndex(a, b), e) ->
            failwith "CompAccess: array of arrays not supported"
        | AIndex(acc, e) ->
            let v = (CompAccess varEnv funEnv acc)
            let i = (CompExpr varEnv funEnv e)
            match acc with
            | AVar x ->
                match Map.find x (fst varEnv) with
                | (GloVar _, _) ->
                    v @ [LDI] @ i @ [ADD]
                | (LocVar _, _) ->
                    v @ [LDI] @ i @ [ADD; GETBP; ADD]
            | _ -> failwith "CA: this was supposed to be a variable name"
        | ADeref e -> (CompExpr varEnv funEnv e)
            // failwith "CA: pointer dereferencing not supported yet"


    (* Bind declared variable in env and generate code to allocate it: *)
    let rec allocate (kind: int -> Var) (typ, x) (varEnv: varEnv) =
        let (env, fdepth) = varEnv
        match typ with
        | ATyp(_, None) -> failwith "Array needs to be given a size"
        | ATyp((ATyp (a, b)), Some size) ->
            failwith "allocate: array of arrays not supported"
        | ATyp(innerType, Some size) ->
            let newEnv = (Map.add x (kind (fdepth + size), (ATyp(innerType, Some size))) env, fdepth + size + 1)
            let arrayRefCode = (CompAccess newEnv Map.empty (AVar(x))) @ [ CSTI fdepth; STI; INCSP -1 ]
            (newEnv, [INCSP (size + 1)] @ arrayRefCode)
        | _ ->
            let newEnv = (Map.add x (kind fdepth, typ) env, fdepth + 1)
            let code = [ INCSP 1 ]
            (newEnv, code)

    /// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment
    let rec CompStm (varEnv: varEnv) (funEnv: funEnv) (stm:Stm)=
        match stm with
        | PrintLn e ->
            CompExpr varEnv funEnv e @ [ PRINTI
                                         INCSP -1 ]
        | MulAss ([], []) -> []
        | MulAss(acc::accs, []) ->
            CompStm varEnv funEnv (MulAss(accs, []))
                @ CompAccess varEnv funEnv acc
                @ [SWAP; STI; INCSP -1]
        | MulAss(accs, e::es) ->
            CompExpr varEnv funEnv e @ CompStm varEnv funEnv (MulAss(accs, es))
        | Ass(acc, e) ->
            CompAccess varEnv funEnv acc @ CompExpr varEnv funEnv e @ [ STI; INCSP -1 ]
        | Alt(GC(l)) -> 
            let labels = List.map (fun _ -> newLabel()) l @ [newLabel(); newLabel()]
            let rec command ((lst): (Exp * Stm list) list) (num: int) =
                match lst with
                | [(expr, stms)] -> Label (labels.Item num) :: CompExpr varEnv funEnv expr 
                                        @ [IFZERO (labels.Item (num + 1))]
                                        @ CompStms varEnv funEnv stms 
                                        @ [GOTO (List.last labels)] 
                                        @ [Label (labels.Item (num + 1)); STOP; Label (List.last labels)] // Fail and end labels
                | (expr, stms) :: t -> Label (labels.Item num) :: CompExpr varEnv funEnv expr 
                                        @ [IFZERO (labels.Item (num + 1))] 
                                        @ CompStms varEnv funEnv stms 
                                        @ [GOTO (List.last labels)] 
                                        @ command t (num + 1)
                | _ -> failwith "Empty guarded command, should have been caught in typecheck"
                
            command l 0
            (* Instruction structure
            <l1> <cond1> <JMPZERO l2>
            <stm1> <GOTO lend>
            <l2> <cond2> <JMPNZERO l3>
            <stm2> <GOTO lend>
            ...
            <ln> <condn> <JMZERO lstop>
            <stmn> <GOTO lend>
            <lstop> STOP
            <lend>
            *)
        | Do(GC(l)) ->
            let labels = List.map (fun _ -> newLabel()) l @ [newLabel(); newLabel()]
            let rec command ((lst): (Exp * Stm list) list) (num: int) =
                match lst with
                | (expr, stms) :: t -> Label (labels.Item num) :: CompExpr varEnv funEnv expr 
                                        @ [IFZERO (labels.Item (num + 1))] 
                                        @ CompStms varEnv funEnv stms 
                                        @ [GOTO labels.Head] 
                                        @ command t (num + 1)
                | [] -> [Label (List.last labels)]
            
            Label labels.Head :: command l 1
            (* Instruction structure
            <lstart>
            // <ltest1> not necessary here, but kept because it ensures that we don't 
            // have to make a special case for it in the code, and it doesn't change 
            // the behavior of the code at all
            <ltest1> <cond1> <JMPZERO ltest2> 
            <stm1> <GOTO lstart>
            <ltest2> <cond2> <JMPZERO ltest3>
            <stm2> <GOTO lstart>
            ...
            <ltestn> <condn> <JMPZERO lend>
            <ln> <stmn> <GOTO lstart>
            <lend>
            *)

        | Block([], stms) -> CompStms varEnv funEnv stms
        | Block(decs, stms) ->  // Allocate stack space of length lng
                                let rec decsList decs = //This should be a global function is used way to many times
                                    match decs with
                                    | [] -> []
                                    | (VarDec(t,n))::rest -> (t,n)::decsList rest
                                    | MulVarDec(typ, varList)::rest ->
                                        List.map (fun x -> (typ, x)) varList @ decsList rest
                                    | _ -> failwith "Functions cant declare functions"

                                let varDescs = decsList decs
                                let (vEnv1, code1) = List.fold (fun (env,l) (t,n) -> let (a,b) = allocate LocVar (t, n) env
                                                                                     (a,l@b)) (varEnv,[]) varDescs
                                // <sl>
                                let code2 = CompStms vEnv1 funEnv stms
                                // INCSP -lng
                                code1 @ code2 @ [INCSP (-1*List.length decs)]
                                //failwith "Block not implemented for dec list"
        // Function return
        | Return(Some(expr)) -> 
            let (_, _, list) = funEnv.Item TMP_FUNCTION_STR

            let localVars = Map.fold (fun acc _ (var, typ) -> 
                                match (var, typ) with
                                | (LocVar(_), ATyp(_, Some(i))) -> acc + i + 1
                                | (LocVar(_), _) -> acc + 1
                                | _ -> acc
                            ) 0 (fst varEnv)
            //let localVars = Map.filter (fun _ (var, _) -> match var with 
            //                                                | LocVar(_) -> true
            //                                                | GloVar(_) -> false
            //                    ) (fst varEnv) |> Map.count
            CompExpr varEnv funEnv expr @ [ RET localVars ]

        | _ -> failwith "CS: this statement is not supported yet"

    and CompStms (vEnv: varEnv) (fEnv: funEnv) stms = List.collect (CompStm vEnv fEnv) stms

    (* ------------------------------------------------------------------- *)

    (* Build environments for global variables and functions *)

    let makeGlobalEnvs decs =
        let rec addv (decs: Dec list) (vEnv: varEnv) (fEnv: funEnv) =
            match decs with
            | [] -> (vEnv, fEnv, [])
            | dec :: decr ->
                match dec with
                | MulVarDec(typ, varList) ->
                    List.fold (fun (vEnv, fEnv, code) name ->
                        let (vEnv2, fEnv2, code2) = addv [VarDec(typ, name)] vEnv fEnv
                        (vEnv2, fEnv2, code @ code2))
                        (vEnv, fEnv, []) varList
                | VarDec(typ, var) ->
                    let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                    (vEnv2, fEnv2, code1 @ code2)
                | FunDec(tyOpt, f, xs, body) ->
                    let rec decsList decs = 
                        match decs with
                        | [] -> []
                        | (VarDec(t,n))::rest -> (t,n)::decsList rest
                        | _ -> failwith "Functions cant take functions as input"
                    let label =  newLabel()
                    let varDescs = decsList xs
                    let fEnv1 = Map.add f (label, tyOpt, varDescs) fEnv
                    let tempEnv = Map.add TMP_FUNCTION_STR ("", None, decsList xs) fEnv1

                    let ((lv,i),a) = List.fold (fun ((env,b),a) (t,n) -> (((Map.add n (LocVar(a),t) env),b),a+1)) (vEnv,0) varDescs

                    let vEn = lv,a

                    let skipLabel = newLabel()
                    let code1 = GOTO skipLabel :: Label label :: CompStm vEn tempEnv body @ [ Label skipLabel ]
                    let (vEnv2, fEnv2, code2) = addv decr vEnv fEnv1
                    (vEnv2, fEnv2, code1 @ code2)
        addv decs (Map.empty, 0) Map.empty

    /// CP prog gives the code for a program prog
    let CP(P(decs, stms)) =
        let _ = resetLabels()
        let ((gvM, _) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs
        let tmp = initCode @ CompStms gvEnv fEnv stms @ [ STOP ]
        tmp

(*
code // Allocate stack space of length lng
<sl>
INCSP -lng
*)

(*
INCSP 1 // Declare x

GOTO "progStart" //Needs to jump over declaration of f

Label "f" // Declare f

GETBP //
CSTI 0 // Address for y
ADD 
LDI // Load y
PRINTI // Print y
INCSP -1
GETBP //
CSTI 0 // Address for y
ADD 
LDI // Load y
CSTI 1 // Push 1 to stack
ADD // Add y and 1
RET 1 // Return y

Label "progStart"

CSTI 0 // Address of x
CSTI 2 // Parameter for function call
CALL "f" 1 //Function call

STI // Store value in x address
INCSP -1
CSTI 0 // x address
LDI // Load x value
PRINTI //Print x value 
INCSP -1

*)