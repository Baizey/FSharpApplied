namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018

open System
open Machine

open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGenerationOptFuncs
open GuardedCommands.Util.CompilerSharedFuncs

module CodeGenerationOpt =
    // This string is used to add a known temporary string
    // to the function environment, so that the number of arguments
    // can be fetched in the Return statement.
    // Note: This must be a keyword, otherwise we risk it overwriting
    // another function
    let TMP_FUNCTION_STR = "function"

    type Var =
        | GloVar of int (* absolute address in stack           *)
        | LocVar of int (* address relative to bottom of frame *)

    (* The variable environment keeps track of global and local variables, and
    keeps track of next available offset for local variables *)
    type varEnv = Map<string, Var * Typ> * int

    (* The function environment maps function name to label and parameter decs *)
    type ParamDecs = (Typ * string) list
    type funEnv = Map<string, label * Typ option * ParamDecs>


    
    /// CompExpr e vEnv fEnv k gives the code for an expression e on the basis of a variable and a function environment and continuation k
    //let rec CompExpr e vEnv fEnv k =
    let rec CompExpr (vEnv: varEnv) (fEnv: funEnv) (exp: Exp) (k: instr list): instr list =
        match exp with
        | N n -> addCST n k
        | B b ->
            addCST
                (if b then 1
                 else 0) k
        | Access acc -> CompAccess vEnv fEnv acc (LDI :: k)

        | Apply("-", [ e ]) -> CompExpr vEnv fEnv e (addCST 0 (SWAP :: SUB :: k))

        | Apply("!", [ e ]) -> CompExpr vEnv fEnv e (addNOT k)
        
        | Apply("&&", [ b1; b2 ]) ->
            match k with
            | IFZERO lab :: _ -> CompExpr vEnv fEnv b1 (IFZERO lab :: CompExpr vEnv fEnv b2 k)
            | IFNZRO labthen :: k1 ->
                let (labelse, k2) = addLabel k1
                CompExpr vEnv fEnv b1 (IFZERO labelse :: CompExpr vEnv fEnv b2 (IFNZRO labthen :: k2))
            | _ ->
                let (jumpend, k1) = makeJump k
                let (labfalse, k2) = addLabel (addCST 0 k1)
                CompExpr vEnv fEnv b1 (IFZERO labfalse :: CompExpr vEnv fEnv b2 (addJump jumpend k2))
                
        | Apply("||", [ b1; b2 ]) ->
            match k with 
            | IFNZRO lab :: _ -> CompExpr vEnv fEnv b1 (IFNZRO lab :: CompExpr vEnv fEnv b2 k)
            | IFZERO labthen :: k1 ->
                let (labelse, k2) = addLabel k1
                CompExpr vEnv fEnv b1 (IFNZRO labelse :: CompExpr vEnv fEnv b2 (IFZERO labthen :: k2))
            | _ ->
                let (jumpend, k1) = makeJump k
                let (labfalse, k2) = addLabel (addCST 1 k1)
                CompExpr vEnv fEnv b1 (IFNZRO labfalse :: CompExpr vEnv fEnv b2 (addJump jumpend k2))
                
        | Apply(o, [ e1; e2 ]) when List.exists (fun x -> o = x) [ "+"; "*"; "="; "-"; "<"; "<>"; ">" ; "<="] ->
            let ins =
                match o with
                | "+" -> ADD :: k
                | "*" -> MUL :: k
                | "=" -> EQ :: k
                | "-" -> SUB :: k
                | "<" -> LT :: k
                | "<>" -> EQ :: addNOT k
                | ">" -> SWAP :: LT :: k
                | "<=" -> SWAP :: LT :: addNOT k
                | _ -> failwith "CompExpr: this case is not possible"
            CompExpr vEnv fEnv e1 (CompExpr vEnv fEnv e2 ins)
        | Func(f,es) -> let (flabel,_,_) = Map.find f fEnv
                        CompExprs vEnv fEnv es (makeCall (List.length es) flabel k)

        | Addr acc -> CompAccess vEnv fEnv acc k
        | _ -> failwith "CompExpr: not supported yet"

    and CompExprs vEnv fEnv es k =
        match es with
        | [] -> k
        | e :: es' -> CompExpr vEnv fEnv e (CompExprs  vEnv fEnv es' k)

    /// CA acc vEnv fEnv k gives the code for an access acc on the basis of a variable and a function environment and continuation k
    and CompAccess (varEnv: varEnv) (funEnv: funEnv) (access: Access) (k: instr list): instr list =
        match access with
        | AVar x -> match Map.find x (fst varEnv) with
            | (GloVar addr, ATyp(_,None)) -> addCST addr (GETBP :: ADD :: k)
            | (GloVar addr, _) -> addCST addr k
            | (LocVar offset, _) -> GETBP :: (addCST offset (ADD :: k))
        // TODO: optimize how we get environment in use?
        | AIndex(acc, e) -> match isLocal acc varEnv with
            | true -> CompAccess varEnv funEnv acc (LDI :: (CompExpr varEnv funEnv e (ADD :: GETBP :: ADD :: k)))
            | false -> CompAccess varEnv funEnv acc (LDI :: (CompExpr varEnv funEnv e (ADD :: k)))
        | ADeref e -> CompExpr varEnv funEnv e k
    and isLocal (arr:Access) (varEnv:varEnv): bool =
        match arr with
        | AIndex(a, _) -> isLocal a varEnv
        | AVar a -> match Map.find a (fst varEnv) with
            | (GloVar _, _) -> false
            | (LocVar _, _) -> true
        | _ -> failwith "CA: this was supposed to be an array or variable name"
        
    let rec allocate (kind: int -> Var) (typ, x) (varEnv: varEnv) (code:instr list) : varEnv * instr list =
        let (env, fdepth) = varEnv
        match typ with
        | ATyp(_, None) -> failwith "Array needs to be given a size"
        | ATyp(innerType, Some size) ->
            let sizes = List.rev (arraySizes (ATyp(innerType, Some size)))
            let totalAllocation = arrayAllocationSize sizes
            let codeWithArrayPointer = addCST (fdepth + totalAllocation - List.head sizes) code
            let finishedCode = addCode (List.rev (fst (allocateDeepArray fdepth sizes))) codeWithArrayPointer
            let newEnv = (Map.add x (kind (fdepth + totalAllocation), (ATyp(innerType, Some size))) env, fdepth + totalAllocation + 1)
            (newEnv, finishedCode)
        | _ ->
            let newEnv = (Map.add x (kind fdepth, typ) env, fdepth + 1)
            (newEnv, addINCSP 1 code)
    and arraySizes (typ:Typ) : int list =
        match typ with
        | ATyp(ATyp(a, b), Some size) -> size :: arraySizes (ATyp(a, b))
        | ATyp(_, Some size) -> [size]
        | _ -> failwith "This isn't an array!"
    and arrayAllocationSize (sizes:int list) : int = match sizes with
        | [x] -> x
        | x::xs -> x + x * (arrayAllocationSize xs)
        | _ -> failwith "This is not supposed to happen"
    and allocateDeepArray (depth:int) (sizes:int list) : instr list * int =
        match sizes with
        | [] -> failwith "deep array: Not supposed to happen"
        // Bottom layer is a normal array
        | [size] -> ([ INCSP size ], depth + size)
        // All other layers has to point to their sub-arrays and have a normal p at the end
        | size::s::sizes ->
            let (deepArrayCode, depths, depth) = (List.fold (fun (code, depths, curr) _ ->
                    let (tCode, depth) = allocateDeepArray curr (s::sizes)
                    (code @ tCode, (depth - s) :: depths, depth))
                    ([], [], depth)
                    (List.init size (fun _ -> 0)))
            let arrayCode = List.rev (List.map (fun item -> CSTI item) depths)
            (deepArrayCode @ arrayCode, depth + size)
    and addCode (newCode: instr list) (oldCode: instr list): instr list = match newCode with
    | [] -> oldCode
    | x::xs ->
        match x with
        | CSTI k -> addCode xs (addCST k oldCode)
        | INCSP k -> addCode xs (addINCSP k oldCode)
        | _ -> failwith "This is not handled yet"
            
   

    /// CS s vEnv fEnv k gives the code for a statement s on the basis of a variable and a function environment and continuation k
    //let rec CompStm stm vEnv fEnv k =
    let rec CompStm (vEnv: varEnv) (fEnv: funEnv) (stm: Stm) (k: instr list): instr list =
        match stm with
        | PrintLn e -> CompExpr vEnv fEnv e (PRINTI :: INCSP -1 :: k)
        | MulAss([],[]) -> k
        | MulAss(acc::accs, []) ->
            CompStm vEnv fEnv (MulAss(accs, [])) (CompAccess vEnv fEnv acc (SWAP :: STI :: addINCSP -1 k))
        | MulAss(accs, e::es) ->
                CompExpr vEnv fEnv e (CompStm vEnv fEnv (MulAss(accs, es)) k)
        | Ass(acc, e) -> CompAccess vEnv fEnv acc (CompExpr vEnv fEnv e (STI :: addINCSP -1 k))

        | Block([], stms) -> CompStms vEnv fEnv stms k
        | Block(decs, stms) -> 
            let varDescs = decsList decs
            let (vEnv1, code1) = List.fold (fun (currEnv, currCode) (typ, name) -> allocate LocVar (typ, name) currEnv currCode)
                                                                  (vEnv, [])
                                                                  varDescs
                                                                  
            let allocationSize = List.fold
                                    (fun acc var -> match var with
                                     | VarDec(ATyp(a, Some size), _) -> acc + 1 + arrayAllocationSize (List.rev (arraySizes (ATyp(a, Some(size)))))
                                     | _ -> acc + 1)
                                    0
                                    decs
            // Todo: See if we can't get rid of this @
            code1 @ CompStms vEnv1 fEnv stms (INCSP -allocationSize :: k)
        
            //failwith "CompStm: blocks with definitions not made yet"
        | Alt(GC(l)) -> 
            let (endlabel, k1) = addLabel k
            let k1 = STOP :: k1
            let rec command ((lst): (Exp * Stm list) list) (k: instr list): (label * instr list) =
                match lst with
                | [] -> addLabel k
                | (expr, stms) :: t ->
                    let (lstlabel, k1) = command t k
                    let k2 = addGOTO endlabel k1
                    let k3 = CompStms vEnv fEnv stms k2
                    let k4 = CompExpr vEnv fEnv expr (IFZERO lstlabel :: k3)
                    addLabel k4

            snd (command l k1)

        | Do(GC(l)) ->
            let startlabel = newLabel()
            let rec command ((lst): (Exp * Stm list) list) (k: instr list): (label * instr list) =
                match lst with
                | [] -> addLabel k
                | (expr, stms) :: t -> 
                    let (lstlabel, k1) = command t k
                    let k2 = addGOTO startlabel k1
                    let k3 = CompStms vEnv fEnv stms k2
                    let k4 = CompExpr vEnv fEnv expr (IFZERO lstlabel :: k3)
                    addLabel k4

            Label startlabel :: snd (command l k)

        | Return(rt) -> 
            let (_, _, list) = fEnv.Item TMP_FUNCTION_STR
            let inVar = List.length list
            let localVars = Map.fold (fun acc _ (var, typ) -> 
                                match (var, typ) with
                                | (LocVar(a), ATyp(b, Some(i))) when a >= inVar -> acc + 1 + arrayAllocationSize (List.rev (arraySizes (ATyp(b, Some(i)))))
                                | (LocVar(a), _) when a >= inVar -> acc + 1
                                | _ -> acc
                            ) 0 (fst vEnv)
            match rt with
            | Some(expr) -> CompExpr vEnv fEnv expr (RET (inVar + localVars) :: k)
            | None -> (RET (localVars + inVar - 1))::k

        | Call(f, es) ->
            let (flabel,_,_) = Map.find f fEnv
            CompExprs vEnv fEnv es (makeCall (List.length es) flabel (addINCSP (-1) k))


    and CompStms vEnv fEnv stms k =
        match stms with
        | [] -> k
        | stm :: stms' -> CompStm vEnv fEnv stm (CompStms  vEnv fEnv stms' k)

    (* ------------------------------------------------------------------- *)

    (* Build environments for global variables and functions *)

    let makeGlobalEnvs decs =
        let rec addv (decs: Dec list) (vEnv: varEnv) (fEnv: funEnv) =
            match decs with
            | [] -> (vEnv, fEnv, [])
            | dec :: decr ->
                match dec with
                | MulVarDec(typ, varList) ->
                    let (vEnv1, _, code1) = (List.fold (fun (vEnv, fEnv, code) name ->
                        let (vEnv2, fEnv2, code2) = addv [VarDec(typ, name)] vEnv fEnv
                        (vEnv2, fEnv2, code @ code2))
                        (vEnv, fEnv, []) varList)
                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                    (vEnv2, fEnv2, code1 @ code2)
                | VarDec(typ, var) ->
                    // TODO: dont @ code, give it instead of [] to optimize
                    let (vEnv1, code1) = allocate GloVar (typ, var) vEnv []
                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                    (vEnv2, fEnv2, code1 @ code2)
                | FunDec(tyOpt, f, xs, body) ->
                    let label =  newLabel()
                    let varDescs = decsList xs
                    let fEnv1 = Map.add f (label, tyOpt, varDescs) fEnv
                    let tempEnv = Map.add TMP_FUNCTION_STR ("", None, decsList xs) fEnv1

                    let arrTest typ a = match typ with
                                        | ATyp(_, _) -> GloVar a
                                        | _ -> LocVar a
                    let ((lv,_),a) = List.fold (fun ((env,b),a) (t,n) -> (((Map.add n (arrTest t a,t) env),b),a+1)) (vEnv,0) varDescs

                    let vEn = lv,a

                    let skipLabel = newLabel()
                    let procRet = match tyOpt with
                                    | Some(_) -> []
                                    | None -> [ RET (xs.Length - 1) ]


                    let code1 = GOTO skipLabel :: Label label :: CompStm vEn tempEnv body [] @ procRet @ [ Label skipLabel ]
                    let (vEnv2, fEnv2, code2) = addv decr vEnv fEnv1
                    (vEnv2, fEnv2, code1 @ code2)
        addv decs (Map.empty, 0) Map.empty

    (* Compile a complete micro-C program: globals, call to main, functions *)

    let CP(P(decs, stms)) =
        let _ = resetLabels()
        let ((gvM, _) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs
        //(initCode @ CompStms gvEnv fEnv stms [ STOP ])
        SecondPassOpt (initCode @ CompStms gvEnv fEnv stms [ STOP ])
