namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open Machine

open GuardedCommands.Frontend.AST

module CodeGeneration =


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
    let rec CompExpr varEnv funEnv =
        function
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

        | Apply(o, [ e1; e2 ]) when List.exists (fun x -> o = x) [ "+"; "*"; "="; "-" ] ->
            let ins =
                match o with
                | "+" -> [ ADD ]
                | "*" -> [ MUL ]
                | "=" -> [ EQ ]
                | "-" -> [ SUB ]
                | _ -> failwith "CE: this case is not possible"
            CompExpr varEnv funEnv e1 @ CompExpr varEnv funEnv e2 @ ins

        | _ -> failwith "CE: not supported yet"


    /// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
    and CompAccess varEnv funEnv =
        function
        | AVar x ->
            match Map.find x (fst varEnv) with
            | (GloVar addr, _) -> [ CSTI addr ]
            | (LocVar addr, _) -> failwith "CA: Local variables not supported yet"
        | AIndex(acc, e) -> failwith "CA: array indexing not supported yet"
        | ADeref e -> failwith "CA: pointer dereferencing not supported yet"


    (* Bind declared variable in env and generate code to allocate it: *)
    let allocate (kind: int -> Var) (typ, x) (varEnv: varEnv) =
        let (env, fdepth) = varEnv
        match typ with
        | ATyp(ATyp _, _) ->
            raise (Failure "allocate: array of arrays not permitted")
        | ATyp(t, Some i) -> failwith "allocate: array not supported yet"
        | _ ->
            let newEnv = (Map.add x (kind fdepth, typ) env, fdepth + 1)
            let code = [ INCSP 1 ]
            (newEnv, code)


    /// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment
    let rec CompStm varEnv funEnv =
        function
        | PrintLn e ->
            CompExpr varEnv funEnv e @ [ PRINTI
                                         INCSP -1 ]

        | Ass(acc, e) ->
            CompAccess varEnv funEnv acc @ CompExpr varEnv funEnv e @ [ STI
                                                                        INCSP -1 ]
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

        | _ -> failwith "CS: this statement is not supported yet"

        (*
        <lstart>
        <ltest1> <cond1> <JMPZERO ltest2>
        <stm1> <GOTO lstart>
        <ltest2> <cond2> <JMPZERO ltest3>
        <stm2> <GOTO lstart>
        ...
        <ltestn> <condn> <JMPZERO lend>
        <ln> <stmn> <GOTO lstart>
        <lend>
        *)

    and CompStms vEnv fEnv stms = List.collect (CompStm vEnv fEnv) stms



    (* ------------------------------------------------------------------- *)

    (* Build environments for global variables and functions *)

    let makeGlobalEnvs decs =
        let rec addv decs vEnv fEnv =
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
                    failwith "makeGlobalEnvs: function/procedure declarations not supported yet"
        addv decs (Map.empty, 0) Map.empty

    /// CP prog gives the code for a program prog
    let CP(P(decs, stms)) =
        let _ = resetLabels()
        let ((gvM, _) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs
        initCode @ CompStms gvEnv fEnv stms @ [ STOP ]
