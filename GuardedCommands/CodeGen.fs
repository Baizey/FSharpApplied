﻿namespace GuardedCommands.Backend
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

    type varEnv = Map<string, Var * Type> * int

    (* The function environment maps function name to label and parameter decs *)

    type ParamDecs = (Type * string) list

    type funEnv = Map<string, label * Type option * ParamDecs>

    /// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
    let rec CompEnv varEnv funEnv =
        function
        | N n -> [ CSTI n ]
        | B b ->
            [ CSTI
                (if b then 1
                 else 0) ]
        | Access acc -> CompAccess varEnv funEnv acc @ [ LDI ]

        | Apply("-", [ e ]) ->
            CompEnv varEnv funEnv e @ [ CSTI 0
                                        SWAP
                                        SUB ]

        | Apply("&&", [ b1; b2 ]) ->
            let labend = newLabel()
            let labfalse = newLabel()
            CompEnv varEnv funEnv b1 @ [ IFZERO labfalse ] @ CompEnv varEnv funEnv b2 @ [ GOTO labend
                                                                                          Label labfalse
                                                                                          CSTI 0
                                                                                          Label labend ]

        | Apply(o, [ e1; e2 ]) when List.exists (fun x -> o = x) [ "+"; "*"; "=" ] ->
            let ins =
                match o with
                | "+" -> [ ADD ]
                | "*" -> [ MUL ]
                | "=" -> [ EQ ]
                | _ -> failwith "CE: this case is not possible"
            CompEnv varEnv funEnv e1 @ CompEnv varEnv funEnv e2 @ ins

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
        | AType(AType _, _) ->
            raise (Failure "allocate: array of arrays not permitted")
        | AType(t, Some i) -> failwith "allocate: array not supported yet"
        | _ ->
            let newEnv = (Map.add x (kind fdepth, typ) env, fdepth + 1)
            let code = [ INCSP 1 ]
            (newEnv, code)


    /// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment
    let rec CS varEnv funEnv =
        function
        | PrintLn e ->
            CompEnv varEnv funEnv e @ [ PRINTI
                                        INCSP -1 ]

        | Ass(acc, e) ->
            CompAccess varEnv funEnv acc @ CompEnv varEnv funEnv e @ [ STI
                                                                       INCSP -1 ]

        | Block([], stms) -> CSs varEnv funEnv stms

        | _ -> failwith "CS: this statement is not supported yet"

    and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms



    (* ------------------------------------------------------------------- *)

    (* Build environments for global variables and functions *)

    let makeGlobalEnvs decs =
        let rec addv decs vEnv fEnv =
            match decs with
            | [] -> (vEnv, fEnv, [])
            | dec :: decr ->
                match dec with
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
        initCode @ CSs gvEnv fEnv stms @ [ STOP ]
