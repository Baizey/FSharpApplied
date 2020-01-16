namespace GuardedCommands.Util

open GuardedCommands.Frontend.AST

module CompilerSharedFuncs =
    let rec decsNames (decs: Dec list): string list =
        match decs with
        | [] -> []
        | (VarDec(_, n)) :: rest -> n :: decsNames rest
        | (MulVarDec(typ,varList)::rest) -> varList@decsNames rest
        | _ -> failwith "Functions cant take functions as input"

    let rec tcUniqueName decs =
        match decs with
        | [] -> ()
        | d::rest -> if List.contains d rest then failwith "Multiple variable have the same name" else tcUniqueName rest

    let rec decsTypes decs = 
        match decs with
        | [] -> []
        | (VarDec(t,_))::rest -> t::decsTypes rest
        | (MulVarDec(t,varList)::rest) -> List.map(fun i -> t) varList @ decsTypes rest
        | _ -> failwith "Functions cant take functions as input"

    let rec decsList decs = 
        match decs with
        | [] -> []
        | (VarDec(t, n)) :: rest -> (t, n) :: decsList rest
        | MulVarDec(typ, varList) :: rest -> List.map (fun x -> (typ, x)) varList @ decsList rest
        | _ -> failwith "Functions cant declare functions"