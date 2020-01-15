namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016 , 04-01-2018

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck =
    type Env = Map<string, Typ>

    /// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
    /// for global and local variables
    let rec tcExpr (gtenv: Env) (ltenv: Env) (exp: Exp): Typ =
        match exp with
        | N _ -> ITyp
        | B _ -> BTyp
        | Addr e -> PTyp (tcA gtenv ltenv e)
        | Access acc -> tcA gtenv ltenv acc

        | Apply(f, [ e ]) when List.exists (fun x -> x = f) [ "-"; "!" ] -> tcMonadic gtenv ltenv f e

        | Apply(f, [ e1; e2 ]) when List.exists (fun x -> x = f) [ "+"; "-"; "*"; "="; "&&"; "<"; "<>"; "<="; ">"; "||" ] ->
            tcDyadic gtenv ltenv f e1 e2
        | Func(f, es) -> tcNaryFunction gtenv ltenv f es

        | _ -> failwith "tcE: not supported yet"

    and tcMonadic (gtenv: Env) (ltenv: Env) (f: string) (e: Exp): Typ =
        match (f, tcExpr gtenv ltenv e) with
        | ("-", ITyp) -> ITyp
        | ("!", BTyp) -> BTyp
        | _ -> failwith "illegal/illtyped monadic expression"

    and tcDyadic (gtenv: Env) (ltenv: Env) (f: string) (e1: Exp) (e2: Exp): Typ =
        match (f, tcExpr gtenv ltenv e1, tcExpr gtenv ltenv e2) with
        | (o, ITyp, ITyp) when List.exists (fun x -> x = o) [ "+"; "*"; "-" ] -> ITyp
        | (o, ITyp, ITyp) when List.exists (fun x -> x = o) [ "="; "<" ; "<>" ; "<=" ; ">"] -> BTyp
        | (o, BTyp, BTyp) when List.exists (fun x -> x = o) [ "&&"; "="; "<>"; "||" ] -> BTyp
        | _ -> failwith ("illegal/illtyped dyadic expression: " + f)

    and tcNaryArgsChecker (gtenv: Env) (ltenv: Env) (ts: Typ list) (es: Exp list): bool =
        if es.Length <> ts.Length then 
            failwith "number of given arguments does not match the number of arguments the function/procedure defines"
        if List.forall (fun (e1, e2) -> tcEquality e1 e2) (List.zip (List.map (fun x -> tcExpr gtenv ltenv x) es) ts) then 
            true
        else 
            false

    and tcNaryFunction (gtenv: Env) (ltenv: Env) (f: string) (es: Exp list): Typ = 
        let func = 
            match Map.tryFind f gtenv with
            | None -> match Map.tryFind f ltenv with
                      | None -> failwith "Function does not exists"
                      | Some(a) -> a
            | Some(a) -> a

        match func with
            | FTyp(ts, Some(t)) ->
                if tcNaryArgsChecker gtenv ltenv ts es then t
                else failwith "function call types are mismatched"
            | _ -> failwith "Function either has no return type, or structure is wrong"

    and tcEquality (typ1: Typ) (typ2: Typ): bool = 
        match (typ1, typ2) with
        | (PTyp t1), (PTyp t2) -> tcEquality t1 t2
        | (ATyp(t1, _), ATyp(t2, _)) -> tcEquality t1 t2
        | (t1, t2) -> t1 = t2

    and tcNaryProcedure (gtenv: Env) (ltenv: Env) (f: string) (es: Exp list): unit =
        let proc = 
            match Map.tryFind f gtenv with
            | None -> match Map.tryFind f ltenv with
                      | None -> failwith "Procedure does not exists"
                      | Some(a) -> a
            | Some(a) -> a

        match proc with 
        | FTyp(ts, None) -> 
            if tcNaryArgsChecker gtenv ltenv ts es then ()
            else failwith "procedure call types are mismatched"
        | _ -> failwith "Procedure either has return type, or structure is wrong"
        
        //failwith "type check: procedures not supported yet"


    /// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
    /// for global and local variables
    and tcA (gtenv: Env) (ltenv: Env) (acc: Access): Typ =
        match acc with
        | AVar x ->
            match Map.tryFind x ltenv with
            | None ->
                match Map.tryFind x gtenv with
                | None -> failwith ("no declaration for : " + x)
                | Some t -> t
            | Some t -> t
        | AIndex(AIndex(a, b), i) ->
            let typ = tcA gtenv ltenv (AIndex(a, b))
            let b = tcExpr gtenv ltenv i
            match b with
            | ITyp _ ->
                match typ with
                | ATyp(a, _) ->
                    a
                | _ -> failwith "tcA: this is not an array"
            | _ -> failwith "tcA: array index needs to be int"
        | AIndex(acc, i) ->
            let res = tcA gtenv ltenv acc
            match res with
            | ATyp(a, _) ->
                let b = tcExpr gtenv ltenv i
                match b with
                | ITyp _ -> a
                | _ -> failwith "tcA: array index needs to be int"
            | _ -> failwith "Wait, this isn't an array!"
        | ADeref e -> match tcExpr gtenv ltenv e with
            | PTyp a -> a
            | _ -> failwith "Attempted to dereference something that wasn't a pointer"


    /// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
    /// for global and local variables and the possible type of return expressions
    and tcStm (gtenv: Env) (ltenv: Env) (stm: Stm): unit =
        match stm with
        | PrintLn e -> ignore (tcExpr gtenv ltenv e)
        | MulAss([], []) ->
            ()
        | MulAss(_, []) ->
            failwith "illtyped multi assignment, missing right side expressions"
        | MulAss([], _) ->
            failwith "illtyped multi assignment, missing left side variables"
        | MulAss(acc::accs, e::es) ->
            if tcA gtenv ltenv acc = tcExpr gtenv ltenv e then tcStm gtenv ltenv (MulAss(accs, es))
            else failwith "illtyped multi assignment"
        | Ass(acc, e) ->
            let a =  tcA gtenv ltenv acc
            let b = tcExpr gtenv ltenv e
            if tcEquality a b then ()
            else failwith "illtyped assignment"
        | Return(Some(a)) ->
                        let t = tcExpr gtenv ltenv a
                        let rt = Map.find "function" ltenv
                        if t = rt then () else failwith "Return type wrong"
        | Do(GC(l))
        | Alt(GC(l)) -> match (List.forall (fun (e,_) -> (tcExpr gtenv ltenv e = BTyp)) l) with
                                         | true -> List.iter (fun (_,stms) -> List.iter (tcStm gtenv ltenv) stms) l
                                         | false -> failwith "Guard is not of type boolean"
        | Block([], stms) -> List.iter (tcStm gtenv ltenv) stms
        | Block(decs, stms) -> let innerLtenv = tcGDecs gtenv decs
                               let locEnv = Map.fold (fun acc str typ -> Map.add str typ acc) ltenv innerLtenv
                               List.iter (tcStm gtenv locEnv) stms
        | Call(f, es) -> tcNaryProcedure gtenv ltenv f es
        | _ -> failwith "tcS: this statement is not supported yet"

    and tcGDec (gtenv: Env) (dec: Dec): Env =
        match dec with
        | VarDec(typ, str) -> Map.add str typ gtenv
        | MulVarDec(typ, strList) ->
            List.fold (fun state str -> tcGDec state (VarDec(typ, str))) gtenv strList
        | FunDec(topt, f, decs, stm) -> 
            match topt with
            | Some(rtyp) -> tcFDecs (decsNames decs) //Check all input arguements are unique
                            let ftyp = FTyp(decsTypes decs, Some(rtyp))
                            let ltenv = Map.add f ftyp (Map.add "function" rtyp (tcGDecs Map.empty decs))
                            if tcReturnStm stm then () else failwith "Function has no return statement"
                            tcStm gtenv ltenv stm //Check stm is wellformed and return types correct.
                            Map.add f ftyp gtenv //Add function to enviroment
            | None -> tcFDecs (decsNames decs)
                      let ftyp = FTyp(decsTypes decs, None)
                      let ltenv = Map.add f ftyp (tcGDecs Map.empty decs)
                      if tcReturnStm stm then failwith "Procedure contains return statement" else ()
                      tcStm gtenv ltenv stm
                      Map.add f ftyp gtenv
    and tcGDecs (gtenv: Env) (decs: Dec list): Env =
        match decs with
        | dec :: decs -> tcGDecs (tcGDec gtenv dec) decs
        | _ -> gtenv
    and decsNames decs = 
        match decs with
        | [] -> []
        | (VarDec(_,n))::rest -> n::decsNames rest
        | _ -> failwith "Functions cant take functions as input"
    and decsTypes decs = 
        match decs with
        | [] -> []
        | (VarDec(t,_))::rest -> t::decsTypes rest
        | _ -> failwith "Functions cant take functions as input"
    and tcFDecs decs =
        match decs with
        | [] -> ()
        | d::rest -> if List.contains d rest then failwith "Multiple variable have the same name" else tcFDecs rest
    and tcReturnStm (stm:Stm) =
        match stm with
        | Return(_) -> true
        | Do(GC(b)) -> tcReturnGC b
        | Alt(GC(b)) -> tcReturnGC b
        | Block(_,stms) -> List.exists tcReturnStm stms
        | _ -> false
    and tcReturnGC (l) =
        match l with
        | [] -> false
        | ((_,s)::rest) -> List.exists tcReturnStm s || tcReturnGC (rest)
    /// tcP prog checks the well-typeness of a program prog
    and tcP (P(decs, stms)) =
        let gtenv = tcGDecs Map.empty decs
        List.iter (tcStm gtenv Map.empty) stms