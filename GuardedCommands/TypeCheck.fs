namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016 , 04-01-2018

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck =

    /// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
    /// for global and local variables
    let rec tcExpr gtenv ltenv =
        function
        | N _ -> ITyp
        | B _ -> BTyp
        | Access acc -> tcA gtenv ltenv acc

        | Apply(f, [ e ]) when List.exists (fun x -> x = f) [ "-"; "!" ] -> tcMonadic gtenv ltenv f e

        | Apply(f, [ e1; e2 ]) when List.exists (fun x -> x = f) [ "+"; "-"; "*"; "="; "&&" ] ->
            tcDyadic gtenv ltenv f e1 e2

        | _ -> failwith "tcE: not supported yet"

    and tcMonadic gtenv ltenv f e =
        match (f, tcExpr gtenv ltenv e) with
        | ("-", ITyp) -> ITyp
        | ("!", BTyp) -> BTyp
        | _ -> failwith "illegal/illtyped monadic expression"

    and tcDyadic gtenv ltenv f e1 e2 =
        match (f, tcExpr gtenv ltenv e1, tcExpr gtenv ltenv e2) with
        | (o, ITyp, ITyp) when List.exists (fun x -> x = o) [ "+"; "*"; "-" ] -> ITyp
        | (o, ITyp, ITyp) when List.exists (fun x -> x = o) [ "=" ] -> BTyp
        | (o, BTyp, BTyp) when List.exists (fun x -> x = o) [ "&&"; "=" ] -> BTyp
        | _ -> failwith ("illegal/illtyped dyadic expression: " + f)

    and tcNaryFunction gtenv ltenv f es = failwith "type check: functions not supported yet"

    and tcNaryProcedure gtenv ltenv f es = failwith "type check: procedures not supported yet"


    /// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
    /// for global and local variables
    and tcA gtenv ltenv =
        function
        | AVar x ->
            match Map.tryFind x ltenv with
            | None ->
                match Map.tryFind x gtenv with
                | None -> failwith ("no declaration for : " + x)
                | Some t -> t
            | Some t -> t
        | AIndex(acc, e) -> failwith "tcA: array indexing not supported yes"
        | ADeref e -> failwith "tcA: pointer dereferencing not supported yes"


    /// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
    /// for global and local variables and the possible type of return expressions
    and tcStm gtenv ltenv =
        function
        | PrintLn e -> ignore (tcExpr gtenv ltenv e)
        | Ass(acc, e) ->
            if tcA gtenv ltenv acc = tcExpr gtenv ltenv e then ()
            else failwith "illtyped assignment"
        | Do(GC(l))
        | Alt(GC(l)) -> match (List.forall (fun (e,_) -> (tcExpr gtenv ltenv e = BTyp)) l) with
                                         | true -> List.iter (fun (_,stms) -> List.iter (tcStm gtenv ltenv) stms) l
                                         | false -> failwith "Guard is not of type boolean"
        | Block([], stms) -> List.iter (tcStm gtenv ltenv) stms
        | _ -> failwith "tcS: this statement is not supported yet"

    and tcGDec gtenv =
        function
        | VarDec(typ, str) -> Map.add str typ gtenv
        | MulVarDec(typ, strList) -> strList |> List.map (fun str -> tcGDec gtenv (VarDec(typ, str))) |> List.head
        | FunDec(topt, f, decs, stm) -> failwith "type check: function/procedure declarations not yet supported"

    and tcGDecs gtenv =
        function
        | dec :: decs -> tcGDecs (tcGDec gtenv dec) decs
        | _ -> gtenv


    /// tcP prog checks the well-typeness of a program prog
    and tcP (P(decs, stms)) =
        let gtenv = tcGDecs Map.empty decs
        List.iter (tcStm gtenv Map.empty) stms