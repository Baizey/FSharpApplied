namespace Project1

open AndrewKennedyTree

module AST1 = 
  // Michael R. Hansen 06-01-2018
  // This file is obtained by an adaption of the file MicroC/Absyn.fs by Peter Sestoft
  //
  type Exp =
           | N  of int                   (* Integer constant            *)
           | B of bool                   (* Boolean constant            *)
           | Access of Access            (* x    or  ^p    or  a[e]     *)
           | Addr of Access              (* &x   or  &p^   or  &a[e]    *)
           | Apply of string * Exp list  (* Function application        *)

  and Access = 
            | AVar of string             (* Variable access        x    *) 
            | AIndex of Access * Exp     (* Array indexing         a[e] *)
            | ADeref of Exp              (* Pointer dereferencing  p^   *)

  type Stm  =                            
            | PrintLn of Exp               (* Print                          *) 
            | Ass of Access * Exp          (* x:=e  or  p^:=e  or  a[e]:=e   *)
            | Return of Exp option         (* Return from function           *)   
            | Alt of GuardedCommand        (* Alternative statement          *) 
            | Do of GuardedCommand         (* Repetition statement           *) 
            | Block of Dec list * Stm list (* Block: grouping and scope      *)
            | Call of string * Exp list    (* Procedure call                 *)
               
  and GuardedCommand = GC of (Exp * Stm list) list (* Guarded commands    *)

  and Dec = 
           | VarDec of Typ * string        (* Variable declaration               *)
           | FunDec of Typ option * string * Dec list * Stm
                                           (* Function and procedure declaration *) 

  and Typ  = 
           | ITyp                          (* Type int                    *)
           | BTyp                          (* Type bool                   *)
           | ATyp of Typ * int option      (* Type array                  *)
           | PTyp of Typ                   (* Type pointer                *)
           | FTyp of Typ list * Typ option (* Type function and procedure *)

  type Program = P of Dec list * Stm list   (* Program                 *)


  let convertToNodes (P(decl, stml): Program) =
    let rec convertType (typ: Typ) =
      match typ with
      | ITyp -> Node("ITyp", [])
      | BTyp -> Node("BTyp", [])
      | ATyp(innerType, size) -> Node("ATyp", [convertType innerType; Node(string size, [])])
      | PTyp(innerType) -> Node("PTyp", [convertType innerType])
      | FTyp(typl, typ') -> Node("Ftyp", List.map (fun x -> convertType x) typl @ [Node(string typ', [])])

    let rec convertExp (exp: Exp) =
      match exp with
      | N(i) -> Node("N", [Node(string i, [])])
      | B(b) -> Node("B", [Node(string b, [])])
      | Access(acc) -> Node("Access", [convertAccess acc])
      | Addr(acc) -> Node("Addr", [convertAccess acc])
      | Apply(str, expl) -> Node("Apply", [Node(str, []); convertExpList expl])
    and convertExpList (expl: Exp list) = Node("ExpList", List.map (fun x -> convertExp x) expl)

    and convertAccess (acc: Access) =
      match acc with
      | AVar(str) -> Node("AVar", [Node(str, [])])
      | AIndex(acc', exp) -> Node("AIndex", [convertAccess acc'; convertExp exp])
      | ADeref(exp) -> Node("ADeref", [convertExp exp])

    let rec convertStm (stm: Stm) = 
      match stm with
      | PrintLn(exp) -> Node("PrintLn", [convertExp exp])
      | Ass(acc, exp) -> Node("Ass", [convertAccess acc; convertExp exp])
      | Return(expOpt) -> 
        match expOpt with
        | Some(exp) -> Node("Return", [convertExp exp])
        | None -> Node("Return", [])
      | Alt(gc) -> Node("Alt", [convertGc gc])
      | Do(gc) -> Node("Do", [convertGc gc])
      | Block(decl, stml) -> Node("Block", [convertDecList decl; convertStmList stml])
      | Call(str, expl) -> Node("Call " + str, [convertExpList expl])
    and convertStmList (stml: Stm list) = Node("StmList", List.map (fun x -> convertStm x) stml)

    and convertGc (GC(gc): GuardedCommand) =
      let convertGcElem ((exp, stml): (Exp * Stm list)) =
        Node("GcElem", [convertExp exp; convertStmList stml])
      Node("Gc", List.map (fun x -> convertGcElem x) gc)

    and convertDec (dec: Dec) = 
      match dec with 
      | VarDec(typ, x) -> Node("VarDec", [convertType typ; Node(x, [])])
      | FunDec(typOpt, str, decl, stm) -> 
        match typOpt with
        | Some(typ) -> Node("FunDec", [convertType typ; Node(str, []); convertDecList decl; convertStm stm])
        | None -> Node("FunDec", [Node(str, []); convertDecList decl; convertStm stm])
    and convertDecList (decl: Dec list) = Node("DecList", List.map (fun x -> convertDec x) decl)

    // | FunDec of Typ option * string * Dec list * Stm


    Node("Program", [convertDecList decl; convertStmList stml])
