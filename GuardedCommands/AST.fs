namespace GuardedCommands.Frontend
// Michael R. Hansen 05-01-2016
// This file is obtained by an adaption of the file MicroC/Absyn.fs by Peter Sestoft
//
// It must preceed TypeChecks.fs, CodeGen.fs, CodeGenOpt.fs, Util.fs in Solution Explorer
//

open System
open Machine

module AST =

    type Expr =
        | N of int (* Integer constant            *)
        | B of bool (* Boolean constant            *)
        | Access of Access (* x    or  ^p    or  a[e]     *)
        | Addr of Access (* &x   or  &p^   or  &a[e]    *)
        | Apply of string * Expr list (* Function application        *)

    and Access =
        | AVar of string (* Variable access        x    *)
        | AIndex of Access * Expr (* Array indexing         a[e] *)
        | ADeref of Expr (* Pointer dereferencing  p^   *)

    type Stm =
        | PrintLn of Expr (* Print                          *)
        | Ass of Access * Expr (* x:=e  or  p^:=e  or  a[e]:=e   *)
        | Return of Expr option (* Return from function           *)
        | Alt of GuardedCommand (* Alternative statement          *)
        | Do of GuardedCommand (* Repetition statement           *)
        | Block of Dec list * Stm list (* Block: grouping and scope      *)
        | Call of string * Expr list (* Procedure call                 *)

    and GuardedCommand = GC of (Expr * Stm list) list

    and Dec =
        | VarDec of Type * string (* Variable declaration               *)
        | FunDec of Type option * string * Dec list * Stm

    (* Function and procedure declaration *)

    and Type =
        | IType (* Type int                    *)
        | BType (* Type bool                   *)
        | AType of Type * int option (* Type array                  *)
        | PType of Type (* Type pointer                *)
        | FType of Type list * Type option (* Type function and procedure *)

    type Program = P of Dec list * Stm list (* Program                 *)
