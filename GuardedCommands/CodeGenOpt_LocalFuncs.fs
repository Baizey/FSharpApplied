﻿namespace GuardedCommands.Backend

open Machine

module CodeGenerationOptFuncs =
    (* Directly copied from Peter Sestoft   START
    Code-generating functions that perform local optimizations *)

     let rec addINCSP m1 C: instr list =
         match C with
         | INCSP m2 :: C1 -> addINCSP (m1 + m2) C1
         | RET m2 :: C1 -> RET(m2 - m1) :: C1
         | Label lab :: RET m2 :: _ -> RET(m2 - m1) :: C
         | _ ->
             if m1 = 0 then C
             else INCSP m1 :: C

     let addLabel C: label * instr list =
         match C with
         | Label lab :: _ -> (lab, C)
         | GOTO lab :: _ -> (lab, C)
         | _ ->
             let lab = newLabel()
             (lab, Label lab :: C)

     let makeJump C: instr * instr list =
         match C with
         | RET m :: _ -> (RET m, C)
         | Label lab :: RET m :: _ -> (RET m, C)
         | Label lab :: _ -> (GOTO lab, C)
         | GOTO lab :: _ -> (GOTO lab, C)
         | _ ->
             let lab = newLabel()
             (GOTO lab, Label lab :: C)

     let makeCall m lab C: instr list =
         match C with
         | RET n :: C1 -> TCALL(m, n, lab) :: C1
         | Label _ :: RET n :: _ -> TCALL(m, n, lab) :: C
         | _ -> CALL(m, lab) :: C

     let rec deadcode C =
         match C with
         | [] -> []
         | Label lab :: _ -> C
         | _ :: C1 -> deadcode C1

     let addNOT C =
         match C with
         | NOT :: C1 -> C1
         | IFZERO lab :: C1 -> IFNZRO lab :: C1
         | IFNZRO lab :: C1 -> IFZERO lab :: C1
         | _ -> NOT :: C

     let addJump jump C =
         let C1 = deadcode C
         match (jump, C1) with
         | (GOTO lab1, Label lab2 :: _) ->
             if lab1 = lab2 then C1
             else GOTO lab1 :: C1
         | _ -> jump :: C1

     let addGOTO lab C = addJump (GOTO lab) C

     let rec addCST i C =
         match (i, C) with
         | (0, ADD :: C1) -> C1
         | (0, SUB :: C1) -> C1
         | (0, NOT :: C1) -> addCST 1 C1
         | (_, NOT :: C1) -> addCST 0 C1
         | (1, MUL :: C1) -> C1
         | (1, DIV :: C1) -> C1
         | (0, EQ :: C1) -> addNOT C1
         | (_, INCSP m :: C1) ->
             if m < 0 then addINCSP (m + 1) C1
             else CSTI i :: C
         | (0, IFZERO lab :: C1) -> addGOTO lab C1
         | (_, IFZERO lab :: C1) -> C1
         | (0, IFNZRO lab :: C1) -> C1
         | (_, IFNZRO lab :: C1) -> addGOTO lab C1
         | _ -> CSTI i :: C

     (* ------------------------------------------------------------------- *)

     (* End code directly copied from Peter Sestoft *)