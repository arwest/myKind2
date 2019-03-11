(* Copyright (c) 2019 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)

(** @author Andrew West *)

module A = NuxmvAst

type semantic_error_type =
    | LtlUse
    | NextExpr
    | DoubleNextExpr
    | RangeLowerValue

type 'a check_result = 
    | CheckOk
    | CheckError of Position.t * 'a

let empty_pos = Position.create_empty_position

let rec s_eval_expr (ltl: bool) (next:bool) (expr: A.nuxmv_expr) : semantic_error_type check_result = 
    match expr with
    | True _ -> CheckOk
    | False _ -> CheckOk
    | CInt _ -> CheckOk
    | CFloat _ -> CheckOk
    | Ident _ -> CheckOk
    | CRange (p, i1, i2) -> if i1 <= i2 then CheckOk else CheckError (p, RangeLowerValue)
    | A.Call (_, ci, nel) -> CheckOk (* TODO: check the next_expr list and the complex identifier *)
    | A.Not (_, e) -> s_eval_expr ltl next e
    | A.And (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1
    | A.Or (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1
    | A.Xor (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1
    | A.Xnor (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1   
    | A.Impl (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1
    | A.Equiv (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Eq (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.NotEq (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Lt (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Lte (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1
    | A.Gt (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Gte (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Plus (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Uminus (_, e) -> s_eval_expr ltl next e
    | A.Minus (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Multiply (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.Divide (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1
    | A.Mod (_, e1, e2) -> let r1 = s_eval_expr ltl next e1 in 
                             if r1 == CheckOk then s_eval_expr ltl next e2
                             else r1 
    | A.SetExp (_, el) -> let result_list = List.map (s_eval_expr ltl false) el in
                            if List.for_all (fun x -> x == CheckOk) result_list then CheckOk
                            else List.find (fun x -> x != CheckOk) result_list
    | A.CaseExp (_, el) -> let el1 = List.map (fun x -> match x with (x1, _) -> x1) el in
                           let el2 = List.map (fun x -> match x with (_, x2) -> x2) el in
                           let result_list1 = List.map (s_eval_expr ltl false) el1 in
                            if List.for_all (fun x -> x == CheckOk) result_list1 then CheckOk
                            else let result_list2 = List.map (s_eval_expr ltl false) el2 in
                                  if List.for_all (fun x -> x == CheckOk) result_list2 then CheckOk
                                  else List.find (fun x -> x != CheckOk) result_list2
    | A.NextState (p, e) -> if ltl == false then CheckError (p, LtlUse)
                            else s_eval_expr ltl next e
    | A.Globally (p, e) -> if ltl == false then CheckError (p, LtlUse)
                           else s_eval_expr ltl next e
    | A.Finally (p, e) -> if ltl == false then CheckError (p, LtlUse)
                          else s_eval_expr ltl next e
    | A.Until (p, e1, e2) -> if ltl == false then CheckError (p, LtlUse)
                             else 
                                let r1 = s_eval_expr ltl next e1 in 
                                if r1 == CheckOk then s_eval_expr ltl next e2
                                else r1 
    | A.Releases (p, e1, e2) -> if ltl == false then CheckError (p, LtlUse)
                                else 
                                    let r1 = s_eval_expr ltl next e1 in 
                                    if r1 == CheckOk then s_eval_expr ltl next e2
                                    else r1 
    | A.PrevState (p, e) -> if ltl == false then CheckError (p, LtlUse)
                            else s_eval_expr ltl next e
    | A.NotPrevStateNot (p, e) -> if ltl == false then CheckError (p, LtlUse)
                                  else s_eval_expr ltl next e
    | A.Historically (p, e) -> if ltl == false then CheckError (p, LtlUse)
                               else s_eval_expr ltl next e
    | A.Once (p, e) -> if ltl == false then CheckError (p, LtlUse)
                       else s_eval_expr ltl next e
    | A.Since (p, e1, e2) -> if ltl == false then CheckError (p, LtlUse)
                             else 
                                let r1 = s_eval_expr ltl next e1 in 
                                 if r1 == CheckOk then s_eval_expr ltl next e2
                                 else r1 
    | A.Triggered (p, e1, e2) -> if ltl == false then CheckError (p, LtlUse)
                                 else 
                                    let r1 = s_eval_expr ltl next e1 in 
                                     if r1 == CheckOk then s_eval_expr ltl next e2
                                     else r1
    | A.NextExp (p, e) -> if next == false then CheckError (p, NextExpr)
                          else 
                            match e with 
                            | A.NextExp _ -> CheckError (p, DoubleNextExpr)
                            | _ -> s_eval_expr ltl next e

let rec s_eval_expr_type (expr_type: A.expr_type) : semantic_error_type check_result = 
    match expr_type with
    | A.LtlExpr (_, expr) -> CheckOk (* Check that there is no next () *)
    | A.NextExpr (_, expr) -> CheckOk (* Check that there is no next ( next (.)) and no ltl*)
    | A.SimpleExpr (_, expr) -> CheckOk (* Check that there is no next() expr and no ltl *)
    | A.ArrayExpr (_, etl) -> let result_list = List.map s_eval_expr_type etl in
                                if List.for_all (fun x -> x == CheckOk) result_list then CheckOk
                                else List.find (fun x -> x != CheckOk) result_list

let s_eval_module_element (me : A.module_element): semantic_error_type check_result = 
    match me with
    | A.StateVarDecl (_, svdl) -> CheckOk (* Check moduleType specifier param list for expressions to be simple *)
    | A.DefineDecl (_, del) -> CheckOk (* Check define element expression *)
    | A.AssignConst (_, acl) -> CheckOk (* Check expr to make sure they are simple, expcept next is next *)
    | A.TransConst (_, expr_type) -> s_eval_expr_type expr_type
    | A.LtlSpec (_, expr_type) -> s_eval_expr_type expr_type

let rec semantic_eval (ml:A.nuxmv_module list) : semantic_error_type check_result = 
    match ml with
    | [] -> CheckOk
    | A.CustomModule (_, _, mel) :: [] -> let result_list = List.map s_eval_module_element mel in
                                          if List.for_all (fun x -> x == CheckOk) result_list then CheckOk
                                          else List.find (fun x -> x != CheckOk) result_list
    | A.CustomModule (_, _, mel) :: t -> let result_list = List.map s_eval_module_element mel in
                                          if List.for_all (fun x -> x == CheckOk) result_list then semantic_eval t
                                          else List.find (fun x -> x != CheckOk) result_list