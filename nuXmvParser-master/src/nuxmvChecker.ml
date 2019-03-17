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
    | A.And (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Or (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Xor (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Xnor (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)   
    | A.Impl (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Equiv (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1) 
    | A.Eq (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.NotEq (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Lt (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Lte (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Gt (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Gte (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Plus (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.Uminus (_, e) -> s_eval_expr ltl next e
    | A.Minus (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                             | CheckOk -> s_eval_expr ltl next e2
                             | r1 -> r1)
    | A.Multiply (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.Divide (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                              | CheckOk -> s_eval_expr ltl next e2
                              | r1 -> r1)
    | A.Mod (_, e1, e2) -> (match s_eval_expr ltl next e1 with
                            | CheckOk -> s_eval_expr ltl next e2
                            | r1 -> r1)
    | A.SetExp (p, el) ->( if ltl then CheckError (p, LtlUse)
                           else
                            let mapped = List.map (s_eval_expr ltl false) el in
                            let result = List.find_opt (fun x -> x != CheckOk) mapped in
                            match result with
                            | Some res -> res
                            | None -> CheckOk )
    | A.CaseExp (p, el) -> (if ltl then CheckError (p, LtlUse)
                            else
                            let mapped1 = List.map (s_eval_expr ltl false) (List.map fst el) in
                            let result1 = List.find_opt (fun x -> x != CheckOk) mapped1 in
                            match result1 with
                            | Some res -> res
                            | None -> let mapped2 = List.map (s_eval_expr ltl false) (List.map snd el) in
                                      let result2 = List.find_opt (fun x -> x != CheckOk) mapped2 in
                                      match result2 with
                                      | Some res -> res
                                      | None -> CheckOk )
    | A.NextExp (p, e) -> if not next then CheckError (p, NextExpr)
                          else 
                            (match e with 
                             | A.NextExp _ -> CheckError (p, DoubleNextExpr)
                             | _ -> s_eval_expr ltl next e)
    | A.NextState (p, e) -> if not ltl then CheckError (p, LtlUse)
                            else s_eval_expr ltl next e
    | A.Globally (p, e) -> if not ltl then CheckError (p, LtlUse)
                           else s_eval_expr ltl next e 
    | A.Finally (p, e) -> if not ltl  then CheckError (p, LtlUse)
                          else s_eval_expr ltl next e
    | A.Until (p, e1, e2) -> (if not ltl then CheckError (p, LtlUse)
                              else 
                                match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.Releases (p, e1, e2) -> (if not ltl then CheckError (p, LtlUse)
                                else 
                                match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.PrevState (p, e) -> if not ltl then CheckError (p, LtlUse)
                            else s_eval_expr ltl next e
    | A.NotPrevStateNot (p, e) -> if not ltl then CheckError (p, LtlUse)
                                  else s_eval_expr ltl next e
    | A.Historically (p, e) ->  if not ltl then CheckError (p, LtlUse)
                                else s_eval_expr ltl next e
    | A.Once (p, e) -> if not ltl then CheckError (p, LtlUse)
                       else s_eval_expr ltl next e
    | A.Since (p, e1, e2) -> if not ltl then CheckError (p, LtlUse)
                             else
                                (match s_eval_expr ltl next e1 with
                                | CheckOk -> s_eval_expr ltl next e2
                                | r1 -> r1)
    | A.Triggered (p, e1, e2) -> (if not ltl then CheckError (p, LtlUse)
                                  else 
                                 match s_eval_expr ltl next e1 with
                                 | CheckOk -> s_eval_expr ltl next e2
                                 | r1 -> r1)

let rec s_eval_expr_type (expr_type: A.expr_type) : semantic_error_type check_result = 
    match expr_type with
    | A.LtlExpr (_, expr) -> s_eval_expr true false expr
    | A.NextExpr (_, expr) -> s_eval_expr false true expr
    | A.SimpleExpr (_, expr) -> s_eval_expr false false expr
    | A.ArrayExpr (_, etl) -> let result_list = List.map s_eval_expr_type etl in
                              if List.for_all (fun x -> x == CheckOk) result_list then CheckOk
                              else List.find (fun x -> x != CheckOk) result_list

let rec s_eval_complex_id (ci: A.comp_ident):  semantic_error_type check_result =
    match ci with
    | PerIdent (pos, ci, i) -> s_eval_complex_id ci
    | BrackIdent (pos, ci, et) -> (match s_eval_complex_id ci with
                                  | CheckOk -> s_eval_expr_type et
                                  | other -> other)
    | _ -> CheckOk

let rec s_eval_state_var_decl (svdl: A.state_var_decl list) :  semantic_error_type check_result =
    match svdl with
    | [] -> CheckOk
    | svd :: t -> match svd with
                  | ModuleType (p, i, mts) -> 
                      (match mts with 
                          | ModuleTypeSpecifier (pos, id, etl) -> 
                          let mapped = List.map s_eval_expr_type etl in
                          let result = List.find_opt (fun x -> x != CheckOk) mapped in
                          match result with
                          | Some res -> res
                          | None -> s_eval_state_var_decl t )
                  | _ -> s_eval_state_var_decl t

let rec s_eval_define_decl (del: A.define_element list):  semantic_error_type check_result =
    match del with
    | [] -> CheckOk
    | svd :: t -> match svd with
                  | SimpleDef (pos, id, et) -> (match s_eval_expr_type et with
                                               | CheckOk -> s_eval_define_decl t
                                               | other -> other)
                  | ArrayDef (pos, id, et) -> (match s_eval_expr_type et with
                                               | CheckOk -> s_eval_define_decl t
                                               | other -> other)

let rec s_eval_assign_const (acl: A.assign_const list):  semantic_error_type check_result =
    match acl with
    | [] -> CheckOk
    | svd :: t -> match svd with
                  | InitAssign (pos, ci, et) -> (match s_eval_complex_id ci with
                                                | CheckOk -> s_eval_expr_type et
                                                | other -> other)
                  | NextAssign (pos, ci, et) -> (match s_eval_complex_id ci with
                                                | CheckOk -> s_eval_expr_type et
                                                | other -> other)
                  | Assign (pos, ci, et) -> (match s_eval_complex_id ci with
                                                | CheckOk -> s_eval_expr_type et
                                                | other -> other)

let s_eval_module_element (me : A.module_element): semantic_error_type check_result = 
    match me with
    | A.StateVarDecl (_, svdl) -> s_eval_state_var_decl svdl
    | A.DefineDecl (_, del) -> s_eval_define_decl del
    | A.AssignConst (_, acl) -> s_eval_assign_const acl
    | A.TransConst (_, expr_type) -> s_eval_expr_type expr_type
    | A.LtlSpec (_, expr_type) -> s_eval_expr_type expr_type

let rec semantic_eval (ml:A.nuxmv_module list) : semantic_error_type check_result = 
    match ml with
    | [] -> CheckOk
    | A.CustomModule (_, _, mel) :: [] -> 
            (let result_list = List.map s_eval_module_element mel in
            if List.for_all (fun x -> x == CheckOk) result_list then CheckOk
            else List.find (fun x -> x != CheckOk) result_list)
    | A.CustomModule (_, _, mel) :: t -> 
            (let result_list = List.map s_eval_module_element mel in
            if List.for_all (fun x -> x == CheckOk) result_list then semantic_eval t
            else List.find (fun x -> x != CheckOk) result_list)