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
    | LtlUse of Position.t
    | NextExpr of Position.t
    | DoubleNextExpr of Position.t
    | RangeLowerValue of Position.t

type nuxmv_ast_type = 
    | IntT
    | SymbolicT
    | FloatT
    | RangeT
    | EnumT of (string * nuxmv_ast_type) list
    | ArrayT of nuxmv_ast_type list
    | BoolT
    | SetT of nuxmv_ast_type list
    | CaseT of nuxmv_ast_type list
    (* | FunT of nuxmv_ast_type list * nuxmv_ast_type *)
    (* | ModuleT of (string * nuxmv_ast_type) list *)

type type_error =
    | Expected of Position.t * nuxmv_ast_type list * nuxmv_ast_type
    | NonMatching of Position.t * nuxmv_ast_type * nuxmv_ast_type
    | MissingVariable of Position.t * string
    | AssignType of Position.t * nuxmv_ast_type * nuxmv_ast_type
    | EnumValueExist of Position.t * string
    | EnumNotContain of Position.t * string

type env = (string * nuxmv_ast_type) list

type 'a check_result = 
    | CheckOk
    | CheckError of 'a

(* SEMANTIC CHECKER *)

let rec s_eval_expr (ltl: bool) (next:bool) (expr: A.nuxmv_expr) : semantic_error_type check_result = 
    match expr with
    | True _ -> CheckOk
    | False _ -> CheckOk
    | CInt _ -> CheckOk
    | CFloat _ -> CheckOk
    | Ident _ -> CheckOk
    | CRange (p, i1, i2) -> if i1 <= i2 then CheckOk else CheckError (RangeLowerValue (p))
    | A.Call (_, ci, nel) -> (
        let result1 = s_eval_complex_id ci in
        match result1 with
        | CheckError _ -> result1
        | CheckOk -> 
            let mapped2 = List.map (s_eval_expr ltl next) nel in
            let result2 = List.find_opt (fun x -> x != CheckOk) mapped2 in
            match result2 with
            | Some res -> res
            | None -> CheckOk (* How to deal with function call return types *) 
                            )
    | A.Not (_, e) -> s_eval_expr ltl next e
    | A.And (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Or (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Xor (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Xnor (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)   
    | A.Impl (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Equiv (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1) 
    | A.Eq (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.NotEq (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Lt (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Lte (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Gt (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Gte (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Plus (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Uminus (_, e) -> s_eval_expr ltl next e
    | A.Minus (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Multiply (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Divide (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.Mod (_, e1, e2) -> (
        match s_eval_expr ltl next e1 with
        | CheckOk -> s_eval_expr ltl next e2
        | r1 -> r1)
    | A.SetExp (p, el) ->( 
        if ltl then CheckError (LtlUse (p))
        else
            let mapped = List.map (s_eval_expr ltl false) el in
            let result = List.find_opt (fun x -> x != CheckOk) mapped in
            match result with
            | Some res -> res
            | None -> CheckOk )
    | A.CaseExp (p, el) -> (
        if ltl then CheckError (LtlUse (p))
        else
            let mapped1 = List.map (s_eval_expr ltl next) (List.map fst el) in
            let result1 = List.find_opt (fun x -> x != CheckOk) mapped1 in
            match result1 with
            | Some res -> res
            | None -> 
                let mapped2 = List.map (s_eval_expr ltl next) (List.map snd el) in
                let result2 = List.find_opt (fun x -> x != CheckOk) mapped2 in
                match result2 with
                | Some res -> res
                | None -> CheckOk )
    | A.NextExp (p, e) -> (
        if not next then CheckError (NextExpr (p))
        else 
            (match e with 
            | A.NextExp _ -> CheckError (DoubleNextExpr (p))
            | _ -> s_eval_expr ltl next e))
    | A.NextState (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Globally (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Finally (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Until (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)
    | A.Releases (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)
    | A.PrevState (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.NotPrevStateNot (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Historically (p, e) -> 
        if not ltl then CheckError (LtlUse (p))
        else s_eval_expr ltl next e
    | A.Once (p, e) -> if not ltl then CheckError (LtlUse (p))
                       else s_eval_expr ltl next e
    | A.Since (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)
    | A.Triggered (p, e1, e2) -> (
        if not ltl then CheckError (LtlUse (p))
        else 
            match s_eval_expr ltl next e1 with
            | CheckOk -> s_eval_expr ltl next e2
            | r1 -> r1)

and s_eval_expr_type (expr_type: A.expr_type) : semantic_error_type check_result = 
    match expr_type with
    | A.LtlExpr (_, expr) -> s_eval_expr true false expr
    | A.NextExpr (_, expr) -> s_eval_expr false true expr
    | A.SimpleExpr (_, expr) -> s_eval_expr false false expr
    | A.ArrayExpr (_, etl) -> 
        let result_list = List.map s_eval_expr_type etl in
        match List.find_opt (fun x -> x != CheckOk) result_list with
        | None -> CheckOk
        | Some e -> e

and s_eval_complex_id (ci: A.comp_ident):  semantic_error_type check_result =
    match ci with
    | CIdent _ -> CheckOk

let rec s_eval_state_var_decl (svdl: A.state_var_decl list) :  semantic_error_type check_result =
    match svdl with
    | [] -> CheckOk
    | svd :: t -> 
        match svd with
        | ModuleType (p, i, mts) -> (
            match mts with 
            | ModuleTypeSpecifier (pos, id, etl) -> (
                let mapped = List.map s_eval_expr_type etl in
                let result = List.find_opt (fun x -> x != CheckOk) mapped in
                match result with
                | Some res -> res
                | None -> s_eval_state_var_decl t) )
        | _ -> s_eval_state_var_decl t

let rec s_eval_define_decl (del: A.define_element list):  semantic_error_type check_result =
    match del with
    | [] -> CheckOk
    | svd :: t -> 
        match svd with
        | SimpleDef (pos, id, et) -> (
            match s_eval_expr_type et with
            | CheckOk -> s_eval_define_decl t
            | other -> other)
        | ArrayDef (pos, id, et) -> (
            match s_eval_expr_type et with
            | CheckOk -> s_eval_define_decl t
            | other -> other)

let rec s_eval_assign_const (acl: A.assign_const list):  semantic_error_type check_result =
    match acl with
    | [] -> CheckOk
    | svd :: t -> match svd with
        | InitAssign (pos, ci, et) -> (
            match s_eval_complex_id ci with
            | CheckOk -> s_eval_expr_type et
            | other -> other)
        | NextAssign (pos, ci, et) -> (
            match s_eval_complex_id ci with
            | CheckOk -> s_eval_expr_type et
            | other -> other)
        | Assign (pos, ci, et) -> (
            match s_eval_complex_id ci with
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
    | A.CustomModule (_, _, mel) :: t -> 
        (let mapped = List.map s_eval_module_element mel in
        let result = List.find_opt (fun x -> x != CheckOk) mapped in
        match result with
        | Some res -> res
        | None -> semantic_eval t)

(* TYPE CHECKER *)
let enum_contains_value (enum_type_list: (string * nuxmv_ast_type) list ) (id : string) (pos : Position.t) : (bool, type_error) result = 
    let check_list = fun x -> match x with | (s,t) -> if s = id then true else false in
    match List.exists check_list enum_type_list with
    | true -> Ok true
    | false -> Error (EnumNotContain (pos, id))

let rec t_eval_expr (env : env) (expr: A.nuxmv_expr)  : (nuxmv_ast_type, type_error) result = 
    match expr with
    | True _ -> Ok BoolT
    | False _ -> Ok BoolT
    | CInt _ -> Ok IntT
    | CFloat _ -> Ok FloatT
    | Ident (p, id) -> (
        let res = List.find_opt (fun x -> match x with | (s,t) when s = id -> true | _ -> false) env in 
        match res with
        | Some (s,t) -> Ok t 
        | None -> Error (MissingVariable (p, id)))
    | CRange (p, i1, i2) -> Ok RangeT
    | Call (p, ci, nel) -> Ok BoolT (* TODO: figure out how to deal with the call *)
    | Not (p, e) ->( 
        match t_eval_expr env e with
        | Ok t when t = BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.And (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Or (p, e1, e2) ->  (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Xor (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Xnor (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e) 
    | A.Impl (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Equiv (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Eq (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok (EnumT enum_list), _ ) -> (
            match (e2, t_eval_expr enum_list e2) with
            | (A.Ident (pos, id), _) -> (
                match enum_contains_value enum_list id pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (A.CInt (pos, i), _) -> (
                match enum_contains_value enum_list (string_of_int i) pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (_, Ok t) -> Error (NonMatching (p, EnumT [], t))
            | (_, Error e) -> Error e)
        | ( _ , Ok (EnumT enum_list) ) -> (
            match (e1, t_eval_expr enum_list e1) with
            | (A.Ident (pos, id), _) -> (
                match enum_contains_value enum_list id pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (A.CInt (pos, i), _) -> (
                match enum_contains_value enum_list (string_of_int i) pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (_, Ok t) -> Error (NonMatching (p, EnumT [], t))
            | (_, Error e) -> Error e)
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT; BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.NotEq (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok (EnumT enum_list), _ ) -> (
            match (e2, t_eval_expr enum_list e2) with
            | (A.Ident (pos, id), _) -> (
                match enum_contains_value enum_list id pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (A.CInt (pos, i), _) -> (
                match enum_contains_value enum_list (string_of_int i) pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (_, Ok t) -> Error (NonMatching (p, EnumT [], t))
            | (_, Error e) -> Error e)
        | ( _ , Ok (EnumT enum_list) ) -> (
            match (e1, t_eval_expr enum_list e1) with
            | (A.Ident (pos, id), _) -> (
                match enum_contains_value enum_list id pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (A.CInt (pos, i), _) -> (
                match enum_contains_value enum_list (string_of_int i) pos with 
                | Ok _ -> Ok BoolT
                | Error e -> Error e)
            | (_, Ok t) -> Error (NonMatching (p, EnumT [], t))
            | (_, Error e) -> Error e)
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT; BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Lt (p, e1, e2) ->  ( 
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Lte (p, e1, e2) -> ( 
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Gt (p, e1, e2) ->  ( 
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Gte (p, e1, e2) -> ( 
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok BoolT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Plus (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Uminus (p, e) -> (
        match t_eval_expr env e with
        | Ok t when t = IntT -> Ok IntT
        | Ok t -> Error (Expected (p, [IntT], t))
        | err -> err)
    | A.Minus (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Multiply (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Divide (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | FloatT -> Ok FloatT
            | _ -> Error (Expected (p, [IntT; FloatT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Mod (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | IntT -> Ok IntT
            | _ -> Error (Expected (p, [IntT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.SetExp (p, el) ->  (
        let mapped = List.map (t_eval_expr env) el in
        let result = List.find_opt (fun x -> match x with | Ok _ -> false | _ -> true) mapped in
        match result with
        | Some e -> e
        | None -> Ok (SetT (List.map (fun x -> match x with Ok t -> t | _ -> assert false) mapped)))
    | A.CaseExp (p, el) -> (
        let mapped1 = List.map (t_eval_expr env) (List.map fst el) in
        let result1 = 
            List.find_opt (fun x -> match x with | Ok BoolT -> false | _ -> true) mapped1 in
        match result1 with
        | Some res -> res
        | None -> 
            let mapped2 = List.map (t_eval_expr env) (List.map snd el) in
            let result2 = 
            List.find_opt (fun x -> match x with | Ok _ -> false | _ -> true) mapped2 in
            let result3 = 
            List.find_opt (fun x -> match x with | Ok (SetT _) -> true | _ -> false) mapped2 in
            match (result2, result3) with
            | (Some res, _ ) -> res
            | (_, Some _) -> 
                Ok (CaseT (List.map 
                            (fun x -> match x with Ok (SetT t) -> SetT t | Ok t -> SetT [t] | _ -> assert false) 
                            mapped2))
            | (_, _ ) -> Ok (CaseT (List.map (fun x -> match x with Ok t -> t | _ -> assert false) mapped2 )))
    | A.NextExp (p, e) -> (
        match t_eval_expr env e with
        | Ok t -> Ok t
        | e -> e)
    | A.NextState (p, e) ->  (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.Globally (p, e) ->  (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.Finally (p, e) -> (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.Until (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Releases (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.PrevState (p, e) ->  (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.NotPrevStateNot (p, e) ->  (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.Historically (p, e) ->  (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.Once (p, e) -> (
        match t_eval_expr env e with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.Since (p, e1, e2) ->  (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)
    | A.Triggered (p, e1, e2) -> (
        match (t_eval_expr env e1, t_eval_expr env e2) with
        | (Ok t1, Ok t2) when t1 = t2 -> (
            match t1 with
            | BoolT -> Ok BoolT
            | _ -> Error (Expected (p, [BoolT], t1)))
        | (Ok t1, Ok t2) -> Error (NonMatching (p, t1, t2))
        | (Error e , _ ) -> Error e
        | ( _ , Error e) -> Error e)

let rec t_eval_expr_type (env : env) (expr_type: A.expr_type) : (nuxmv_ast_type, type_error) result  = 
    match expr_type with
    | A.LtlExpr (p, expr) -> (
        match t_eval_expr env expr with
        | Ok BoolT -> Ok BoolT
        | Ok t -> Error (Expected (p, [BoolT], t))
        | e -> e)
    | A.NextExpr (_, expr) -> (
        match t_eval_expr env expr with
        | Ok t -> Ok t
        | e -> e)
    | A.SimpleExpr (_, expr) ->(
        match t_eval_expr env expr with
        | Ok t -> Ok t
        | e -> e)
    | A.ArrayExpr (_, etl) -> (
        let result_list = List.map (t_eval_expr_type env) etl in
        match List.find_opt (fun x -> match x with | Ok _ -> false | Error _ -> true) result_list with
        | Some e -> e
        | None -> Ok (ArrayT (List.map (fun x -> match x with Ok t -> t | _ -> assert false) result_list)))

let rec t_eval_complex_id (env : env) (ci: A.comp_ident):  ((nuxmv_ast_type, type_error) result  * env) =
    match ci with
    | CIdent (pos, id) -> (
        match List.find_opt (fun x -> match x with (s,t) when s = id -> true | _ -> false) env with
        | Some (s,t) -> (Ok t, env)
        | None -> (Error (MissingVariable (pos, id)), []))

let rec t_eval_enum_var_decl (etvl: A.enum_type_value list) : (((string * nuxmv_ast_type) list, type_error) result)=
    let exist = fun y x -> match x with (s,t) -> if s = y then true else false in
    match etvl with
    | [] -> Ok []
    (* Do not add the enum values to the overall namespace *)
    | etv :: tail ->(
        match etv with 
        | A.ETId (pos, id) -> (
            let res = t_eval_enum_var_decl tail in
            match res with
             Ok lst -> 
                if List.exists (exist id) lst 
                then Error (EnumValueExist (pos, id))
                else Ok ((id, SymbolicT) :: lst)
            | error -> error)
        | A.ETCInt (pos, v) -> (
            let res = t_eval_enum_var_decl tail in
            match res with
             Ok lst -> 
                if List.exists (exist (string_of_int v) ) lst 
                then Error (EnumValueExist (pos, string_of_int v ))
                else Ok ((string_of_int v, IntT) :: lst)
            | error -> error))

let rec t_eval_state_var_decl (env : env) (svdl: A.state_var_decl list) : (env, type_error) result =
    match svdl with
    | [] -> Ok env 
    | svd :: t  -> 
        (match svd with (* TODO: Move all identations over for better readabiity and lessen it *)
        | ModuleType (p, i, mts) -> 
            (match mts with 
                | ModuleTypeSpecifier (pos, id, etl) -> 
                let mapped = List.map (t_eval_expr_type env) etl in
                let result = List.find_opt (fun x -> match x with Ok _ -> false | Error _ -> true) mapped in
                match result with
                | Some Error e -> Error e
                | None -> Ok env (* TODO: How to do module calling, skipping this for now *) 
                | _ -> assert false)
        | SimpleType (p, i, sts) -> 
            (match sts with
                (* TODO: Check that the identifier isn't being used already *)
                | Bool _ -> 
                    let env' = (i, BoolT) :: env in t_eval_state_var_decl env' t
                | Int _ -> 
                    let env' = (i, IntT) :: env in t_eval_state_var_decl env' t
                | Real _ -> 
                    let env' = (i, FloatT) :: env in t_eval_state_var_decl env' t
                | IntRange _ -> 
                    let env' = (i, RangeT) :: env in t_eval_state_var_decl env' t
                (* Figure out what the problem with checking the id's in the declaration and calling the top level id ... *)
                | EnumType (p, etvl) -> ( 
                    let enumLst = t_eval_enum_var_decl etvl in
                    match enumLst with
                    | Ok lst -> let env' = (i, EnumT lst) :: env in t_eval_state_var_decl env' t
                    | Error e -> Error e)
            )
        )

let lookup_type (env: env) (pos: Position.t) (id: string) : (nuxmv_ast_type, type_error) result =
    let res = List.find_opt (fun x -> match x with (s,t) when s = id -> true | _ -> false) env in
    match res with
    | Some (s,t) -> Ok (t) 
    | None -> Error (MissingVariable (pos, id))

let rec check_enum_for_value (lst: (string * nuxmv_ast_type) list ) (id : string) : nuxmv_ast_type option = 
    match lst with
    | [] -> None
    | (s,t) :: tail when s = id -> Some t
    | h :: tail -> check_enum_for_value tail id

let rec get_expr_from_expr_type (et: A.expr_type) : A.nuxmv_expr =
    match et with
    | A.LtlExpr (p, expr) -> expr
    | A.NextExpr (_, expr) -> expr
    | A.SimpleExpr (_, expr) -> expr
    | A.ArrayExpr (_, etl) -> get_expr_from_expr_type (List.hd etl)

let rec t_eval_define_decl (env : env) (del: A.define_element list):  (env, type_error) result =
    match del with
    | [] -> Ok env
    | svd :: tail -> 
        match svd with
        | SimpleDef (pos, id, et) -> (
            match t_eval_expr_type env et with
            | Ok t-> t_eval_define_decl env tail
            | Error e -> Error e)
        | ArrayDef (pos, id, et) -> (
            match t_eval_expr_type env et with
            | Ok t-> t_eval_define_decl env tail
            | Error e -> Error e)

let rec t_eval_assign_const (env : env) (acl: A.assign_const list): (env, type_error) result =
    match acl with
    | [] -> Ok env
    | svd :: t -> 
        match svd with
        | InitAssign (pos, ci, et) -> (
            match (t_eval_complex_id env ci, t_eval_expr_type env et) with
            | ((Ok t1, _), Ok t2) when t1 = t2 -> Ok env
            | ((Ok t1, _), Ok t2) -> Error (AssignType (pos, t1, t2))
            | ((Error e, _), _ ) -> Error e
            | (_ , Error e) -> Error e)
        | NextAssign (pos, ci, et) -> (
            match (t_eval_complex_id env ci, t_eval_expr_type env et) with
            | ((Ok t1, _), Ok t2) when t1 = t2 -> Ok env
            | ((Ok t1, _), Ok t2) -> Error (AssignType (pos, t1, t2))
            | ((Error e, _), _ ) -> Error e
            | (_ , Error e) -> Error e)
        | Assign (pos, ci, et) -> (
            match (t_eval_complex_id env ci, t_eval_expr_type env et) with
            | ((Ok t1, _), Ok t2) when t1 = t2 -> Ok env
            | ((Ok t1, _), Ok t2) -> Error (AssignType (pos, t1, t2))
            | ((Error e, _), _ ) -> Error e
            | (_ , Error e) -> Error e)

let t_eval_module_element (env : env) (me : A.module_element): (env, type_error) result = 
    match me with
    | A.StateVarDecl (_, svdl) -> t_eval_state_var_decl env svdl
    | A.DefineDecl (_, del) -> t_eval_define_decl env del
    | A.AssignConst (_, acl) -> t_eval_assign_const env acl
    | A.TransConst (_, expr_type) -> (
        match t_eval_expr_type env expr_type with
        | Ok _ -> Ok env
        | Error e -> Error e)
    | A.LtlSpec (_, expr_type) -> (
        match t_eval_expr_type env expr_type with
        | Ok _ -> Ok env
        | Error e -> Error e)

let rec type_eval_module_element_list (env: env) (mel: A.module_element list) : (env, type_error) result = 
    match mel with
    | [] -> Ok env
    | me :: t -> 
        match t_eval_module_element env me with
        | Ok env' -> type_eval_module_element_list env' t
        | error -> error

let rec type_eval_rec (env : env) (ml:A.nuxmv_module list) : (env, type_error) result = 
    match ml with
    | [] -> Ok env
    | A.CustomModule (_, _, mel) :: t -> 
        match type_eval_module_element_list env mel with
        | Ok env' -> type_eval_rec env' t
        | error -> error
let type_eval (ml : A.nuxmv_module list) : type_error check_result = 
    match type_eval_rec [] ml with
    | Ok _ -> CheckOk
    | Error e -> CheckError e
