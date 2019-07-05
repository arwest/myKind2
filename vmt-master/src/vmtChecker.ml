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
module A = VmtAst

type vmt_error = 
    | IdentifierAlreadyExists of Position.t * string
    | InvalidOeprator of Position.t * string
    | InvalidType of Position.t * string
    | InvalidTypeWithOperator of Position.t * string * string
    | MissingIdentifer of Position.t * string
    | MissingTerm of Position.t 
    | NonMatchingTypes of Position.t * string * string
    | NotSupported of Position.t * string
    | InvalidArgCount of Position.t * int * int

let filter_map (f : ('a -> 'b option)) (l : 'a list) : 'b list =
    l |> List.map f 
      |> List.filter (fun x -> match x with None -> false | _ -> true)
      |> List.map (fun x -> match x with Some v -> v | _ -> assert false)
    
let find_opt (func : ('a -> bool)) (lst: 'a list) : 'a option =
    try let ans = List.find func lst in Some ans
    with Not_found -> None

let rec eval_sort sort =
    match sort with
    | A.Sort (pos, str) ->(
        match str with
        | "Bool" -> Ok "Bool"
        | "Int" -> Ok "Bool"
        | "Real" -> Ok "Bool"
        | _ -> Error ( InvalidType (pos, str) )
    )
    | A.MultiSort (pos, str, sort_list) -> Error (NotSupported (pos, "MutliSort")) (* TODO: Not currently supported becasue don't know how to handle it *)

and eval_sort_list sort_list =
    match sort_list with
    | [] -> None
    | sort :: t ->(
        match eval_sort sort with
        | Ok rt -> eval_sort_list t
        | Error error -> Some error
    )

let rec eval_sorted_var_list sorted_var_list local_env =
    match sorted_var_list with
    | [] -> Ok local_env
    | sorted_var :: tail -> (
        match sorted_var with
        | A.SortedVar (pos, ident, sort) -> (
            match (eval_sort sort) with
            | Error error -> Error error
            | Ok _type -> (
                let local_env' = (ident, _type) :: local_env in
                eval_sorted_var_list tail local_env'
            )
        )
    )

let rec eval_term term env =
    match term with
    | A.Ident (pos, ident) -> (
        match filter_map (fun x -> if fst x = ident then Some (snd x) else None) env with
        | [] -> Error (MissingIdentifer (pos, ident))
        | var_type :: _ -> Ok var_type
    )
    | A.Integer (pos, int) -> Ok "Int"
    | A.Real (pos, float) -> Ok "Real"
    | A.True (pos) -> Ok "Bool"
    | A.False (pos) -> Ok "Bool"
    | A.Operation (pos, op, term_list) -> eval_operation pos op term_list env
    | A.AttributeTerm (pos, term, attribute) -> (
        match (attribute, eval_term term env) with
        | (NextName (n_pos, n_ident), term_res)-> (
            match filter_map (fun x -> if fst x = n_ident then Some (snd x) else None) env with
            | [] -> Error (MissingIdentifer (pos, n_ident))
            | n_type :: _ -> (
                match term_res with
                | Ok term_type -> if n_type = term_type then Ok n_type else Error (InvalidType (n_pos, n_type))
                | error -> error
            )
        )
        | (InitTrue p, term_res) -> term_res
        | (TransTrue p, term_res) -> term_res
        | (InvarProperty (p, int), term_res) -> term_res
        | (LiveProperty (p, int), term_res) -> Error (NotSupported (p, "LiveProperty"))
    )

and eval_operation pos op term_list env = 
    match (op, eval_term_list term_list env pos) with
    | ("not", Ok "Bool") -> (
        let len = List.length term_list in
        if len > 1 
            then Error (InvalidArgCount (pos, 1, len))
            else Ok "Bool"
    )
    | ("or", Ok "Bool") -> Ok "Bool"
    | ("and", Ok "Bool") -> Ok "Bool"
    | ("xor", Ok "Bool") -> Ok "Bool"
    | ("=", Ok _type) when _type = "Bool" -> Ok _type
    | ("=", Ok _type) when _type = "Int" -> Ok "Bool"
    | ("<=", Ok _type) when _type = "Int" -> Ok "Bool"
    | ("<=", Ok _type) when _type = "Real" -> Ok "Bool"
    | ("<", Ok _type) when _type = "Int" -> Ok "Bool"
    | ("<", Ok _type) when _type = "Real" -> Ok "Bool"
    | (">=", Ok _type) when _type = "Int" -> Ok "Bool"
    | (">=", Ok _type) when _type = "Real" -> Ok "Bool"
    | (">", Ok _type) when _type = "Int" -> Ok "Bool"
    | (">", Ok _type) when _type = "Real" -> Ok "Bool"
    | ("-", Ok _type) when _type = "Int" -> Ok _type 
    | ("-", Ok _type) when _type = "Real" -> Ok _type 
    | ("+", Ok _type) when _type = "Int" -> Ok _type 
    | ("+", Ok _type) when _type = "Real" -> Ok _type 
    | ("*", Ok _type) when _type = "Int" -> Ok _type 
    | ("*", Ok _type) when _type = "Real" -> Ok _type 
    | ("/", Ok _type) when _type = "Int" -> Ok "Real"
    | ("/", Ok _type) when _type = "Real" -> Ok _type 
    | ("//", Ok _type) when _type = "Int" -> Ok _type 
    | ("//", Ok _type) when _type = "Real" -> Ok "Int" 
    | ("mod", Ok _type) when _type = "Int" -> (
        let len = List.length term_list in
        if len <> 2 
            then Error (InvalidArgCount (pos, 2, len))
            else Ok "Int"
    )
    | ("abs", Ok _type) -> (
        let len = List.length term_list in
        if len <> 2 
            then Error (InvalidArgCount (pos, 2, len))
            else 
                match _type with
                | "Int" -> Ok _type
                | "Real" -> Ok _type
                | _ -> Error (InvalidTypeWithOperator (pos, _type, "abs"))
    )
    | (op, Ok _type) -> Error (InvalidTypeWithOperator (pos, _type, op))
    | (_, Error error) -> Error error (* TODO: write the logic for dealing with operators and the type given to them *)


and eval_term_list term_list env pos = 
    match term_list with
    | [] -> Error (MissingTerm pos)
    | term :: [] -> eval_term term env
    | term :: tail -> (
        let term_result = eval_term term env in
        let tail_result = eval_term_list tail env pos in
        match (term_result, tail_result) with
        | (Error error, _) -> Error error
        | (_, Error error) -> Error error
        | (Ok term_type, Ok list_type) -> (
            if term_type = list_type 
                then Ok term_type
                else Error (NonMatchingTypes (pos, term_type, list_type))
        )
    )

let eval_expr expr env =
    match expr with
    | A.DeclareFun (pos, ident, sort_list, sort) -> (
        let id_exist = find_opt (fun x -> if fst x = ident then true else false) env in
        match (id_exist, eval_sort_list sort_list, eval_sort sort) with
        | (Some _, _, _) -> Error (IdentifierAlreadyExists (pos, ident))
        | (_, Some error, _) -> Error error
        | (_, _, Error error) -> Error error
        | (_, _, Ok return_type ) -> Ok ((ident, return_type) :: env)
    )
    | A.DefineFun (pos, ident, sorted_var_list, sort, term) -> (
        let id_exist = find_opt (fun x -> if fst x = ident then true else false) env in
        match (id_exist, eval_sorted_var_list sorted_var_list env, eval_sort sort) with
        | (Some _, _, _) -> Error (IdentifierAlreadyExists (pos, ident))
        | (_, Error error, _) -> Error error
        | (_, _, Error error) -> Error error
        | (_, Ok local_env, Ok return_type) -> (
            match eval_term term local_env with
            | Ok return_type -> Ok ((ident, return_type) :: env)
            | Error error -> Error error
        )
    )
    (* TODO: determine what we need to check or if we even support declare sort, define sort, set logic, and set option*)
    | A.DeclareSort (pos, ident, num) -> Ok env 
    | A.DefineSort (pos, ident, ident_list, sort) -> Ok env
    | A.SetLogic (pos, ident) -> Ok env
    | A.SetOption (pos, ident, att) -> Ok env
    | A.Assert (pos, term) -> (
        match eval_term term env with
        | Ok "Bool" -> Ok env
        | Ok wrong_type -> Error (InvalidType (pos, wrong_type))
        | Error error -> Error error
    )

let rec evaluate_expr_list expr_list env = 
    match expr_list with
    | [] -> Ok expr_list
    | expr :: t -> (
        let res = eval_expr expr env in
        match res with
        | Ok env' -> evaluate_expr_list t env'
        | Error error -> Error error
    )

let check_vmt (expr_list : A.t ) : (A.t, vmt_error) result = 
    evaluate_expr_list expr_list []