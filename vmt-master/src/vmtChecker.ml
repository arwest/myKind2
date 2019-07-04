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
    | MissingIdentifer of Position.t * string
    | MissingTerm of Position.t * string
    | NonMatchingTypes of Position.t * string * string

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
        | "Bool" -> None
        | "Int" -> None
        | "Real" -> None
        | _ -> Some ( InvalidType (pos, str) )
    )
    | A.MultiSort (pos, str, sort_list) ->(
        match str with
        | "Bool" -> eval_sort_list sort_list
        | "Int" -> eval_sort_list sort_list
        | "Real" -> eval_sort_list sort_list
        | _ -> Some ( InvalidType (pos, str) )
    )

and eval_sort_list sort_list =
    match sort_list with
    | [] -> None
    | sort :: t ->(
        match eval_sort sort with
        | None -> eval_sort_list t
        | error -> error
    )

let rec eval_sorted_var_list sorted_var_list local_env =
    match sorted_var_list with
    | [] -> local_env
    | sorted_var :: tail -> (
        match sorted_var with
        | A.SortedVar (pos, ident, sort) -> (
            match (eval_sort sort) with
            | Some error -> Error error
            | None -> (
                let local_env' = (ident, sort) :: local_env in
                eval_sorted_var_list tail local_env'
            )
        )
    )

let eval_term term env =
    match term with
    | A.Ident (pos, ident) -> (
        match filter_map (fun x -> if fst x = ident then Some (snd x) else None) env
        | [] -> Error (MissingIdentifer (pos, ident))
        | var_type :: _ -> Ok var_type
    )
    | A.Integer (pos, int) -> Ok "Int"
    | A.Real (pos, float) -> Ok "Real"
    | A.True (pos) -> Ok "Bool"
    | A.False (pos) -> Ok "Bool"
    | A.Operation (pos, op, term_list) -> (
        match (op, eval_term_list term_list env pos) with
        | _ -> Ok "Bool" (* TODO: write the logic for dealing with operators and the type given to them *)
    )
    | A.AttributeTerm (pos, term, attribute) -> (
        match (attribute, eval_term term env) with
        | _ -> Ok "Bool" (* TODO: write the logic for attributes and returning the type of term 
                            (if attribute is next verify the two vars have same type) *)
    )

and eval_term_list term_list env pos = 
    match term_list with
    | [] -> Error (MissingTerm pos)
    | term :: [] -> eval_term term env
    | term :: tail -> (
        let term_result = eval_term env in
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
        | (_, _, Some error) -> Error error
        | _ -> (ident, sort) :: env
    )
    | A.DefineFun (pos, ident, sorted_var_list, sort, term) -> (
        let id_exist = find_opt (fun x -> if fst x = ident then true else false) env in
        match (id_exist, eval_sorted_var_list sorted_var_list, eval_sort sort) with
        | (Some _, _, _) -> Error (IdentifierAlreadyExists (pos, ident))
        | (_, Error error, _) -> Error error
        | (_, _, Some error) -> Error error
        | (_, Ok local_env, _) -> (
            match eval_term term local_env with
            | Some 
        )
    )
    | A.DeclareSort (pos, ident, num) -> ()
    | A.DefineSort (pos, ident, ident_list, sort) -> ()
    | A.SetLogic (pos, ident) -> ()
    | A.SetOption (pos, ident, att) -> ()
    | A.Assert (pos, term) -> ()

let rec evaluate_expr_list expr_list env = 
    match expr_list with
    | [] -> Ok expr_list
    | expr :: t -> (
        let res = eval_expr env in
        match res with
        | Ok env' -> evaluate_expr_list t env'
        | _ -> res
    )

let check_vmt (expr_list : A.t ) : (A.t, vmt_error) result = 
    evaluate_expr_list expr_list []